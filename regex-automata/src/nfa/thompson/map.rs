// This module contains a couple simple and purpose built hash maps. The key
// trade off they make is that they serve as caches rather than true maps. That
// is, inserting a new entry may cause eviction of another entry. This gives
// us two things. First, there's less overhead associated with inserts and
// lookups. Secondly, it lets us control our memory usage.
//
// These maps are used in some fairly hot code when generating NFA states for
// large Unicode character classes.
//
// Instead of exposing a rich hashmap entry API, we just permit the caller to
// produce a hash of the key directly. The hash can then be reused for both
// lookups and insertions at the cost of leaking abstraction a bit. But these
// are for internal use only, so it's fine.
//
// The Utf8BoundedMap is used for Daciuk's algorithm for constructing a
// (almost) minimal DFA for large Unicode character classes in linear time.
// (Daciuk's algorithm is always used when compiling forward NFAs. For reverse
// NFAs, it's only used when the compiler is configured to 'shrink' the NFA,
// since there's a bit more expense in the reverse direction.)
//
// The Utf8SuffixMap is used when compiling large Unicode character classes for
// reverse NFAs when 'shrink' is disabled. Specifically, it augments the naive
// construction of UTF-8 automata by caching common suffixes. This doesn't
// get the same space savings as Daciuk's algorithm, but it's basically as
// fast as the naive approach and typically winds up using less memory (since
// it generates smaller NFAs) despite the presence of the cache.
//
// These maps effectively represent caching mechanisms for sparse and
// byte-range NFA states, respectively. The former represents a single NFA
// state with many transitions of equivalent priority while the latter
// represents a single NFA state with a single transition. (Neither state ever
// has or is an epsilon transition.) Thus, they have different key types. It's
// likely we could make one generic map, but the machinery didn't seem worth
// it. They are simple enough.

use alloc::{vec, vec::Vec};

use crate::{
    nfa::thompson::Transition,
    util::{
        int::{Usize, U64},
        primitives::StateID,
    },
};

// Basic FNV-1a hash constants as described in:
// https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
const PRIME: u64 = 1099511628211;
const INIT: u64 = 14695981039346656037;

/// The number of hash buckets allocated together in a map chunk.
///
/// Keeping this reasonably small avoids initializing all (for example) 10,000
/// buckets for a small Unicode class, while amortizing allocation for larger
/// classes.
const UTF8_BOUND_MAP_CHUNK_LEN: usize = 64;

/// A bounded hash map where the key is a sequence of NFA transitions and the
/// value is a pre-existing NFA state ID.
///
/// std's hashmap can be used for this, however, this map has two important
/// advantages. Firstly, it has lower overhead. Secondly, it permits us to
/// control our memory usage by limited the number of slots. In general, the
/// cost here is that this map acts as a cache. That is, inserting a new entry
/// may remove an old entry. We are okay with this, since it does not impact
/// correctness in the cases where it is used. The only effect that dropping
/// states from the cache has is that the resulting NFA generated may be bigger
/// than it otherwise would be.
///
/// This improves benchmarks that compile large Unicode character classes,
/// since it makes the generation of (almost) minimal UTF-8 automaton faster.
/// Specifically, one could observe the difference with std's hashmap via
/// something like the following benchmark:
///
///   hyperfine "regex-cli debug thompson -qr --captures none '\w{90} ecurB'"
///
/// But to observe that difference, you'd have to modify the code to use
/// std's hashmap.
///
/// It is quite possible that there is a better way to approach this problem.
/// For example, if there happens to be a very common state that collides with
/// a lot of less frequent states, then we could wind up with very poor caching
/// behavior. Alas, the effectiveness of this cache has not been measured.
/// Instead, ad hoc experiments suggest that it is "good enough." Additional
/// smarts (such as an LRU eviction policy) have to be weighed against the
/// amount of extra time they cost.
///
/// This cache is stored on the compiler instead of in thread-local storage.
/// Thread-local storage could reuse it across compiler instances, but would
/// also retain a potentially large cache for every thread that compiles a
/// Unicode regex and would require a separate path for no-std builds anyway.
/// The chunked representation below makes first use cheap while keeping
/// ownership and reclamation tied to the compiler.
#[derive(Clone, Debug)]
pub struct Utf8BoundedMap {
    /// The current version of this map. Only entries with matching versions
    /// are considered during lookups. If an entry is found with a mismatched
    /// version, then the map behaves as if the entry does not exist.
    ///
    /// This makes it possible to clear the map by simply incrementing the
    /// version number instead of actually deallocating any storage.
    version: u16,
    /// The total number of entries this map can store.
    capacity: usize,
    /// The entries, keyed by hash and allocated in chunks. Collisions between
    /// different states result in the old state being dropped.
    map: Vec<Vec<Utf8BoundedEntry>>,
}

/// An entry in this map.
#[derive(Clone, Debug, Default)]
struct Utf8BoundedEntry {
    /// The version of the map used to produce this entry. If this entry's
    /// version does not match the current version of the map, then the map
    /// should behave as if this entry does not exist.
    version: u16,
    /// The key, which is a sorted sequence of non-overlapping NFA transitions.
    key: Vec<Transition>,
    /// The state ID corresponding to the state containing the transitions in
    /// this entry.
    val: StateID,
}

impl Utf8BoundedMap {
    /// Create a new bounded map with the given capacity. The map will never
    /// grow beyond the given size.
    ///
    /// Note that this does not allocate. Instead, callers must call `clear`
    /// before using this map. `clear` will allocate space if necessary.
    ///
    /// This avoids the need to pay for the allocation of this map when
    /// compiling regexes that lack large Unicode character classes.
    pub fn new(capacity: usize) -> Utf8BoundedMap {
        assert!(capacity > 0);
        Utf8BoundedMap { version: 0, capacity, map: vec![] }
    }

    /// Clear this map of all entries, but permit the reuse of allocation
    /// if possible.
    ///
    /// This must be called before the map can be used.
    pub fn clear(&mut self) {
        if self.map.is_empty() {
            let chunk_len =
                1 + ((self.capacity - 1) / UTF8_BOUND_MAP_CHUNK_LEN);
            self.map = vec![vec![]; chunk_len];
        } else {
            self.version = self.version.wrapping_add(1);
            // If we loop back to version 0, then we forcefully clear every
            // allocated chunk. Otherwise, it might be possible to incorrectly
            // match entries used to generate other NFAs.
            if self.version == 0 {
                for chunk in &mut self.map {
                    chunk.fill(Utf8BoundedEntry::default());
                }
            }
        }
    }

    /// Return a hash of the given transitions.
    pub fn hash(&self, key: &[Transition]) -> usize {
        let mut h = INIT;
        for t in key {
            h = (h ^ u64::from(t.start)).wrapping_mul(PRIME);
            h = (h ^ u64::from(t.end)).wrapping_mul(PRIME);
            h = (h ^ t.next.as_u64()).wrapping_mul(PRIME);
        }
        (h % self.capacity.as_u64()).as_usize()
    }

    /// Retrieve the cached state ID corresponding to the given key. The hash
    /// given must have been computed with `hash` using the same key value.
    ///
    /// If there is no cached state with the given transitions, then None is
    /// returned.
    pub fn get(&mut self, key: &[Transition], hash: usize) -> Option<StateID> {
        let chunk = &self.map[hash / UTF8_BOUND_MAP_CHUNK_LEN];
        if chunk.is_empty() {
            return None;
        }
        let entry = &chunk[hash % UTF8_BOUND_MAP_CHUNK_LEN];
        if entry.version != self.version {
            return None;
        }
        // There may be a hash collision, so we need to confirm real equality.
        if entry.key != key {
            return None;
        }
        Some(entry.val)
    }

    /// Add a cached state to this map with the given key. Callers should
    /// ensure that `state_id` points to a state that contains precisely the
    /// NFA transitions given.
    ///
    /// `hash` must have been computed using the `hash` method with the same
    /// key.
    pub fn set(
        &mut self,
        key: Vec<Transition>,
        hash: usize,
        state_id: StateID,
    ) {
        let chunk = hash / UTF8_BOUND_MAP_CHUNK_LEN;
        let offset = hash % UTF8_BOUND_MAP_CHUNK_LEN;
        // When we don't yet have a chunk for this particular key,
        // lazily allocate.
        if self.map[chunk].is_empty() {
            let start = chunk * UTF8_BOUND_MAP_CHUNK_LEN;
            let len = core::cmp::min(
                UTF8_BOUND_MAP_CHUNK_LEN,
                // Careful to avoid exceeding our configured capacity.
                // This can only be smaller than the chunk length
                // on the last chunk.
                self.capacity - start,
            );
            self.map[chunk] = vec![Utf8BoundedEntry::default(); len];
        }
        self.map[chunk][offset] =
            Utf8BoundedEntry { version: self.version, key, val: state_id };
    }
}

/// A cache of suffixes used to modestly compress UTF-8 automata for large
/// Unicode character classes.
#[derive(Clone, Debug)]
pub struct Utf8SuffixMap {
    /// The current version of this map. Only entries with matching versions
    /// are considered during lookups. If an entry is found with a mismatched
    /// version, then the map behaves as if the entry does not exist.
    version: u16,
    /// The total number of entries this map can store.
    capacity: usize,
    /// The actual entries, keyed by hash. Collisions between different states
    /// result in the old state being dropped.
    map: Vec<Utf8SuffixEntry>,
}

/// A key that uniquely identifies an NFA state. It is a triple that represents
/// a transition from one state for a particular byte range.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Utf8SuffixKey {
    pub from: StateID,
    pub start: u8,
    pub end: u8,
}

/// An entry in this map.
#[derive(Clone, Debug, Default)]
struct Utf8SuffixEntry {
    /// The version of the map used to produce this entry. If this entry's
    /// version does not match the current version of the map, then the map
    /// should behave as if this entry does not exist.
    version: u16,
    /// The key, which consists of a transition in a particular state.
    key: Utf8SuffixKey,
    /// The identifier that the transition in the key maps to.
    val: StateID,
}

impl Utf8SuffixMap {
    /// Create a new bounded map with the given capacity. The map will never
    /// grow beyond the given size.
    ///
    /// Note that this does not allocate. Instead, callers must call `clear`
    /// before using this map. `clear` will allocate space if necessary.
    ///
    /// This avoids the need to pay for the allocation of this map when
    /// compiling regexes that lack large Unicode character classes.
    pub fn new(capacity: usize) -> Utf8SuffixMap {
        assert!(capacity > 0);
        Utf8SuffixMap { version: 0, capacity, map: vec![] }
    }

    /// Clear this map of all entries, but permit the reuse of allocation
    /// if possible.
    ///
    /// This must be called before the map can be used.
    pub fn clear(&mut self) {
        if self.map.is_empty() {
            self.map = vec![Utf8SuffixEntry::default(); self.capacity];
        } else {
            self.version = self.version.wrapping_add(1);
            if self.version == 0 {
                self.map = vec![Utf8SuffixEntry::default(); self.capacity];
            }
        }
    }

    /// Return a hash of the given transition.
    pub fn hash(&self, key: &Utf8SuffixKey) -> usize {
        // Basic FNV-1a hash as described:
        // https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
        const PRIME: u64 = 1099511628211;
        const INIT: u64 = 14695981039346656037;

        let mut h = INIT;
        h = (h ^ key.from.as_u64()).wrapping_mul(PRIME);
        h = (h ^ u64::from(key.start)).wrapping_mul(PRIME);
        h = (h ^ u64::from(key.end)).wrapping_mul(PRIME);
        (h % self.map.len().as_u64()).as_usize()
    }

    /// Retrieve the cached state ID corresponding to the given key. The hash
    /// given must have been computed with `hash` using the same key value.
    ///
    /// If there is no cached state with the given key, then None is returned.
    pub fn get(
        &mut self,
        key: &Utf8SuffixKey,
        hash: usize,
    ) -> Option<StateID> {
        let entry = &self.map[hash];
        if entry.version != self.version {
            return None;
        }
        if key != &entry.key {
            return None;
        }
        Some(entry.val)
    }

    /// Add a cached state to this map with the given key. Callers should
    /// ensure that `state_id` points to a state that contains precisely the
    /// NFA transition given.
    ///
    /// `hash` must have been computed using the `hash` method with the same
    /// key.
    pub fn set(&mut self, key: Utf8SuffixKey, hash: usize, state_id: StateID) {
        self.map[hash] =
            Utf8SuffixEntry { version: self.version, key, val: state_id };
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn key(byte: u8) -> Vec<Transition> {
        vec![Transition {
            start: byte,
            end: byte,
            next: StateID::must(usize::from(byte) + 1),
        }]
    }

    fn key_with_hash(
        map: &Utf8BoundedMap,
        expected: usize,
    ) -> Vec<Transition> {
        for byte in 0..=u8::MAX {
            let key = key(byte);
            if map.hash(&key) == expected {
                return key;
            }
        }
        panic!("could not find key for hash bucket {expected}");
    }

    #[test]
    fn bounded_map_allocates_entries_lazily() {
        let mut map = Utf8BoundedMap::new(10_000);
        assert!(map.map.is_empty());

        map.clear();
        assert_eq!(
            map.map.len(),
            1 + ((10_000 - 1) / UTF8_BOUND_MAP_CHUNK_LEN),
        );
        assert!(map.map.iter().all(Vec::is_empty));

        let key = key(b'a');
        let hash = map.hash(&key);
        map.set(key.clone(), hash, StateID::must(1));
        assert_eq!(
            map.map.iter().filter(|chunk| !chunk.is_empty()).count(),
            1
        );
        let chunk = hash / UTF8_BOUND_MAP_CHUNK_LEN;
        assert_eq!(
            map.map[chunk].len(),
            core::cmp::min(
                UTF8_BOUND_MAP_CHUNK_LEN,
                map.capacity - (chunk * UTF8_BOUND_MAP_CHUNK_LEN),
            ),
        );
        assert_eq!(map.get(&key, hash), Some(StateID::must(1)));
    }

    #[test]
    fn bounded_map_replaces_collisions() {
        let mut map = Utf8BoundedMap::new(1);
        map.clear();
        let key1 = key(b'a');
        let key2 = key(b'b');
        let hash = map.hash(&key1);
        assert_eq!(hash, map.hash(&key2));

        map.set(key1.clone(), hash, StateID::must(1));
        map.set(key2.clone(), hash, StateID::must(2));
        assert_eq!(map.map[0].len(), 1);
        assert_eq!(map.get(&key1, hash), None);
        assert_eq!(map.get(&key2, hash), Some(StateID::must(2)));
    }

    #[test]
    fn bounded_map_clear_invalidates_entries() {
        let mut map = Utf8BoundedMap::new(2);
        map.clear();
        let key0 = key_with_hash(&map, 0);
        let key1 = key_with_hash(&map, 1);
        map.set(key0.clone(), 0, StateID::must(1));
        map.set(key1.clone(), 1, StateID::must(2));
        assert_eq!(map.map[0].len(), 2);

        map.clear();
        assert_eq!(map.get(&key0, 0), None);
        assert_eq!(map.get(&key1, 1), None);
        assert_eq!(map.map[0].len(), 2);

        map.set(key1.clone(), 1, StateID::must(3));
        assert_eq!(map.get(&key1, 1), Some(StateID::must(3)));
    }

    #[test]
    fn bounded_map_version_wrap_invalidates_entries() {
        let mut map = Utf8BoundedMap::new(1);
        map.clear();
        let key = key(b'a');
        let hash = map.hash(&key);
        map.set(key.clone(), hash, StateID::must(1));

        // Simulate enough clears to wrap the version back to the version used
        // by the existing entry. The wrap must forcefully reset that entry.
        map.version = u16::MAX;
        map.clear();
        assert_eq!(map.version, 0);
        assert_eq!(map.get(&key, hash), None);

        map.set(key.clone(), hash, StateID::must(2));
        assert_eq!(map.get(&key, hash), Some(StateID::must(2)));
    }
}
