use core::{char, cmp, fmt::Debug, mem};

use alloc::vec::Vec;

use crate::unicode;

// This module contains an *internal* implementation of interval sets.
//
// The primary invariant that interval sets guards is canonical ordering. That
// is, every interval set contains an ordered sequence of intervals where
// no two intervals are overlapping or adjacent. While this invariant is
// occasionally broken within the implementation, it should be impossible for
// callers to observe it.
//
// Since case folding (as implemented below) breaks that invariant, we roll
// that into this API even though it is a little out of place in an otherwise
// generic interval set. (Hence the reason why the `unicode` module is imported
// here.)
//
// Some of the implementation complexity here is a result of me wanting to
// preserve the sequential representation without using additional memory.
// In many cases, we do use linear extra memory, but it is at most 2x and it
// is amortized. If we relaxed the memory requirements, this implementation
// could become much simpler. The extra memory is honestly probably OK, but
// character classes (especially of the Unicode variety) can become quite
// large, and it would be nice to keep regex compilation snappy even in debug
// builds. (In the past, I have been careless with this area of code and it has
// caused slow regex compilations in debug mode, so this isn't entirely
// unwarranted.)
//
// Tests on this are relegated to the public API of HIR in src/hir.rs.

#[derive(Clone, Debug)]
pub struct IntervalSet<I> {
    /// A sorted set of non-overlapping ranges.
    ranges: Vec<I>,
    /// While not required at all for correctness, we keep track of whether an
    /// interval set has been case folded or not. This helps us avoid doing
    /// redundant work if, for example, a set has already been cased folded.
    /// And note that whether a set is folded or not is preserved through
    /// all of the pairwise set operations. That is, if both interval sets
    /// have been case folded, then any of difference, union, intersection or
    /// symmetric difference all produce a case folded set.
    ///
    /// Note that when this is true, it *must* be the case that the set is case
    /// folded. But when it's false, the set *may* be case folded. In other
    /// words, we only set this to true when we know it to be case, but we're
    /// okay with it being false if it would otherwise be costly to determine
    /// whether it should be true. This means code cannot assume that a false
    /// value necessarily indicates that the set is not case folded.
    ///
    /// Bottom line: this is a performance optimization.
    folded: bool,
}

impl<I: Interval> Eq for IntervalSet<I> {}

// We implement PartialEq manually so that we don't consider the set's internal
// 'folded' property to be part of its identity. The 'folded' property is
// strictly an optimization.
impl<I: Interval> PartialEq for IntervalSet<I> {
    fn eq(&self, other: &IntervalSet<I>) -> bool {
        self.ranges.eq(&other.ranges)
    }
}

impl<I: Interval> IntervalSet<I> {
    /// Create a new set from a sequence of intervals. Each interval is
    /// specified as a pair of bounds, where both bounds are inclusive.
    ///
    /// The given ranges do not need to be in any specific order, and ranges
    /// may overlap.
    pub fn new<T: IntoIterator<Item = I>>(intervals: T) -> IntervalSet<I> {
        let ranges: Vec<I> = intervals.into_iter().collect();
        // An empty set is case folded.
        let folded = ranges.is_empty();
        let mut set = IntervalSet { ranges, folded };
        set.canonicalize();
        set
    }

    /// Add a new interval to this set.
    pub fn push(&mut self, interval: I) {
        // TODO: This could be faster. e.g., Push the interval such that
        // it preserves canonicalization.
        self.ranges.push(interval);
        self.canonicalize();
        // We don't know whether the new interval added here is considered
        // case folded, so we conservatively assume that the entire set is
        // no longer case folded if it was previously.
        self.folded = false;
    }

    /// Return an immutable slice of intervals in this set.
    ///
    /// The sequence returned is in canonical ordering.
    pub fn intervals(&self) -> &[I] {
        &self.ranges
    }

    /// Expand this interval set such that it contains all case folded
    /// characters. For example, if this class consists of the range `a-z`,
    /// then applying case folding will result in the class containing both the
    /// ranges `a-z` and `A-Z`.
    ///
    /// This returns an error if the necessary case mapping data is not
    /// available.
    pub fn case_fold_simple(&mut self) -> Result<(), unicode::CaseFoldError> {
        if self.folded {
            return Ok(());
        }
        let len = self.ranges.len();
        for i in 0..len {
            let range = self.ranges[i];
            if let Err(err) = range.case_fold_simple(&mut self.ranges) {
                self.canonicalize();
                return Err(err);
            }
        }
        self.canonicalize();
        self.folded = true;
        Ok(())
    }

    /// Union this set with the given set, in place.
    pub fn union(&mut self, other: &IntervalSet<I>) {
        if other.ranges.is_empty() || self.ranges == other.ranges {
            return;
        }

        if self.ranges.is_empty() {
            self.clone_from(other);
            return;
        }

        // No way to know what the new size will be, so for now we assume that
        // in typical cases, the union of a set of classes won't have many
        // overlaps.
        let mut ranges =
            Vec::with_capacity(self.ranges.len() + other.ranges.len());

        let merged =
            MergeIter::new(self.ranges.iter(), other.ranges.iter()).copied();

        let final_range = merged.reduce(|range, next_range| {
            range.union_right(&next_range).unwrap_or_else(|| {
                ranges.push(range);
                next_range
            })
        });

        if let Some(final_range) = final_range {
            ranges.push(final_range);
        }

        self.ranges = ranges;
        self.folded = self.folded && other.folded;
    }

    /// Intersect this set with the given set, in place.
    pub fn intersect(&mut self, other: &IntervalSet<I>) {
        if self.ranges.is_empty() {
            return;
        }
        if other.ranges.is_empty() {
            self.ranges.clear();
            // An empty set is case folded.
            self.folded = true;
            return;
        }

        // There should be a way to do this in-place with constant memory,
        // but I couldn't figure out a simple way to do it. So just append
        // the intersection to the end of this range, and then drain it before
        // we're done.
        let drain_end = self.ranges.len();

        let mut ita = 0..drain_end;
        let mut itb = 0..other.ranges.len();
        let mut a = ita.next().unwrap();
        let mut b = itb.next().unwrap();
        loop {
            if let Some(ab) = self.ranges[a].intersect(&other.ranges[b]) {
                self.ranges.push(ab);
            }
            let (it, aorb) =
                if self.ranges[a].upper() < other.ranges[b].upper() {
                    (&mut ita, &mut a)
                } else {
                    (&mut itb, &mut b)
                };
            match it.next() {
                Some(v) => *aorb = v,
                None => break,
            }
        }
        self.ranges.drain(..drain_end);
        self.folded = self.folded && other.folded;
    }

    /// Subtract the given set from this set, in place.
    pub fn difference(&mut self, other: &IntervalSet<I>) {
        if self.ranges.is_empty() || other.ranges.is_empty() {
            return;
        }

        // This algorithm is (to me) surprisingly complex. A search of the
        // interwebs indicate that this is a potentially interesting problem.
        // Folks seem to suggest interval or segment trees, but I'd like to
        // avoid the overhead (both runtime and conceptual) of that.
        //
        // The following is basically my Shitty First Draft. Therefore, in
        // order to grok it, you probably need to read each line carefully.
        // Simplifications are most welcome!
        //
        // Remember, we can assume the canonical format invariant here, which
        // says that all ranges are sorted, not overlapping and not adjacent in
        // each class.
        let drain_end = self.ranges.len();
        let (mut a, mut b) = (0, 0);
        'LOOP: while a < drain_end && b < other.ranges.len() {
            // Basically, the easy cases are when neither range overlaps with
            // each other. If the `b` range is less than our current `a`
            // range, then we can skip it and move on.
            if other.ranges[b].upper() < self.ranges[a].lower() {
                b += 1;
                continue;
            }
            // ... similarly for the `a` range. If it's less than the smallest
            // `b` range, then we can add it as-is.
            if self.ranges[a].upper() < other.ranges[b].lower() {
                let range = self.ranges[a];
                self.ranges.push(range);
                a += 1;
                continue;
            }
            // Otherwise, we have overlapping ranges.
            assert!(!self.ranges[a].is_intersection_empty(&other.ranges[b]));

            // This part is tricky and was non-obvious to me without looking
            // at explicit examples (see the tests). The trickiness stems from
            // two things: 1) subtracting a range from another range could
            // yield two ranges and 2) after subtracting a range, it's possible
            // that future ranges can have an impact. The loop below advances
            // the `b` ranges until they can't possible impact the current
            // range.
            //
            // For example, if our `a` range is `a-t` and our next three `b`
            // ranges are `a-c`, `g-i`, `r-t` and `x-z`, then we need to apply
            // subtraction three times before moving on to the next `a` range.
            let mut range = self.ranges[a];
            while b < other.ranges.len()
                && !range.is_intersection_empty(&other.ranges[b])
            {
                let old_range = range;
                range = match range.difference(&other.ranges[b]) {
                    (None, None) => {
                        // We lost the entire range, so move on to the next
                        // without adding this one.
                        a += 1;
                        continue 'LOOP;
                    }
                    (Some(range1), None) | (None, Some(range1)) => range1,
                    (Some(range1), Some(range2)) => {
                        self.ranges.push(range1);
                        range2
                    }
                };
                // It's possible that the `b` range has more to contribute
                // here. In particular, if it is greater than the original
                // range, then it might impact the next `a` range *and* it
                // has impacted the current `a` range as much as possible,
                // so we can quit. We don't bump `b` so that the next `a`
                // range can apply it.
                if other.ranges[b].upper() > old_range.upper() {
                    break;
                }
                // Otherwise, the next `b` range might apply to the current
                // `a` range.
                b += 1;
            }
            self.ranges.push(range);
            a += 1;
        }
        while a < drain_end {
            let range = self.ranges[a];
            self.ranges.push(range);
            a += 1;
        }
        self.ranges.drain(..drain_end);
        self.folded = self.folded && other.folded;
    }

    /// Compute the symmetric difference of the two sets, in place.
    ///
    /// This computes the symmetric difference of two interval sets. This
    /// removes all elements in this set that are also in the given set,
    /// but also adds all elements from the given set that aren't in this
    /// set. That is, the set will contain all elements in either set,
    /// but will not contain any elements that are in both sets.
    pub fn symmetric_difference(&mut self, other: &IntervalSet<I>) {
        // TODO(burntsushi): Fix this so that it amortizes allocation.
        let mut intersection = self.clone();
        intersection.intersect(other);
        self.union(other);
        self.difference(&intersection);
    }

    /// Negate this interval set.
    ///
    /// For all `x` where `x` is any element, if `x` was in this set, then it
    /// will not be in this set after negation.
    pub fn negate(&mut self) {
        let Some(first_range) = self.ranges.first() else {
            let (min, max) = (I::Bound::MIN, I::Bound::MAX);
            self.ranges.push(I::create(min, max));
            // The set containing everything must case folded.
            self.folded = true;
            return;
        };

        // The basic algorithm: replace each interval `[low..high]` with
        // `[margin..low]``, and record `high` as the new `margin`. Do all of
        // that without making off-by-one errors, and take  care that there may
        // not be a new interval at [0..?] or at [?..max].

        // First, take care of the first range; if it's 0.., it has no leftward
        // negation, so it's skipped, and its upper bound is used as the first
        // leftward margin.
        let (margin, skip_first) = match first_range.lower() == I::Bound::MIN {
            false => (I::Bound::MIN, false),
            true => match first_range.upper().try_increment() {
                Some(bound) => (bound, true),
                // The current range covers everything, so its negation is the empty set
                None => {
                    self.ranges.clear();
                    self.folded = true;
                    return;
                }
            },
        };

        let mut left_margin = Some(margin);
        let mut left_margin_ref = left_margin.as_ref().unwrap();

        // Again, we're replacing each range with its leftward negation. If
        // skip_first is true, then the first range HAS no leftward negation,
        // and everything else is shifted to the left one slot.
        let start = if skip_first { 1 } else { 0 };

        for index in start..self.ranges.len() {
            let dest_index = if skip_first { index - 1 } else { index };

            let start = *left_margin_ref;
            let end = self.ranges[index].lower().decrement();
            left_margin = self.ranges[index].upper().try_increment();

            self.ranges[dest_index] = I::create(start, end);

            left_margin_ref = match left_margin.as_ref() {
                Some(margin) => margin,
                None => break,
            }
        }

        // If we skipped the first range, then all of the subsequent ranges
        // were stored one slot to the left. Pop the last slot.
        if skip_first {
            self.ranges.pop();
        }

        // If there's a final margin, we need to add an extra righward negation,
        // covering everything on that side.
        if let Some(left_margin) = left_margin {
            self.ranges.push(I::create(left_margin, I::Bound::MAX));
        }

        // We don't need to update whether this set is folded or not, because
        // it is conservatively preserved through negation. Namely, if a set
        // is not folded, then it is possible that its negation is folded, for
        // example, [^☃]. But we're fine with assuming that the set is not
        // folded in that case. (`folded` permits false negatives but not false
        // positives.)
        //
        // But what about when a set is folded, is its negation also
        // necessarily folded? Yes. Because if a set is folded, then for every
        // character in the set, it necessarily included its equivalence class
        // of case folded characters. Negating it in turn means that all
        // equivalence classes in the set are negated, and any equivalence
        // class that was previously not in the set is now entirely in the set.
    }

    /// Converts this set into a canonical ordering.
    fn canonicalize(&mut self) {
        if self.is_canonical() {
            return;
        }

        self.ranges.sort_unstable();
        assert!(!self.ranges.is_empty());

        // `merge_idx` is the range into which we're merging contiguous ranges.
        let mut merge_idx = 0;

        for i in 1..self.ranges.len() {
            if let Some(union) =
                self.ranges[merge_idx].union_right(&self.ranges[i])
            {
                self.ranges[merge_idx] = union;
            } else {
                merge_idx += 1;
                self.ranges[merge_idx] = self.ranges[i];
            }
        }

        // At this point, `merge_idx` is the index of the last range that was
        // merged into, so we truncate.
        self.ranges.truncate(merge_idx + 1);
    }

    /// Returns true if and only if this class is in a canonical ordering.
    fn is_canonical(&self) -> bool {
        for pair in self.ranges.windows(2) {
            if pair[0] >= pair[1] {
                return false;
            }
            if pair[0].is_contiguous(&pair[1]) {
                return false;
            }
        }
        true
    }
}

pub trait Interval:
    Clone + Copy + Debug + Default + Eq + PartialEq + PartialOrd + Ord
{
    type Bound: Bound;

    fn lower(&self) -> Self::Bound;
    fn upper(&self) -> Self::Bound;
    fn set_lower(&mut self, bound: Self::Bound);
    fn set_upper(&mut self, bound: Self::Bound);
    fn case_fold_simple(
        &self,
        intervals: &mut Vec<Self>,
    ) -> Result<(), unicode::CaseFoldError>;

    /// Create a new interval.
    fn create(lower: Self::Bound, upper: Self::Bound) -> Self {
        let mut int = Self::default();
        if lower <= upper {
            int.set_lower(lower);
            int.set_upper(upper);
        } else {
            int.set_lower(upper);
            int.set_upper(lower);
        }
        int
    }

    /// Union the given overlapping range into this range, assuming that
    /// self.begin <= right.begin. Useful for performing a series of unions
    /// on a sorted list of intervals.
    ///
    /// If the ranges aren't contiguous, this returns `None`.
    /// Returns unspecified garbage if self.begin > right.begin.
    fn union_right(&self, right: &Self) -> Option<Self> {
        if self.upper().as_u32().saturating_add(1) < right.lower().as_u32() {
            None
        } else {
            Some(Self::create(
                self.lower(),
                cmp::max(self.upper(), right.upper()),
            ))
        }
    }

    /// Intersect this range with the given range and return the result.
    ///
    /// If the intersection is empty, then this returns `None`.
    fn intersect(&self, other: &Self) -> Option<Self> {
        let lower = cmp::max(self.lower(), other.lower());
        let upper = cmp::min(self.upper(), other.upper());
        if lower <= upper {
            Some(Self::create(lower, upper))
        } else {
            None
        }
    }

    /// Subtract the given range from this range and return the resulting
    /// ranges.
    ///
    /// If subtraction would result in an empty range, then no ranges are
    /// returned.
    fn difference(&self, other: &Self) -> (Option<Self>, Option<Self>) {
        if self.is_subset(other) {
            return (None, None);
        }
        if self.is_intersection_empty(other) {
            return (Some(self.clone()), None);
        }
        let add_lower = other.lower() > self.lower();
        let add_upper = other.upper() < self.upper();
        // We know this because !self.is_subset(other) and the ranges have
        // a non-empty intersection.
        assert!(add_lower || add_upper);
        let mut ret = (None, None);
        if add_lower {
            let upper = other.lower().decrement();
            ret.0 = Some(Self::create(self.lower(), upper));
        }
        if add_upper {
            let lower = other.upper().increment();
            let range = Self::create(lower, self.upper());
            if ret.0.is_none() {
                ret.0 = Some(range);
            } else {
                ret.1 = Some(range);
            }
        }
        ret
    }

    /// Returns true if and only if the two ranges are contiguous. Two ranges
    /// are contiguous if and only if the ranges are either overlapping or
    /// adjacent.
    fn is_contiguous(&self, other: &Self) -> bool {
        let lower1 = self.lower().as_u32();
        let upper1 = self.upper().as_u32();
        let lower2 = other.lower().as_u32();
        let upper2 = other.upper().as_u32();
        cmp::max(lower1, lower2) <= cmp::min(upper1, upper2).saturating_add(1)
    }

    /// Returns true if and only if the intersection of this range and the
    /// other range is empty.
    fn is_intersection_empty(&self, other: &Self) -> bool {
        let (lower1, upper1) = (self.lower(), self.upper());
        let (lower2, upper2) = (other.lower(), other.upper());
        cmp::max(lower1, lower2) > cmp::min(upper1, upper2)
    }

    /// Returns true if and only if this range is a subset of the other range.
    fn is_subset(&self, other: &Self) -> bool {
        let (lower1, upper1) = (self.lower(), self.upper());
        let (lower2, upper2) = (other.lower(), other.upper());
        (lower2 <= lower1 && lower1 <= upper2)
            && (lower2 <= upper1 && upper1 <= upper2)
    }
}

pub trait Bound:
    Copy + Clone + Debug + Eq + PartialEq + PartialOrd + Ord
{
    const MIN: Self;
    const MAX: Self;

    fn as_u32(self) -> u32;
    fn try_increment(self) -> Option<Self>;
    fn try_decrement(self) -> Option<Self>;

    fn increment(self) -> Self {
        self.try_increment().unwrap()
    }

    fn decrement(self) -> Self {
        self.try_decrement().unwrap()
    }
}

impl Bound for u8 {
    const MIN: Self = u8::MIN;
    const MAX: Self = u8::MAX;

    fn as_u32(self) -> u32 {
        u32::from(self)
    }

    fn try_increment(self) -> Option<Self> {
        self.checked_add(1)
    }

    fn try_decrement(self) -> Option<Self> {
        self.checked_sub(1)
    }
}

impl Bound for char {
    const MIN: Self = '\x00';
    const MAX: Self = '\u{10FFFF}';

    fn as_u32(self) -> u32 {
        u32::from(self)
    }

    fn try_increment(self) -> Option<Self> {
        match self {
            '\u{D7FF}' => Some('\u{E000}'),
            c => char::from_u32(u32::from(c).checked_add(1)?),
        }
    }

    fn try_decrement(self) -> Option<Self> {
        match self {
            '\u{E000}' => Some('\u{D7FF}'),
            c => char::from_u32(u32::from(c).checked_sub(1)?),
        }
    }
}

struct MergeIter<I: Iterator> {
    left: I,
    right: I,

    state: MergeIterState<I::Item>,
}

impl<I> MergeIter<I>
where
    I: Iterator,
    I::Item: Ord,
{
    pub fn new(mut left: I, right: I) -> Self {
        let state = match left.next() {
            Some(item) => MergeIterState::LeftItem(item),
            None => MergeIterState::LeftExhausted,
        };

        Self { left, right, state }
    }
}

enum MergeIterState<T> {
    LeftExhausted,
    RightExhausted,
    LeftItem(T),
    RightItem(T),
}

impl<T> MergeIterState<T> {
    // Get the current state, and if it's an item, replace the state with
    // the appropriate exhaustion state for the other side.
    fn step(&mut self) -> Self {
        use MergeIterState::*;

        match *self {
            LeftItem(_) => mem::replace(self, RightExhausted),
            RightItem(_) => mem::replace(self, LeftExhausted),
            LeftExhausted => LeftExhausted,
            RightExhausted => RightExhausted,
        }
    }
}

impl<I> Iterator for MergeIter<I>
where
    I: Iterator,
    I::Item: Ord,
{
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        use MergeIterState::*;

        let (left, right) = match self.state.step() {
            LeftExhausted => return self.right.next(),
            RightExhausted => return self.left.next(),

            LeftItem(left) => match self.right.next() {
                Some(right) => (left, right),
                None => return Some(left),
            },
            RightItem(right) => match self.left.next() {
                Some(left) => (left, right),
                None => return Some(right),
            },
        };

        let (item, state) = match left <= right {
            true => (left, RightItem(right)),
            false => (right, LeftItem(left)),
        };

        self.state = state;
        Some(item)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        // Fundamentally this is a spicy concatenation, so add the sizes together
        let (min1, max1) = self.left.size_hint();
        let (min2, max2) = self.right.size_hint();

        let min = min1.saturating_add(min2);
        let max = max1.and_then(|max| max.checked_add(max2?));

        (min, max)
    }
}

// Tests for interval sets are written in src/hir.rs against the public API.
