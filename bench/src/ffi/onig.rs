use onig;

pub struct Regex(onig::Regex);

unsafe impl Send for Regex {}

impl Regex {
    pub fn new(pattern: &str) -> Result<Self, onig::Error> {
        onig::Regex::new(pattern).map(Regex)
    }

    pub fn is_match(&self, text: &str) -> bool {
        // Gah. onig's is_match function is anchored, but find is not.
        self.0
            .search_with_options(
                text,
                0,
                text.len(),
                onig::SearchOptions::SEARCH_OPTION_NONE,
                None,
            )
            .is_some()
    }

    pub fn find_iter<'r, 't>(
        &'r self,
        text: &'t str,
    ) -> onig::FindMatches<'r, 't> {
        self.0.find_iter(text)
    }
}
