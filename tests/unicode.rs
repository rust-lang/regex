mat!(uni_literal, u!(r"☃"), "☃", Some((0, 3)));
mat!(uni_one, u!(r"\pN"), "Ⅰ", Some((0, 3)));
mat!(uni_mixed, u!(r"\pN+"), "Ⅰ1Ⅱ2", Some((0, 8)));
mat!(uni_not, u!(r"\PN+"), "abⅠ", Some((0, 2)));
mat!(uni_not_class, u!(r"[\PN]+"), "abⅠ", Some((0, 2)));
mat!(uni_not_class_neg, u!(r"[^\PN]+"), "abⅠ", Some((2, 5)));
mat!(uni_case, u!(r"(?i)Δ"), "δ", Some((0, 2)));
mat!(uni_case_upper, u!(r"\p{Lu}+"), "ΛΘΓΔα", Some((0, 8)));
mat!(uni_case_upper_nocase_flag, u!(r"(?i)\p{Lu}+"), "ΛΘΓΔα", Some((0, 10)));
mat!(uni_case_upper_nocase, u!(r"\p{L}+"), "ΛΘΓΔα", Some((0, 10)));
mat!(uni_case_lower, u!(r"\p{Ll}+"), "ΛΘΓΔα", Some((8, 10)));

// Test the Unicode friendliness of Perl character classes.
mat!(uni_perl_w, u!(r"\w+"), "dδd", Some((0, 4)));
mat!(uni_perl_w_not, u!(r"\w+"), "⥡", None);
mat!(uni_perl_w_neg, u!(r"\W+"), "⥡", Some((0, 3)));
mat!(uni_perl_d, u!(r"\d+"), "1२३9", Some((0, 8)));
mat!(uni_perl_d_not, u!(r"\d+"), "Ⅱ", None);
mat!(uni_perl_d_neg, u!(r"\D+"), "Ⅱ", Some((0, 3)));
mat!(uni_perl_s, u!(r"\s+"), " ", Some((0, 3)));
mat!(uni_perl_s_not, u!(r"\s+"), "☃", None);
mat!(uni_perl_s_neg, u!(r"\S+"), "☃", Some((0, 3)));

// And do the same for word boundaries.
mat!(uni_boundary_none, u!(r"\d\b"), "6δ", None);
mat!(uni_boundary_ogham, u!(r"\d\b"), "6 ", Some((0, 1)));
mat!(uni_not_boundary_none, u!(r"\d\B"), "6δ", Some((0, 1)));
mat!(uni_not_boundary_ogham, u!(r"\d\B"), "6 ", None);
