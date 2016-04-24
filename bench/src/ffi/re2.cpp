#include <iostream>
#include <stdio.h>

#include "re2.h"

using namespace re2;

extern "C" {
    typedef void re2_regexp;

    typedef struct re2_string {
        const char *text;
        int len;
    } re2_string;

    re2_regexp* re2_regexp_new(re2_string pat) {
        re2::StringPiece re2_pat(pat.text, pat.len);
        return reinterpret_cast<re2_regexp*>(new RE2(re2_pat));
    }

    void re2_regexp_free(re2_regexp *re) {
        delete reinterpret_cast<RE2*>(re);
    }

    bool re2_regexp_match(re2_regexp *re, re2_string text,
                          int startpos, int endpos) {
        RE2 *cpp_re = reinterpret_cast<RE2*>(re);
        re2::StringPiece cpp_text(text.text, text.len);

        return cpp_re->Match(cpp_text, startpos, endpos, RE2::UNANCHORED,
                             NULL, 0);
    }

    bool re2_regexp_find(re2_regexp *re, re2_string text,
                         int startpos, int endpos,
                         int *match_start, int *match_end) {
        RE2 *cpp_re = reinterpret_cast<RE2*>(re);
        re2::StringPiece cpp_text(text.text, text.len);
        re2::StringPiece result;
        bool matched;

        matched = cpp_re->Match(cpp_text, startpos, endpos, RE2::UNANCHORED,
                                &result, 1);
        if (matched) {
            *match_start = result.data() - cpp_text.data();
            *match_end = *match_start + result.length();
        }
        return matched;
    }
}
