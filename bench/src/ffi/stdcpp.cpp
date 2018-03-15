#include <regex>

extern "C" {
    typedef void stdcpp_regexp;

    typedef struct stdcpp_string {
        const char *text;
        int len;
    } stdcpp_string;

    stdcpp_regexp* stdcpp_regexp_new(stdcpp_string pat) {
        return reinterpret_cast<stdcpp_regexp*>(new std::regex(pat.text,
							       pat.len,
							       std::regex::optimize));
    }

    void stdcpp_regexp_free(stdcpp_regexp *re) {
        delete reinterpret_cast<std::regex*>(re);
    }

    bool stdcpp_regexp_match(stdcpp_regexp *re, stdcpp_string text,
			     int startpos, int endpos) {
	std::regex cpp_re(*reinterpret_cast<std::regex*>(re));
        return std::regex_search(text.text + startpos, text.text + endpos,
				 cpp_re);
    }

    bool stdcpp_regexp_find(stdcpp_regexp *re, stdcpp_string text,
			    int startpos, int endpos,
			    int *match_start, int *match_end) {
	std::regex cpp_re(*reinterpret_cast<std::regex*>(re));
	std::cmatch result;
        bool matched;
        matched = std::regex_search(text.text + startpos, text.text + endpos,
				    result, cpp_re);
        if (matched) {
	    *match_start = result[0].first - text.text;
	    *match_end = *match_start + result.length(0);
        }
        return matched;
    }
}
