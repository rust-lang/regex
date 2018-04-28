#ifdef USE_BOOST
#include <boost/regex.hpp>
#else
#include <regex>
#endif

extern "C" {

#ifdef USE_BOOST
    namespace regex_ns = boost;
#else
    namespace regex_ns = std;
#endif

    typedef void stdcpp_regexp;

    typedef struct stdcpp_string {
        const char *text;
        int len;
    } stdcpp_string;

    stdcpp_regexp* stdcpp_regexp_new(stdcpp_string pat) {
	return reinterpret_cast<stdcpp_regexp*>(new regex_ns::regex(pat.text,
								    pat.len,
								    regex_ns::regex::optimize));
    }

    void stdcpp_regexp_free(stdcpp_regexp *re) {
	delete reinterpret_cast<regex_ns::regex*>(re);
    }

    bool stdcpp_regexp_match(stdcpp_regexp *re, stdcpp_string text,
			     int startpos, int endpos) {
	regex_ns::regex cpp_re(*reinterpret_cast<regex_ns::regex*>(re));
	return regex_ns::regex_search(text.text + startpos, text.text + endpos,
				      cpp_re);
    }

    bool stdcpp_regexp_find(stdcpp_regexp *re, stdcpp_string text,
			    int startpos, int endpos,
			    int *match_start, int *match_end) {
	regex_ns::regex cpp_re(*reinterpret_cast<regex_ns::regex*>(re));
	regex_ns::cmatch result;
	bool matched;
	matched = regex_ns::regex_search(text.text + startpos, text.text + endpos,
					 result, cpp_re);
	if (matched) {
	    *match_start = result[0].first - text.text;
	    *match_end = *match_start + result.length(0);
	}
	return matched;
    }
}
