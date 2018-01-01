module d_phobos;

import core.memory;
import core.stdc.stdlib : malloc, free;

import std.regex;
import std.string;
import std.typecons;

import std.stdio;

import d_phobos_ct;

auto rustRegexToD(string regex) {
    auto flags = "g";
    if (regex.startsWith("(?i)")) {
        flags = "gi";
        regex = regex[4..$];
    } else if (regex.startsWith("(?m)")) {
        flags = "gm";
        regex = regex[4..$];
    }

    return tuple(regex.dup, flags);
}

extern(C):

void* d_phobos_regex_new(string s) {
    auto r = cast(Regex!char*)malloc(Regex!char.sizeof);

    version(CtRegex) {
        auto ctR = getCtRegex();
        *r = ctR[s];
    } else {
        *r = regex(rustRegexToD(s).expand);
    }

    return r;
}

void d_phobos_regex_free(void* r) {
    free(r);
}

bool d_phobos_regex_is_match(void* r, string s) {
    auto regex = *cast(Regex!char*)r;
    return !matchFirst(s, regex).empty;
}

bool d_phobos_regex_find_at(void* r, string s, size_t start, out size_t match_start, out size_t match_end) {
    auto regex = *cast(Regex!char*)r;
    auto match = matchFirst(s[start..$], regex);

    if (match.empty) {
        return false;
    }

    match_start = match.pre().ptr - s.ptr;
    match_end = match.post().ptr - s.ptr;
    return true;
}

