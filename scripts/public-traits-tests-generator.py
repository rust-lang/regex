#!/usr/bin/env python3

from subprocess import run
import json
import os

doc_gen_cmd = ["cargo", "rustdoc", "--", "-Z", "unstable-options", "--output-format", "json"]
def main ():
    doc_gen_result = run(doc_gen_cmd)
    if doc_gen_result.returncode != 0:
        print("Unable to generate the docs for the regex")
        exit(1)

    with open(os.path.join("..", "target", "doc", "regex.json")) as docs_file:
        docs = json.load(docs_file)

        from pprint import pprint

        index = docs['index']
        for key in index.keys():      
            if index[key]['kind'] in ['enum', 'struct']:
                pprint(index[key])






if __name__ == "__main__":
    main()

