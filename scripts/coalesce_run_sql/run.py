#!/usr/bin/env python
import sys
from pathlib import Path
import ruamel.yaml 
from ruamel.yaml import YAML
import textwrap
import copy

filename = sys.argv[1]
yaml = YAML(typ='rt')
orig = yaml.load(Path(filename))

sql = ""
comments = ""
ixs_to_delete = []

if orig['type'] != 'bulk':
    sys.exit(0)

if orig['args'].ca.items:
    try:
        for i in orig['args'].ca.items.values():
            for j in i:
                if j is not None:
                    for k in j:
                        comments += k.value
    except KeyError:
        print(orig['args'].ca.items)

min_ix = None

for ix, p in enumerate(orig['args']):
    if p['type'] == 'run_sql':
        if min_ix is None:
            min_ix = ix
        #print(p.ca)

        if p.ca.items:
            for i in p.ca.items.values():
                if i is not None:
                    for j in i:
                        comments += j.value

        current = p['args']['sql']

        # some sql strings don't end with a ;
        # so we fix those up
        if current[-2:] != ";\n":
            current = current[:-1] + ";\n"

        sql += current
        ixs_to_delete.append(ix)

# ancient trick
# if you do it in any order that isn't monotonically decreasing
# the indices shift after each del
# i don't _like_ using del but CommentedSeq has no .delete() method
for ix in sorted(ixs_to_delete, reverse=True):
    del orig['args'][ix]
print(sql)
print("---")
if len(sql) < 5: 
    print(filename)
    print(sql)
    print("---")
    sys.exit(0)

coalesced = """
{}
  type: 'run_sql'
  args: 
    sql: |
{}
""".format(textwrap.indent(comments, " " * 2), textwrap.indent(sql, " " * 8))
# print(coalesced)

orig['args'].insert(min_ix, yaml.load(coalesced))
#orig.yaml_set_comment_before_key('args', comments)
with open(filename, "w") as outfile:
    yaml.dump(orig, outfile)

#for k, v in y:
#    print(k)
#    yaml.dump(v, sys.stdout)
