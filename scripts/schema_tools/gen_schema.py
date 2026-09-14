#!/usr/bin/env python3
"""
Generate a "compatible" (not identical) Postgres schema from Hasura metadata,
so the metadata can be applied to a fresh DB without a real customer schema dump.

Approach:
  1. Column universe per table = union of columns referenced in select/insert/update
     permissions (this is the only place metadata.json records column names).
  2. FK edges:
       - foreign_key_constraint_on relationships: local (table, col) -> target table
         inferred from the relationship's default name (Hasura's naming convention:
         object rel name == target table name; array rel name == target table name + 's').
         Target column is assumed to be the target table's primary key.
       - manual_configuration relationships: column_mapping gives explicit
         local-col <-> remote-col pairs (remote table given explicitly).
  3. Union-find over (table, column) pairs joined by any of the above, so joined
     columns are forced to the same inferred type.
  4. Primary key candidate = a column that (a) matches `<table>_id` naming convention,
     or (b) is the target of >=1 foreign_key_constraint_on edge.
  5. Types are NOT inferred from column names -- naming conventions vary too much
     across customer metadata to generalize (e.g. `_ind`/`_dt`/`_nbr` suffixes are
     specific to one file's style, not a rule). Instead every union-find group gets
     one type drawn at random from a fixed, reasonable-for-OLTP-schemas weighted
     distribution (see TYPE_WEIGHTS). Every column in a group -- i.e. every column
     ever joined to another via a relationship -- always gets the same type, which
     is the only property that actually matters for `hasura metadata apply` to
     succeed and for joins to be executable. The draw is seeded, so output is
     reproducible for a given metadata file + seed.

This is heuristic and lossy by design; see WARNINGS in the output for anything
the script had to guess at with low confidence.
"""
import json
import random
import sys
from collections import defaultdict

def load(path):
    with open(path) as f:
        return json.load(f)

class UnionFind:
    def __init__(self):
        self.parent = {}
    def find(self, x):
        self.parent.setdefault(x, x)
        while self.parent[x] != x:
            self.parent[x] = self.parent[self.parent[x]]
            x = self.parent[x]
        return x
    def union(self, a, b):
        ra, rb = self.find(a), self.find(b)
        if ra != rb:
            self.parent[ra] = rb

# Rough shape of column types in a typical OLTP schema. Not derived from this
# (or any) metadata file on purpose -- see module docstring.
TYPE_WEIGHTS = [
    ('text', 30),
    ('integer', 20),
    ('bigint', 10),
    ('uuid', 10),
    ('timestamptz', 10),
    ('numeric', 5),
    ('boolean', 5),
    ('date', 5),
    ('jsonb', 5),
]

def choose_type(rng):
    types, weights = zip(*TYPE_WEIGHTS)
    return rng.choices(types, weights=weights, k=1)[0]

def main(metadata_path, out_sql_path, out_report_path, seed=0):
    d = load(metadata_path)
    warnings = []

    for source in d.get('sources', []):
        src_name = source['name']
        tables = source['tables']
        table_names = {t['table']['name'] for t in tables}
        table_key = lambda t: (t['table']['schema'], t['table']['name'])

        columns = defaultdict(set)          # (schema, table) -> set(col)
        fk_edges = []                       # ((schema,table,col), (schema,table,col))
        pk_hits = defaultdict(int)          # (schema,table,col) -> count of incoming FK

        # 1. column universe from permissions
        for t in tables:
            tk = table_key(t)
            for permkey in ('select_permissions', 'insert_permissions', 'update_permissions'):
                for p in t.get(permkey, []):
                    for c in (p['permission'].get('columns') or []):
                        columns[tk].add(c)

        # 2. relationships -> FK edges / type-unification edges
        for t in tables:
            tk = table_key(t)
            schema = tk[0]
            for relkey in ('object_relationships', 'array_relationships'):
                for r in t.get(relkey, []):
                    using = r.get('using', {})
                    if 'foreign_key_constraint_on' in using:
                        fkon = using['foreign_key_constraint_on']
                        if relkey == 'object_relationships':
                            # string form: the FK column lives on THIS table,
                            # referencing the target table's PK. Target table isn't
                            # named explicitly -- infer it from Hasura's default
                            # relationship-naming convention (name == target table).
                            if not isinstance(fkon, str):
                                warnings.append(
                                    f"UNEXPECTED dict-form foreign_key_constraint_on on an "
                                    f"object relationship: {tk} .{r['name']} -- skipping."
                                )
                                continue
                            local_col = fkon
                            target = r['name'] if r['name'] in table_names else None
                            if target is None:
                                warnings.append(
                                    f"UNRESOLVED fk-relationship: {tk} .{r['name']} "
                                    f"(object_relationships) -- relationship name doesn't match any "
                                    f"table; could not infer target. Left as plain column."
                                )
                                columns[tk].add(local_col)
                                continue
                            columns[tk].add(local_col)
                            target_pk_col = f"{target}_id"  # convention guess, refined below
                            target_tk = (schema, target)
                            columns[target_tk].add(target_pk_col)
                            fk_edges.append(((schema, tk[1], local_col), (schema, target, target_pk_col), True))
                            pk_hits[(schema, target, target_pk_col)] += 1
                        else:
                            # dict form: the FK column lives on the OTHER (many-side)
                            # table, explicitly named, referencing THIS table's PK.
                            if not isinstance(fkon, dict):
                                warnings.append(
                                    f"UNEXPECTED string-form foreign_key_constraint_on on an "
                                    f"array relationship: {tk} .{r['name']} -- skipping."
                                )
                                continue
                            far_col = fkon.get('column')
                            far_table = fkon['table']['name']
                            far_schema = fkon['table'].get('schema', schema)
                            this_pk_col = f"{tk[1]}_id"  # convention guess, refined below
                            columns[(far_schema, far_table)].add(far_col)
                            columns[tk].add(this_pk_col)
                            fk_edges.append(((far_schema, far_table, far_col), (schema, tk[1], this_pk_col), True))
                            pk_hits[(schema, tk[1], this_pk_col)] += 1
                    elif 'manual_configuration' in using:
                        mc = using['manual_configuration']
                        remote_table = mc['remote_table']['name']
                        remote_schema = mc['remote_table'].get('schema', schema)
                        for lcol, rcol in mc['column_mapping'].items():
                            columns[tk].add(lcol)
                            columns[(remote_schema, remote_table)].add(rcol)
                            fk_edges.append(((schema, tk[1], lcol), (remote_schema, remote_table, rcol), False))
                            pk_hits[(remote_schema, remote_table, rcol)] += 1

            # remote relationships still consume local columns (need to exist,
            # but the actual join target is outside postgres -- a remote schema).
            for rr in t.get('remote_relationships', []):
                defn = rr.get('definition', {})
                for f in (defn.get('hasura_fields') or defn.get('lhs_fields') or []):
                    columns[tk].add(f)

        # 3. union-find for type consistency across joins
        uf = UnionFind()
        for a, b, _strict in fk_edges:
            uf.union(a, b)

        # 4. primary key candidates: `<table>_id` convention, boosted by incoming FK hits
        pk_choice = {}
        for tk_ in columns:
            schema, tname = tk_
            conv = f"{tname}_id"
            if conv in columns[tk_]:
                pk_choice[tk_] = conv
            else:
                # fall back to the most-referenced target column, if any
                hits = {c: n for (s2, t2, c), n in pk_hits.items() if (s2, t2) == tk_}
                if hits:
                    pk_choice[tk_] = max(hits, key=hits.get)
                    warnings.append(
                        f"PK GUESS for {tk_}: no '{conv}' column found; using "
                        f"'{pk_choice[tk_]}' (most-referenced FK target) instead."
                    )

        # 5. one random (but reproducible) type per union-find group -- every
        #    column in a group is joined to every other, directly or transitively,
        #    so they must all share a type regardless of what that type is.
        rng = random.Random(seed)
        group_roots = set()
        for tk_, cols in columns.items():
            for c in cols:
                group_roots.add(uf.find((tk_[0], tk_[1], c)))
        # sort for determinism: set iteration order is hash-seed dependent, but
        # the roots are plain tuples of strings so sorting them is stable.
        group_type = {root: choose_type(rng) for root in sorted(group_roots)}

        def col_type(tk_, c):
            root = uf.find((tk_[0], tk_[1], c))
            return group_type[root]

        # 6. emit DDL
        ddl = []
        ddl.append(f"-- Generated compatible schema for Hasura source '{src_name}'")
        ddl.append("-- NOT a real customer schema -- column names are real, types/PKs are inferred/guessed.")
        ddl.append("BEGIN;")
        for schema in sorted({s for s, _ in columns}):
            ddl.append(f'CREATE SCHEMA IF NOT EXISTS "{schema}";')

        for tk_ in sorted(columns):
            schema, tname = tk_
            cols = sorted(columns[tk_])
            col_lines = []
            pk = pk_choice.get(tk_)
            for c in cols:
                ty = col_type(tk_, c)
                if c == pk:
                    col_lines.append(f'  "{c}" {ty} PRIMARY KEY')
                else:
                    col_lines.append(f'  "{c}" {ty}')
            if not pk:
                warnings.append(f"NO PRIMARY KEY inferred for {tk_}; table created without one.")
            ddl.append(f'CREATE TABLE "{schema}"."{tname}" (')
            ddl.append(",\n".join(col_lines))
            ddl.append(");")

        # 6b. real FK constraints for foreign_key_constraint_on relationships --
        #     Hasura validates these against actual pg constraints at apply time,
        #     unlike manual_configuration relationships which only need matching columns.
        seen_fk = set()
        for (s1, t1, c1), (s2, t2, c2), strict in fk_edges:
            if not strict:
                continue  # manual_configuration edge -- Hasura doesn't require a real FK for these
            if pk_choice.get((s2, t2)) != c2:
                warnings.append(
                    f"SKIPPED real FK constraint for {(t1, c1)} -> {(t2, c2)}: "
                    f"'{c2}' isn't {t2}'s chosen primary key, so a UNIQUE/PK-backed "
                    f"REFERENCES target isn't available. `metadata apply` will report this "
                    f"relationship as inconsistent unless you fix the PK guess by hand."
                )
                continue
            key = (s1, t1, c1, s2, t2, c2)
            if key in seen_fk:
                continue
            seen_fk.add(key)
            cname = f"fk_{t1}_{c1}_{t2}_{c2}"[:63]
            ddl.append(
                f'ALTER TABLE "{s1}"."{t1}" ADD CONSTRAINT "{cname}" '
                f'FOREIGN KEY ("{c1}") REFERENCES "{s2}"."{t2}" ("{c2}");'
            )

        # 7. custom functions: metadata only records name/config, never args/return type.
        # Hasura requires a trackable custom function (query or mutation) to return
        # SETOF <table> (a composite type) -- a bare scalar/void is rejected.
        for fn in source.get('functions', []):
            fname = fn['function']['name']
            fschema = fn['function'].get('schema', 'public')
            exposed_as = fn.get('configuration', {}).get('exposed_as', 'query')
            volatility = 'VOLATILE' if exposed_as == 'mutation' else 'STABLE'
            stub_table = tables[0]['table']['name']
            warnings.append(
                f"CUSTOM FUNCTION '{fschema}.{fname}' (exposed_as={exposed_as}): metadata does not "
                f"record arguments or return type. Emitting a best-effort no-op stub "
                f"(zero args, RETURNS SETOF \"{fschema}\".\"{stub_table}\") purely so it's trackable; "
                f"the real function almost certainly has different args/return table -- fix by hand."
            )
            ddl.append(
                f'CREATE OR REPLACE FUNCTION "{fschema}"."{fname}"() '
                f'RETURNS SETOF "{fschema}"."{stub_table}" '
                f'LANGUAGE plpgsql {volatility} AS $$ BEGIN RETURN QUERY SELECT * FROM "{fschema}"."{stub_table}" LIMIT 0; END; $$;'
            )

        ddl.append("COMMIT;")

        with open(out_sql_path, 'w') as f:
            f.write("\n".join(ddl) + "\n")

    if d.get('remote_schemas'):
        names = [rs['name'] for rs in d['remote_schemas']]
        warnings.append(
            f"REMOTE SCHEMAS present ({names}): `metadata apply`/tracking will try to introspect "
            f"these live GraphQL endpoints. They are unrelated to the Postgres schema and will need "
            f"either a reachable mock server per remote schema, or removal from the metadata copy "
            f"used for this kind of testing."
        )

    with open(out_report_path, 'w') as f:
        f.write(f"{len(warnings)} warnings:\n\n")
        for w in warnings:
            f.write("- " + w + "\n")

    print(f"Wrote {out_sql_path}")
    print(f"Wrote {out_report_path} ({len(warnings)} warnings)")

if __name__ == '__main__':
    if len(sys.argv) not in (4, 5):
        print(
            f"usage: {sys.argv[0]} <metadata.json> <out_schema.sql> <out_report.txt> [seed]",
            file=sys.stderr,
        )
        sys.exit(1)
    seed = int(sys.argv[4]) if len(sys.argv) == 5 else 0
    main(sys.argv[1], sys.argv[2], sys.argv[3], seed)
