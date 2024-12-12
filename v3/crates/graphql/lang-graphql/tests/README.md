- `testdata` contains snapshot/golden test inputs and expectations for SDL
  parsing
- `query_testdata` and `schema_data` were later added, but for executable
  documents and SDL respectively. Theses cases were copied from the apollo-rs
  project at 721e0753 and the license at `lang-graphql/tests/LICENSE-MIT`
  applies to these. Cases added subsequently are licensed under this project's
  top-level license.
- some cases from the apollo lexer tests are copied with a 9xxx prefix
- `schema_data` cases are currently unused (TODO if SDL parsing is important)

A few full `parse()` cases were also added from
[graphql-js tests](https://github.com/graphql/graphql-js/blob/main/src/language/__tests__/parser-test.ts)
although they were not numerous or very interesting. They are added starting at
`ok/1111_*`

Test cases from fuzzing are added into the `err/6xxx_` namespace. In these cases
a test failure is a panic.

Some `ok` cases that are only valid for the 2021 spec were renamed to `*.2021`
and ignored.

# TODO

- the `ok` case output mostly has not been audited
- We might port the old `testdata` cases to the framework taken from apollo
  since it is nicer.
