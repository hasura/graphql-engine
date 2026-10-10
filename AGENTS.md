# AGENTS.md

## Pull Requests

When creating pull requests, always read `.github/PULL_REQUEST_TEMPLATE.md`
first and use it as the basis for the PR body. Make sure to fill in the correct
checkboxes and put your changelog entry in the appropriate spot. This is used
for release automation so the structure needs to stay intact.

## Building and typechecking

For any haskell code changes use `cabal build all --enable-tests --enable-benchmarks` 
to check your work and iterate until it builds cleanly.

Make sure to always do a final pass with the above before concluding your work!

## Third-party dependency docs

`docs/docs/enterprise/dependencies.mdx` publishes a single table of the
third-party dependencies (name, version, license) shipped by the Haskell
server, the Go CLI and the frontend console. Whenever you add, remove or
upgrade a dependency in any of these, update the matching rows in that table
in the same change:

- **Haskell server** — direct dependencies of `server/graphql-engine.cabal`,
  with the resolved version from `cabal.project.freeze`.
- **Go CLI** — modules from `cli/go.mod`, in the `go-licenses report` format
  (license URL pinned to the module version).
- **Frontend console** — the `dependencies` (not `devDependencies`) of
  `frontend/package.json`, with the version actually resolved in
  `frontend/yarn.lock` (not the `^`/`~` range) and the `license` field from
  the installed package.

Keep each section's existing ordering, preserve the file's CRLF line endings,
and format with `docs/.prettierrc.json`.
