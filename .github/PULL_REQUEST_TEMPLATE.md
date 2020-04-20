<!-- Thank you for submitting this PR! :) -->
<!-- Provide a general summary of your changes in the Title above ^, end with (close #<issue-no>) or (fix #<issue-no>) -->

### Description
<!-- The title might not be enough to convey how this change affects the user. -->
<!-- Describe the changes from a user's perspective -->

### Changelog

- [ ] `CHANGELOG.md` is updated with user-facing content relevant to this PR.

### Affected components
<!-- Remove non-affected components from the list -->

- [ ] Server
- [ ] Console
- [ ] CLI
- [ ] Docs
- [ ] Community Content
- [ ] Build System
- [ ] Tests
- [ ] Other (list it)

### Related Issues
<!-- Please make sure you have an issue associated with this Pull Request -->
<!-- And then add `(close #<issue-no>)` to the pull request title -->
<!-- Add the issue number below (e.g. #234) -->

### Solution and Design
<!-- How is this issue solved/fixed? What is the design? -->
<!-- It's better if we elaborate -->

### Steps to test and verify
<!-- If this is a feature, what are the steps to try them out? -->
<!-- If this is a bug-fix, how do we verify the fix? -->

### Limitations, known bugs & workarounds
<!-- Limitations of the PR, known bugs and suggested workarounds -->
<!-- Feel free to delete these comment lines -->

### Server checklist
<!-- A checklist for server code -->

#### Catalog upgrade
<!-- Is hdb_catalog version bumped? -->
Does this PR change Hasura Catalog version?
- [ ] No
- [ ] Yes
  - [ ] Updated docs with SQL for downgrading the catalog <!-- https://hasura.io/docs/1.0/graphql/manual/deployment/downgrading.html#downgrading-across-catalogue-versions -->

#### Metadata
<!-- Hasura metadata changes -->

Does this PR add a new Metadata feature?
- [ ] No
- [ ] Yes
  - Does `run_sql` auto manages the new metadata through schema diffing?
    - [ ] Yes
    - [ ] Not required
  - Does `run_sql` auto manages the definitions of metadata on renaming?
    - [ ] Yes
    - [ ] Not required
  - Does `export_metadata`/`replace_metadata` supports the new metadata added?
    - [ ] Yes
    - [ ] Not required


#### GraphQL
- [ ] No new GraphQL schema is generated
- [ ] New GraphQL schema is being generated:
   - [ ] New types and typenames are correlated
   <!-- No dangling types or typenames with missing types (a potential bug, introspection fails) -->
   <!-- If you have anything in your mind, which can be added here as a check list item, please submit a PR to update this template :) -->

#### Breaking changes

- [ ] No Breaking changes
- [ ] There are breaking changes:

  1. Metadata API

     Existing `query` types:
     - [ ] Modify `args` payload which is not backward compatible
     - [ ] Behavioural change of the API
     - [ ] Change in response `JSON` schema
     - [ ] Change in error code
     <!-- Add if anything not listed above -->

  2. GraphQL API

     Schema Generation:
     <!-- Any changes in schema auto-generation logic -->
     <!-- All GraphQL schema names are case sensitive -->
     - [ ] Change in any `NamedType`
     - [ ] Change in table field names
     <!-- Add if anything not listed above -->

     Schema Resolve:-
     <!-- Any change in logic of resolving input request -->
     - [ ] Change in treatment of `null` value for any input fields <!-- Explain them below -->
     <!-- Add if anything not listed above -->

  3. Logging

     - [ ] Log `JSON` schema has changed
     - [ ] Log `type` names have changed
     <!-- Add if anything not listed above -->

<!-- Add any other breaking change not mentioned above -->

<!-- Explain briefly about your breaking changes below -->
