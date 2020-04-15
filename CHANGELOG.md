# Hasura GraphQL Engine Changelog

## `v1.2.0` (in beta)

### New features :tada:

- Introducing Actions: https://docs.hasura.io/1.0/graphql/manual/actions/index.html
- Downgrade command: https://hasura.io/docs/1.0/graphql/manual/deployment/downgrading.html#downgrading-hasura-graphql-engine
- console: add multi select to data table and bulk delete (#3735)

  Added a checkbox to each row on Browse Rows view that allows selecting one or more rows from the table and bulk delete them.

- server: support reusing Postgres scalars in custom types (close #4125)
- console: allow setting check constraints during table create (#3881)

  Added a component that allows adding check constraints while creating a new table in the same way as it can be done on the `Modify` view.

  ### Select dropdown for Enum columns (console)

  If a table has a field referencing an Enum table via a foreign key, then there will be a select dropdown with all possible enum values on `Insert Row` and `Edit Row` views on the Console.

  (close #3748) (#3810)

### Other changes

- console: disable editing action relationships
- cli: fix typo in cli example for squash (fix #4047) (#4049)
- console: fix run_sql migration modal messaging (close #4020) (#4060)
- docs: add note on pg versions for actions (#4034)
- console: update actions intro image (#4042)
- console: hide starter kit button if a framework has no starter kit (#4023)
- docs: avoid redirect, update title tag suffix (#4030)
- More robust forking, exception safety. Closes #3768 (#3860)
- tag release v1.2.0-beta.2 (#4028)
- docs: correct version info for config v2 section (close #4019) (#4026)
- console: add TypeScript setup (#3902)
- cli: add version flag in update-cli command (#3996)
- cli(migrations-img): add env to skip update prompts (fix #3964) (#3968)
- Don't update catalog version if using --dryRun (#3970)
- cli, server: use prerelease tag as channel for console assets cdn (#3975)
- docs: fix broken link (#4005)
- docs: update connecting actions page title (#4008)
- update actions docs (#4007)
- fix syntax error in codeowners file (#4006)
- cli: fix flags in actions, migrate and metadata cmd (fix #3982) (#3991)
- cli(actions): preserve action definition in metadata apply (fix #3988) (#3993)
- build: rename file to adhere to windows rules (close #4002) (#4003)
- translations(readme): add turkish (#3921)
- docs: add AuthGuardian JWT guide (#3958)
- revert auth heading changes in docs (#3992)
- add changelog file to the repo and update pr template (#3946)
- fix docs 404 (#3979)
- tag release v1.2.0-beta.1 (#3966)
- noop: replace subdomain links with subpath (#3869)
- docs: add reference to QualifiedTable to table args (#3880)
- update actions docs (#3953)
- cli: bug fixes related to actions (#3951)
- update docs (#3947)
- fix regression on tag release script (#3944)
- cli: misc fixes related to actions (#3942)
- rfc: check condition in update permissions (#3750)
- docs: add actions docs (#3907)
- cli: allow managing actions (#3859)
- Maintain downgrade commands in a text file, update when tagging (#3933)
- fix new release notification logic (#3930)
- Update Init.hs for newer tags (#3931)
- adds postgres password to docker-compose setup (fix #3894) (#3895)
- update manifests to v1.1.0 (#3913)
- docs: remove wip actions docs (#3909)
- docs: use install manifests from stable branch (#3906)
- console: add actions support (#3889)
- console: show pre-release update notifications with opt out option (#3888)
- translation(readme): add korean (#3818)
- allow custom mutations through actions (#3042)
- run default tests in test_server_upgrade (#3718)
- Add check expresion to update permissions (close #384) (#3804)
- cli: update installation instructions in the readme (fix #3875) (#3876)
- handle invalid keys in permission builder (close #3848) (#3863)
- fix casting citext column type (fix #2818) (#3861)
- Add downgrade command (close #1156) (#3760)
- persist mix files only when coverage is enabled (#3844)
- add meta descriptions to actions docs (#4082)
- `HASURA_GRAPHQL_EVENTS_FETCH_INTERVAL` changes semantics slightly: we only sleep for the interval
  when there were previously no events to process. Potential space leak fixed. (#3839)
- auto-include `__typename` field in custom types' objects (fix #4063)
- server: validate action webhook response to conform to action output type (fix #3977)
- server: preserve cookie headers from sync action webhook (close #4021)
- server: add 'ID' to default scalars in custom types (fix #4061)
