# Contributing Guidelines

Some basic conventions for contributing to this project.

### General

Please make sure that there aren't existing pull requests attempting to address the issue mentioned.
Likewise, please check for issues related to update, as someone else may be working on the issue in a branch or fork.

- Non-trivial changes should be discussed in an issue first
- Develop in a topic branch, not master
- Squash your commits

### Linting

There's a pre-commit hook which uses Prettier.io for linting and indenting code. This is automated.

### Commit Message Format

Each commit message should include a **console:**, a **subject**:

```
 console: <subject>
```

Lines should not exceed 100 characters. This allows the message to be easier to read on github as well as in various git tools and produces a nice, neat commit log ie:

```
 #459  console: fix graphiql scroll
 #463  console: add react dependency
```

#### General Commit Message Rules

- use the imperative, present tense: "change" not "changed" nor "changes"
- don't capitalize first letter
- no dot (.) at the end
