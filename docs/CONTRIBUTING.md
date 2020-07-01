# Contributing

Hasura GraphQL engine docs are built using [Sphinx](http://www.sphinx-doc.org/en/master/).
Sphinx files are written using the [RST markup language](http://www.sphinx-doc.org/en/master/usage/restructuredtext/basics.html).

## Docs issues in the repo

Issues in the repo for documentation are labelled as `c/docs` 
(see [list](https://github.com/hasura/graphql-engine/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen++label%3Ac%2Fdocs)). 
Issues also labelled as `good first issue`  are aimed at those making their first contribution to the repo 
(see [list](https://github.com/hasura/graphql-engine/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen++label%3Ac%2Fdocs+label%3A%22good+first+issue%22)). 
Issues also marked as `help wanted` 
(see [list](https://github.com/hasura/graphql-engine/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen++label%3Ac%2Fdocs+label%3A%22help+wanted%22)) 
are those that require community contributions.

Please note that some of these issues, labelled with both `c/docs` and `c/console`/`c/server`, are part of a 
change/task that requires modifications in some component of GraphQL engine and the docs.

Feel free to open pull requests to address these issues or to add/fix  docs features/content, even if a 
corresponding issue doesn't exist. If you are unsure about whether to go ahead and work on something like 
the latter, please get in touch with the maintainers in the `GraphQL engine`->`contrib` channel in the 
community [Discord](https://discord.gg/vBPpJkS).

## Setup requirements

- [Python 3](https://www.python.org/downloads/)
- [Pip for Python 3](https://pip.pypa.io/en/stable/installing/)

## Steps for contributing

We use the [fork-and-branch git workflow](https://blog.scottlowe.org/2015/01/27/using-fork-branch-git-workflow/).

- Fork the repo and clone it:
  ```bash
  git clone https://github.com/<your-username>/graphql-engine
  ```
- Move to the `docs` folder via the command line and checkout to a new branch:
  ```bash
  cd docs
  git checkout -b <new-branch-name>
  ```
- Install dependencies:
  ```
  pip3 install -r requirements.txt
  ```
- For development, live reload and auto build while you're editing and saving
  files:
  ```bash
  make livehtml
  ```
- Make the required changes.
- (Optional) Build docs to produce HTML files and verify:
    ```bash
    ENV=<development|production> make html-images
    ```
    - The generated docs are in `_build/html`. 
    - View the built files by running a webserver:
        ```bash
        cd _build/html && http-server
        ```
        or

        ```bash
        cd _build/html && python3 -m http.server 8080
        ```        
- Commit the changes. Follow the common guidelines for commit messages at the 
[main contributing guide](../CONTRIBUTING.md#common-guidelines).
- Push the changes to your fork and submit a pull request.

### Changelog 

In order for all the checks to pass on Github, it's required to do one of the following:

- Make an entry to the `CHANGELOG.md` file (under "Next release") to describe the change from a user's perspective.

- Add the `no-changelog-required` label.

For docs, a changelog entry is required for the following:

- Entire new docs pages
- Considerable changes in the overall structure

For small changes (such as fixes, adding examples, UI changes etc.), the `no-changelog-required` label will suffice.

## Some guidelines while adding docs

### Header section
- Page titles should be self sufficient. Users might not have the context of the hierarchy of the page in the 
docs tree. A user can land on a page via search as well. e.g. Say you are adding a new deployment guide for AWS under 
`Guides -> Deployment -> AWS`. The title of this page should not be just `AWS` but instead `AWS deployment guide for 
Hasura GraphQL engine`. It's ok to alias it to just `AWS` in the sidebar as there the user has the context of the 
page hierarchy.
- Ensure every new added page has a ``Table of contents`` section with the appropriate depth.

### Content
- Add appropriate cross-links in content to assist users. i.e. if you refer to some functionality that is documented in 
some other docs page, add a link to that page. e.g. if you have a statement like "create a relationship between tables
X and Y ...", make "create a relationship" a link to the `Create relationships` page.
- Try to make each section within a page self-sufficient. i.e. avoid structuring all pages as step-by-step guides
unless it really is the intent. This ensures that we can refer to sections externally (from other docs pages, console, 
etc.) and expect that the user will be able to follow the section without being lost on context that was set in earlier 
sections of the page. Adding statements such as "As we have described in the above section ..." might help to set up 
the needed context.

### GraphQL request examples
While adding GraphQL request examples:
- Use a tab width of 2 for nesting the requests and responses for optimal use 
of the space and maintaining consistency.
- Nest query arguments for logical readability. Unfortunately GraphiQL prettify does not do a good job of doing this 
by default.
- Ensure that the order of fields in the responses is the same as in the requests for better readability.

### Images
- Before adding images to docs, first compress them via some tool to ensure users don't have to unnecessarily 
download more data than needed. You can use www.tinypng.com for this. Sometimes you can compress images by over 75% 
without losing any visible quality.
- Use the `thumbnail` directive for images to allow click-to-zoom.

### Syntax
- Ensure heading underlines are the same length as the headings. Short underlines will throw warnings during builds.
- Use bold in headings in place of string literals for aesthetics (i.e. ** in place of ``)
- While adding code blocks ensure the right language type is set. Sometimes adding placeholders breaks the language's
syntax in which case you'll have to set the language type to `none` to avoid warnings during builds.
- Use `:ref:` instead of `:doc:` to link to pages to avoid having to set the relative path and chances of broken links
while moving pages

### Reference links

- For external links, add a double `_` in the end to avoid `Duplicate explicit target name` warnings  , e.g. \``Google <https://www.google.com/>__`\`
- If you link to an external resource, make sure to link to the most current version of the same, e.g. `https://www.postgresql.org/docs/current/intro-whatis.html` rather than `https://www.postgresql.org/docs/9.6/intro-whatis.html`

### Pre-commit checks
- Just before committing your changes, delete your local `_build` folder completely and then build docs again. Scan 
the output and look for any syntax warnings (e.g. title underline too short, could not lex literal block, etc.). 
Ideally there should not be any syntax warnings that are being thrown.

## Notes
- Docs are currently deployed manually. Changes will not reflect immediately after a PR gets merged.
- The search is powered by [Algolia](https://www.algolia.com/) and is updated everyday. Your local changes 
will not be reflected in search results.        
