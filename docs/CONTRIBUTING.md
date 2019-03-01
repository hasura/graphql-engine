# Contributing

Hasura GraphQL engine docs are built using [Sphinx](http://www.sphinx-doc.org/en/master/).
Sphinx files are written using the [RST markup language](http://www.sphinx-doc.org/en/master/usage/restructuredtext/basics.html).

### Docs issues in the repo

Issues in the repo for documentation are labelled as `c/docs` (see [list](https://github.com/hasura/graphql-engine/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen++label%3Ac%2Fdocs)). Issues also labelled as `good first issue`  are aimed at those making their first contribution to the repo (see [list](https://github.com/hasura/graphql-engine/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen++label%3Ac%2Fdocs+label%3A%22good+first+issue%22)). Issues also marked as `help wanted` (see [list](https://github.com/hasura/graphql-engine/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aopen++label%3Ac%2Fdocs+label%3A%22help+wanted%22)) are those that require community contributions on priority.

Please note that some of these issues, labelled with both `c/docs` and `c/console`/`c/server`, are part of a change/task that requires modifications in some component of GraphQL Engine and the docs.

Feel free to open pull requests to address these issues or to add/fix  docs features/content, even if a corresponding issue doesn't exist. If you are unsure about whether to go ahead and work on something like the latter, please get in touch with the maintainers in the `GraphQL Engine`->`contrib` channel in the community [Discord](https://discord.gg/vBPpJkS).

## Requirements

- [Python 3](https://www.python.org/downloads/)
- [Pip for Python 3](https://pip.pypa.io/en/stable/installing/)

## Steps

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
- Commit the changes. Follow the common guidelines for commit messages at the [main
contributing guide](../CONTRIBUTING.md#common-guidelines).
- Push the changes to your fork and submit a pull request.

## Some guidelines while modifying docs
- Just before committing your changes, delete your local `_build` folder completely and then build docs again. Scan 
the output and look for any Warnings (e.g. title underline too short, Could not lex literal block, etc.). Ideally 
there should not be any Warnings that are being thrown.
- Keep heading underlining the same length as the heading. It's just aesthetically nice. (Short underlines will 
even throw a Warning)
- Page titles should be self sufficient. Don't expect the user to have context of the hierarchy of the page in the 
docs tree. A user can land on a page via search as well. e.g. Say you are adding a new deployment guide for AWS under 
`Guides -> Deployment -> AWS`. The title of this page should not be just `AWS` but instead `AWS deployment guide for 
Hasura GraphQL engine`. It's fine to alias it to just `AWS` in the sidebar as there the user has the context of the 
page hierarchy.
- Ensure every new added page has a ``Table of contents`` section with the right depth. You can see any existing
page for reference on how to do this.
- When referring to an external link using `` `....`_``, add an extra underscore to the link. ie. `` `...`__``. A 
single underscore creates a global link which can then be referred from any page in the docs and hence might cause 
some conflicts with other links sometimes (a conflict will show up as a Warning though while building).   
- Before adding an image to docs, first compress it via some tool to ensure users won't have to unnecessarily 
download more data than needed. You can use www.tinypng.com for this. Sometimes you can compress images by 
upto 75% without losing any visible quality.
- Add appropriate cross-links in content to assist users. i.e. if you refer to some functionality that is documented in 
some other docs page, add a link to that page. e.g. if you have a statement like "create a relationship between tables
X and Y ...", make "create a relationship" a link to the `Create relationships` page.
- Try to commit logically separate changes into different commits. i.e. if you need to update the `.gitignore` 
file for some reason and also have other normal docs changes, commit the gitignore change separately for better 
visibility. Ideally each commit should perform just one specific function. e.g. add xyz deployment guide, update 
gitignore, fix broken link, etc. This is not very important though so don't spend too much effort trying to achieve 
this. 

**Notes:** 
- Docs are currently deployed manually. Changes will not reflect immediately after a PR gets merged.
- The search is powered by [Algolia](https://www.algolia.com/) and is updated everyday. Your local changes 
will not be reflected in search results.        
