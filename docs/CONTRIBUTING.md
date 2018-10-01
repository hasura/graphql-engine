# Contributing

[Sphinx](http://www.sphinx-doc.org/en/master/documentation) files are written in
the RST markup language. Here is a [guide to RST markup
language](http://www.sphinx-doc.org/en/master/usage/restructuredtext/basics.html).

## Requirements

- [Python 3](https://www.python.org/downloads/)
- [Pip for Python 3](https://pip.pypa.io/en/stable/installing/)

## Steps

- Fork the repo and clone it:
  ```bash
  git clone https://github.com/<your-username>/graphql-engine
  ```
- Move to `docs` folder and checkout to a new branch:
  ```bash
  cd docs
  git checkout -b <new-branch-name>
  ```
- Install dependencies (Sphinx, beautifulsoup4, algoliasearch, etc.)
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
    ```
    ENV=<development|production> make html-images
    ```
    * The generated docs are in `_build/html`. 
    * View the built files by running a webserver. egg:
        ```
        cd _build/html && http-server
        ```
        or

        ```
        cd _build/html && python3 -m http.server 8080
        ```        
- Commit the changes. Follow common guidelines for commit messages at [main
contributing guide](../CONTRIBUTING.md#common-guidelines).
- Push the changes to your fork and submit a pull request.Any pull request requires the signing of the 
CLA (Contributor License Agreement) before or after it is submitted, which can be found at 
https://cla-assistant.io/hasura/graphql-engine

**Note:** The search is powered by [Algolia](https://www.algolia.com/) and is updated on every deployment. Your local 
  changes will not reflect in search results.        
