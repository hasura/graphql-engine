# Contributing

[Sphinx](http://www.sphinx-doc.org/en/master/documentation) files are written in
the RST markup language. [Here]((http://www.sphinx-doc.org/en/master/usage/restructuredtext/basics.html)) is a guide to the RST markup language.

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

**Note:** The search is powered by [Algolia](https://www.algolia.com/) and is updated on every deployment. Your local changes will not be reflected in search results.        
