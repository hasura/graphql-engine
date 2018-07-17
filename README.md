### This is the documentation for the Hasura GraphQL engine

This documentation project is built using [Sphinx](http://www.sphinx-doc.org/en/master/).

Please raise PRs to contribute.

**To run docs locally:**

* Make sure you have `pip3` installed on your system

* Install dependencies (Sphinx, beautifulsoup4, algoliasearch, etc.)
    ```
    pip3 install -r requirements.txt
    ```

* For development, live reload and auto build
    ```bash
    make livehtml
    ```

* Build docs to produce HTML files. 
    ```
    ENV=<development|production> make html-images
    ```
    * The generated docs are in `_build/html`. 
    * View the built files by running a webserver. eg:
        ```
        cd _build/html && http-server
        ```
        or

        ```
        cd _build/html && python3 -m http.server 8080
        ```        
        
* **Note:** The search is powered by [Algolia](https://www.algolia.com/) and is updated on every deployment. Your local 
  changes will not reflect in search results.        