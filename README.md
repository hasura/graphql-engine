**To run docs locally and to contribute:**

* Make sure you have `pip` installed on your system

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


* To deploy docs, follow [this](https://github.com/hasura/docs/wiki/How-to-deploy-docs)

* To update docs search index, follow [this](https://github.com/hasura/docs/wiki/How-to-update-algolia-index)