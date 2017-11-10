To build, contribute and view docs:

* Make sure you have `pip` installed on your system

* Install Sphinx, beautifulsoup4, algoliasearch

```
pip3 install -r requirements.txt
```

* Live reload and auto build

```bash
make livehtml
```

* To prioritize articles

```
  Add a meta tag as follow

  :created-on: 2017-07-28T10:20:35.073Z
```

* Now build the docs to produce HTML files. Inside the docs folder:
```
ENV=development make html-images

ENV will be development/production depending on the environment docs is built for.
```

* To index the documents to algolia, run the following command with the correct parameters

`` make algolia_index ALGOLIA_APPLICATION_ID=<ALGOLIA_APP_ID > ALGOLIA_SEARCH_KEY=< ALGOLIA_SEARCH_KEY > ALGOLIA_ADMIN_KEY=<ALGOLIA_ADMIN_KEY >``

* The generated docs are in `_build/html`. Navigate to the HTML files by using
a webserver or opening them in the browser. Eg: 

``cd _build/html && http-server``

or

``cd _build/html && python3 -m http.server 8080``

a webserver or opening them in the browser. Eg: ``cd _build/html && http-server``

To deploy release versions and view docs:

* Push your release branch `release-0.13` where 0.13 is the platform version.

* Clone https://github.com/hasura/hasura.io

* Go to hasura.io/docs.

* Update the timestamp value inside ``Dockerfile`` to update staging or ``Dockerfile_prod`` to update production.

* git push to appropriate remote ( staging/production ) to deploy .
`` git push hasura_stg master ``
