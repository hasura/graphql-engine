To build, contribute and view docs:

* Make sure you have `pip` installed on your system

* Install Sphinx, beautifulsoup4, algoliasearch

```
pip3 install -r requirements.txt
```

* Now build the docs to produce HTML files. Inside the docs folder:
```
make html-images
```

* The generated docs are in `_build/html`. Navigate to the HTML files by using
a webserver or opening them in the browser. Eg: 

``cd _build/html && http-server``

or

``cd _build/html && python3 -m http.server 8080``
