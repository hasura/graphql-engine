To build, contribute and view docs:

* Make sure you have `pip` installed on your system

* Install Sphinx
```
pip3 install Sphinx
```

* Now build the docs to produce HTML files. Inside the docs folder:
```
make html-images
```

* The generated docs are in `_build/html`. Navigate to the HTML files by using
a webserver or opening them in the browser. Eg: ``cd _build/html && http-server``

To deploy release versions and view docs:

* Push your release branch `release-0.13` where 0.13 is the platform version.

* Clone https://github.com/hasura/hasura.io

* Go to hasura.io/docs.

* Run docker build -t <imageName> .
