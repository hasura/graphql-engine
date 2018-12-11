# Vue-Hasura-Graphql Hello-World


This repository contains the boilerplate for the hello-world application. This shows how to setup Vue application that uses [Hasura Graphql Engine](https://github.com/hasura/graphql-engine). All necessary files have been setup and can be used for any project.

## Requirements

* Vue version 2.5.2
* Node version >= 6.0.0
* Hasura Engine v1.0.0-alpha20

## Setting up

- **Hasura**: You can setup hasura locally using docker and specify the version given above in requirements to run queries as given in this repo. As some of the query & mutation methods have changed in new releases. We will update this repo with the new verison of engine as soon as possible. To run Hasura Engine locally using docker see [this](https://docs.hasura.io/1.0/graphql/manual/deployment/docker/index.html).

- **Node**: To setup node on your machine you can visit their official [docs](https://nodejs.org/en/download/).

## Setting Environment variables

To setup GRAPHQL_ENDPOINT env variable used in main.js. Go to **config/dev.env.js** and place your environment variables there.

## Directory Structure

- **Components**

This folder contains all the vue.js components.
 
- **Router**

This folder contains all the routes defined for your application

- **src/graphql.js**

This file contains all the queries and mutations required for the graphql.


## Build Setup

``` bash
# install dependencies
npm install

# serve with hot reload at localhost:8080 (or localhost:8081 if your 8080 port is being used by other software e.g. Hasura Engine runs on 8080)
npm run dev

# build for production with minification
npm run build

# build for production and view the bundle analyzer report
npm run build --report
```

## Additional notes:

* For a detailed explanation on how things work, check out the [guide](http://vuejs-templates.github.io/webpack/) and [docs for vue-loader](http://vuejs.github.io/vue-loader).
* If you are behind proxy setup consider reading [this](https://www.jhipster.tech/configuring-a-corporate-proxy/) to setup your proxy support.
* To read more about Hasura Graphql Engine refer to their [docs](https://docs.hasura.io/) and the graphlql-engine [repo](https://github.com/hasura/graphql-engine).

##### Powered By:

![Hasura](../assets/hasura_mascot_logo_horizontal_200px.png)