# Vue-Hasura-Boilerplate

This repository contains the boilerplates for setting up Vue application using Hasura. It uses [Hasura Graphql Engine](https://github.com/hasura/graphql-engine) for its backend support. It contains three boilerplates which can be used depending on need of application.

* Hello-World:

This contains the basic configuration that is needed to just setup an application which will connect to Hasura Graphql engine using apollo client.

* Basic:

Apart from the basic configuration this application contains the Queries, Subscriptions and Mutation examples for Ghrahql queries that one can make to Hasura engine. This application demonstrates these queries using a To-Do application.

* Advanced:

Apart from what we did in Basic app this application contains Auth0 setup necessary for authorizing users.



## Getting Started

Clone this repo using:

`git clone https://github.com/pradeepgangwar/Vue-Graphql.git`

cd into the folder you want to and install the dependencies using:

`npm install`

Once you have installed all the dependencies you can start the application using:

`npm start`

If everything goes well you should have your application running at `localhost:8080`

## Additional notes:

* If you are behind proxy setup consider reading [this](https://www.jhipster.tech/configuring-a-corporate-proxy/) to setup your proxy support.
* To read more about Hasura Graphql Engine refer to their [docs](https://docs.hasura.io/) and the graphlql-engine [repo](https://github.com/hasura/graphql-engine).

##### Powered By:

![Hasura](assets/hasura_mascot_logo_horizontal_200px.png)