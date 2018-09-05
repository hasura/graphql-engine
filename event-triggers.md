# Event triggers on Postgres

Trigger webhooks on database events using Hasura GraphQL Engine's **event triggers**.

**Highlights**

* **Build reactive & async apps/features**: Trigger asynchronous serverless or cloud functions to reduce infrastructure costs and simplify DevOps for developers.

* **Atomic & Reliable**: Using native Postgres listen/notify, every relevant write on the database is captured as an event. Even if Hasura is down or being updated, events will be delivered as soon as possible with an atleast-one guarantee. You can even configure delivery policies like `max_retries` and `retry_interval`.

* **Works with existing, live databases**: Point it to an existing Postgres database to instantly listen to changes in your data and invoke webhooks.


## Quickstart: 

### One-click deployment on Heroku

The fastest way to try event triggers out is via Heroku.

1. Click on the following button to deploy GraphQL Engine on Heroku with the free Postgres add-on:

    [![Deploy to Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

2. Open the Hasura console

   Visit `https://<app-name>.herokuapp.com` (*replace \<app-name\> with your app name*) to open the admin console.

3. Configure your first event trigger and webhook

   Create a table, configure the db update you want to use as a trigger and instantly invoke a webhook by adding a new row in your table. Follow this [simple guide](https://docs.hasura.io/1.0/graphql/manual/getting-started/first-graphql-query.html).

### Other deployment methods

For Docker-based deployment and advanced configuration options, see [deployment guides](https://docs.hasura.io/1.0/graphql/manual/getting-started/index.html).

### Demo & Resources

**Watch**: [Create an event-trigger and webhook in 60 seconds](https://some-youtube-demo.com) (*10:00 mins*)

**Resources**: Use one of our serverless boilerplates (AWS Lambda, Google Cloud Functions, Azure Functions, Zeit Serverless Docker, etc.) to deploy a webhook that can capture database events - https://github.com/hasura/cloud-functions-boilerplates

## Architecture

![Event triggers architecture](assets/event-triggers-arch.png)

## Demos & Tutorials: Building reactive & async apps/features

### Notifications

Trigger push notifications and emails based on database events. Try the demo and tutorial below to see how browser push notifications are triggered when the user inserts some data:

* [Watch Demo](https://some-youtube-demo.com) (*10:00 mins*)
* [Try it out](https://shahidh.in/hasura-web-push-notifs/)
* [Tutorial](https://github.com/shahidhk/hasura-web-push-notifs)


### Async business logic

Convert complex, long-running business logic to be event-driven, asynchronous and resilient to failure. Try this demo and tutorial below to see how an image processing job is run asynchronously to convert an image to a black-and-white version:

* [Watch Demo](https://some-youtube-demo.com) (*10:00 mins*)
* [Try it out](https://shahidh.in/hasura-web-push-notifs/)
* [Tutorial](https://github.com/shahidhk/hasura-web-push-notifs)


### Data transformation (ETL)

Transform and load data into external data-stores. Check out this demo and tutorial below to see how Postgres data is transformed to build and populate an Algolia index:

* [Watch Demo](https://some-youtube-demo.com) (*10:00 mins*)
* [Try it out](https://shahidh.in/hasura-web-push-notifs/)
* [Tutorial](https://github.com/shahidhk/hasura-web-push-notifs)


