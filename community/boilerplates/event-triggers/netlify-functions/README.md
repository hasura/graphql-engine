# Event trigger boilerplates on Netlify

Contributions are welcome for boilerplates in other languages.

### Setup Hasura GraphQL Engine

Click on the following button to deploy GraphQL Engine on Heroku with the free Postgres add-on:

[![Deploy to Heroku](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy?template=https://github.com/hasura/graphql-engine-heroku)

Checkout [docs](https://docs.hasura.io/1.0/graphql/manual/deployment/index.html) for other deployment options.


###  Setup `netlify-cli`

1. Create a Netlify account @ https://app.netlify.com
2. Download [netlify-cli](https://www.netlify.com/docs/cli/):
   ```bash
   npm install netlify-cli -g
   ```
3. Login to `netlify`:
   ```bash
   ntl login
   ```
4. Create a Netlify instance:
   ```bash
   ntl init # or `ntl link` if you already have one and want to link the project
   ```

### Setup and deploy the trigger

```
ntl functions:create # and pick the hasura template
```

The code is located [here](https://github.com/netlify/netlify-dev-plugin/tree/master/src/functions-templates/js/hasura-event-triggered)

and you can write and host your own in your own git repo.

When you are ready to deploy your functions to production, use:

```
ntl deploy --prod
```

For built in authentication, check [netlify-identity-widget and its alternatives](https://github.com/netlify/netlify-identity-widget#list-of-alternatives).
