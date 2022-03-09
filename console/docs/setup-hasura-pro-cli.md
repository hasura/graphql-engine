# Steps to setup Hasura PRO cnsole with Hasura PRO CLI development environment

### Pre-requisites

- [Hasura CLI](https://hasura.io/docs/latest/graphql/core/hasura-cli/install-hasura-cli.html)
- Node JS
- Acccess to self hosted or cloud hosted lux(Hasura Cloud)
- [Hasura PRO plugin](https://hasura.io/docs/latest/graphql/cloud/hasurapro-cli/index.html)

### Hasura CLI setup

1. Login to Hasura cloud and create a self hosted project.(cloud project can also be used but self hosted project will give felxibility to test custom server versions)
2. Generate a PAT from the settings page.
3. Open terminal on a new directory
4. execute `hasura init`
5. This will scaffold the minimum files that are required to run the hasura CLI
6. if you are using a local/ custom lux setup add the pro configuration as mentioned below.

```yaml
version: 3
endpoint: http://localhost:8080
metadata_directory: metadata
admin_secret: randompassword
actions:
  kind: synchronous
  handler_webhook_baseurl: http://localhost:3000
pro:
  metrics_server_endpoint: https://metrics.pro.arusah.com
  data_server_endpoint: https://data.pro.arusah.com
  oauth_server_endpoint: https://oauth.pro.arusah.com
```

7. Execute `hasura pro login` and login to your hasura account with the PAT
8. Execute `hasura pro console --use-server-assets`, this will ask for you to select the projectand it will start hasura console on the port 9695 on successfull authentication.

### Setup Hasura PRO Console on CLI mode

1. Checkout the PRO console code
2. execute `npm install`
3. create a file `.env` as mentioned in the main readme file. an example env file is shown below.

```
PORT=3000
NODE_ENV=development
DATA_API_URL=http://localhost:8080
API_HOST=http://localhost
API_PORT=9693
CONSOLE_MODE=cli
IS_ADMIN_SECRET_SET=true
ADMIN_SECRET=randompassword
IS_ACCESS_KEY_SET=false
URL_PREFIX=/console
ASSETS_PATH=https://graphql-engine-cdn.hasura.io/console/assets
# to run against staging
HASURA_OAUTH_URL='https://oauth.pro.arusah.com'
HASURA_METRICS_URL='https://metrics.pro.arusah.com'
# to run against local lux
# HASURA_OAUTH_URL=http://oauth.lux-dev.hasura.me
# HASURA_METRICS_URL=http://metrics.lux-dev.hasura.me
HASURA_OAUTH_SCOPES=openid
IS_PRO=true
// against staging
HASURA_CLOUD_ROOT_DOMAIN='pro.arusah.com'
PROJECT_NAME="test"
HASURA_CLIENT_ID='d74ee3cf-1785-4aaf-bfc2-7356f661220e_console'
PROJECT_ID='d74ee3cf-1785-4aaf-bfc2-7356f661220e'
```

> Note: HASURA_CLIENT ID and PROJECT_ID needs to be changed from the base64 decoded PRO key generated from the Hasura cloud project dashboard.

4. execute `npm run dev`

This should open up hasura console on http://localhost:3000

> Note: even if you use PAT on the hasura CLI, it won't be passed on to the console. console will ask PAT through the UI again.
