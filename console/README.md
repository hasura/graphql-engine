## Usage of Environment Variables

This app uses a few environment variables which are required for development. The production build uses values directly injected by the server serving this app.

We use [dotenv](https://github.com/motdotla/dotenv) for setting environment variables for development. Create a `.env' file in the root directory (wherever package.json is) and set the following values. Replace accordingly for testing.

```
PORT=3000
NODE_ENV=development
DATA_API_URL=http://localhost:9000
DEV_DATA_API_URL=http://localhost:9000
API_HOST=http://localhost
API_PORT=9693
ACCESS_KEY=abcd
IS_ACCESS_KEY_SET=false
CONSOLE_MODE=cli
URL_PREFIX=/
```

**Note**

- The .env file should not be in version control.
- CONSOLE_MODE can be either 'cli' or 'hasuradb'.
- API_HOST and API_PORT are for contacting hasura cli console server.
- ACCESS_KEY will be set by hasura cli.
- IS_ACCESS_KEY_SET will be set by graphql-engine server.
- DEV_DATA_API_URL is used to simulate cli console, where server can be hosted anywhere.
- URL_PREFIX can be changed to host console on a different base URL path.
-
