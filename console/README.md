## Usage of Environment Variables

This app uses a few environment variables which are required for development. The production build uses values directly injected by the server serving this app.

We use [dotenv](https://github.com/motdotla/dotenv) for setting environment variables for development. Create a `.env' file in the root directory (wherever package.json is) and set the following values. Replace accordingly for testing.

```
PORT=3000
NODE_ENV=development
DATA_API_URL=http://localhost:9000
API_HOST=http://localhost
API_PORT=9693
ACCESS_KEY=abcd
CONSOLE_MODE=cli
DEV_DATA_API_URL=http://localhost:9000
URL_PREFIX=/
```

**Note**
The .env file should not be in version control.

## Contributing

To suggest a feature, create an issue if it does not already exist. If you would like to help develop a suggested feature follow these steps:

- Fork this repo
- Install dependencies with $ npm install
- Implement your source code changes to files in the src/ directory
- Write your tests in the cypress/ directory
- Run the hasura console production server by executing `hasura console`
- Run the hasura console locally with $ npm run dev
- Submit PR for review
