# Setup tables
1. Create table:

```
notes:
  id: int
  note: text
```

# Setup Cloud Function
1. Add `netlify.toml` to your Netlify root directory
2. Rename `.netlifygitignore` to `.gitignore` to your Netlify root directory
2. (alt) Alternatively edit your pre-existing `.gitignore` to ignore your functions folder
3. Add the following scripts to your `package.json`:
```json
// functions is the name of your functions folder
// if you are using a different name change it
"scripts": {
  "lambda-serve": "netlify-lambda serve functions",
  "lambda-build": "netlify-lambda build functions"
}
```
4. Add the file `index.js` to your functions folder

# Running locally
`netlify-lambda serve functions`

# Build
`netlify-lambda build functions`

# Add the trigger in Hasura GraphQL
1. In events tab, add a trigger
2. Select all insert, update, delete operations for the trigger.
3. Paste your function URL as the webhook. (eg: http://host.docker.internal:9000/index)
