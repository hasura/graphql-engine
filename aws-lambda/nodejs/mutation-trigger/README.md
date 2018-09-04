# Setup tables
1. Create the following tables:

```sql

CREATE TABLE notes (
  id SERIAL PRIMARY KEY,
  note TEXT
);

CREATE TABLE note_revision (
  id SERIAL PRIMARY KEY,
  note_id INT REFERENCES notes (id),
  note TEXT,
  updated_at TIMESTAMP DEFAULT now()
);
```

# Setup AWS Lambda
Create a lambda function in AWS. This will be our webhook.

1. Create a function.
2. Select Node.js 6 as the runtime.
3. Select "start from scratch".
4. Add API gateway as a trigger.
5. Add an API to API gateway.
6. Add the code in `index.js` and save. The handler function of your lambda will be the `index.handler`.
7. Add the following enviroment variables in your lambda config:
  i) `ACCESS_KEY`: this is the access key you configured when you setup HGE.
  ii) `HGE_ENDPOINT`: the URL on which you HGE instance is running.

# Add the trigger in Hasura GraphQL
1. In events tab, add a trigger
2. Select all insert, update, delete operations for the trigger.
3. Paste the API endpoint of your AWS lambda as the webhook.
