# NodeJS Echo

This is an example which echoes the event back, written in NodeJS.

### Create a sample table

Visit the GraphQL Engine URL to open console.

Goto `Data` tab.

Create the following table:

```
Table name: notes

Columns:

id     Integer auto-increment
note   Text
```

### Deploy the function


# Setup echo serverless function
1. cd echo
2. edit now.json to change app and alias name (Note: alias will be the domain for example setting alias to hge-events-zeit-node-echo will provide hge-events-zeit-node-echo.now.sh)
3. Deploy the function by running `now && now alias && now remove <app-name> --safe -y`

# Add events in Hasura GraphQL

1. In events tab, add a trigger
2. Select all insert, update, delete operations for the trigger.
3. Paste the API endpoint as the webhook.
