# Setup table

Create the table using the console:

```
Table name: profile

Columns:

id: Integer auto-increment
name: Text
address: Text
lat: Numeric, Nullable
lng: Numeric, Nullable
```

# Deploy Google Cloud function

We are going to use Google Maps API in our cloud function.

Enable Google Maps API and get an API key (we'll call it `GMAPS_API_KEY`) following [this
guide](https://developers.google.com/maps/documentation/geocoding/start?hl=el#auth)

Check the `Places` box to get access to Geocoding API.

We'll follow [this guide](https://cloud.google.com/functions/docs/quickstart)
and create a Cloud Function with NodeJS 6.

```bash
gcloud components update &&
gcloud components install beta
```

Goto the `cloudfunction` directory:

```bash
cd cloudfunction
```

Edit `.env.yaml` and add values for the following as shown:
```yaml
# .env.yaml
GMAPS_API_KEY: '[GMAPS_API_KEY]'
HASURA_GRAPHQL_ENGINE_URL: 'http://[HGE_IP]/v1/graphql'
```

```bash
gcloud beta functions deploy trigger \
       --runtime nodejs6 \
       --trigger-http \
       --region asia-south1 \
       --env-vars-file .env.yaml
```

Get the trigger URL:
```yaml
httpsTrigger:
  url: https://asia-south1-hasura-test.cloudfunctions.net/trigger
```

Goto `HGE_IP` on browser, `Events -> Add Trigger` and create a new trigger:
```
Trigger name: profile_change
Schema/Table: public/profile
Operations: Insert
Webhook URL: [Trigger URL]
```

Once the trigger is created, goto `Data -> profile -> Insert row` and add a new
profile with name and address, save. Goto `Browse rows` tabs to see lat and lng
updated, by the cloud function.
