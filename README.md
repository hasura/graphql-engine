# Web notifications using Hasura GraphQL Engine Event Triggers and FCM

Visit https://shahidhk.github.io/hasura-web-push-notifs for a live demo.

## Pre-requisites

- Google Cloud account with billing enabled
- Firebase Cloud Messaging (FCM) account
- Hasura GraphQL Engine (HGE) installation

## Instructions

### Step 1: Create table

Goto HGE console and create the following table:

```
Table name: message

Columns:

id            UUID        default: gen_random_uuid()
timestamp     Timestamp   default: now()
title         Text
body          Text        nullable
device_token  Text
```

### Step 2: Setup FCM

- Create a new project or use an exisiting project on [Firebase
  Console](https://console.firebase.google.com).
- Note down the Google Cloud project name is also called `PROJECT_ID`.
- Open the [Cloud
  Messaging](https://console.firebase.google.com/project/_/settings/cloudmessaging/)
  tab of the Firebase console Settings pane and scroll to the Web configuration
  section.
- In the Web Push certificates tab, click Generate Key Pair. The console
  displays a notice that the key pair was generated, and displays the public key
  string and date added. 
- Copy the Key Pair (we'll call it `FCM_PUBLIC_KEY`).
- Note down the Sender ID from top of this page (`SENDER_ID`);
- Also note down the Server Key (`SERVER_KEY`).

### Step 3: Deploy Google Cloud Function and setup trigger

We'll configure HGE to send an event to a webhook trigger whenever an insert
happens on the `message` table. The data inserted to this table contains the
notification text and a unique device token. The webhook receives this data and
makes an API call to Firebase to send a push notification to that unique device.

We'll deploy this webhook as a Google Cloud Function.

- Execute the command to deploy the function:
  ```
  cd cloudfunction
  gcloud beta functions deploy push-notification \
         --runtime nodejs8 --trigger-http \
         --set-env-vars="FCM_SERVER_KEY=<SERVER_KEY>"
  ```
- Copy URL from the output


## Architecture
![architecture diagram](arch.png)
