---
title: "Write event webhook"
metaTitle: "Write event webhook | Hasura GraphQL Tutorial"
metaDescription: "In this part, we will look at how to write an event webhook and trigger them asynchronously after a mutation operation."
---

import YoutubeEmbed from "../../src/YoutubeEmbed.js";

<YoutubeEmbed link="https://www.youtube.com/embed/_6Sc5emXq5U" />

Now let's move to the second use-case of sending an email when a user registers on the app.

When the user registers on the app using Auth0, we insert a new row into the `users` table to keep the user data in sync. Remember the Auth0 rule we wrote during signup to make a mutation?

This is an `insert` operation on table `users`.
The payload for each event is mentioned [here](https://docs.hasura.io/1.0/graphql/manual/event-triggers/payload.html#json-payload)

Now we are going to capture this insert operation to trigger our event.

## SendGrid SMTP Email API

For this example, we are going to make use of `SendGrid`'s SMTP server and use `nodemailer` to send the email.

Signup on [SendGrid](https://sendgrid.com/) and create a free account.

Create an API Key by following the docs [here](https://sendgrid.com/docs/for-developers/sending-email/integrating-with-the-smtp-api/)

### Write the webhook

```javascript
const nodemailer = require('nodemailer');
const transporter = nodemailer.createTransport('smtp://'+process.env.SMTP_LOGIN+':'+process.env.SMTP_PASSWORD+'@' + process.env.SMTP_HOST);
const fs = require('fs');
const path = require('path');
const express = require('express');
const bodyParser = require('body-parser');
const app = express();

app.set('port', (process.env.PORT || 3000));

app.use('/', express.static(path.join(__dirname, 'public')));
app.use(bodyParser.json());
app.use(bodyParser.urlencoded({extended: true}));

app.use(function(req, res, next) {
    res.setHeader('Access-Control-Allow-Origin', '*');
    res.setHeader('Cache-Control', 'no-cache');
    next();
});

app.post('/send-email', function(req, res) {
  
  const name = req.body.event.data.new.name;
  // setup e-mail data
  const mailOptions = {
      from: process.env.SENDER_ADDRESS, // sender address
      to: process.env.RECEIVER_ADDRESS, // list of receivers
      subject: 'A new user has registered', // Subject line
      text: 'Hi, This is to notify that a new user has registered under the name of ' + name, // plaintext body
      html: '<p>'+'Hi, This is to notify that a new user has registered under the name of ' + name + '</p>' // html body
  };
  // send mail with defined transport object
  transporter.sendMail(mailOptions, function(error, info){
      if(error){
          return console.log(error);
      }
      console.log('Message sent: ' + info.response);
      res.json({'success': true});
  });
  
});

app.listen(app.get('port'), function() {
  console.log('Server started on: ' + app.get('port'));
});
```

## Deploy 

[![DEPLOY TO GLITCH](https://raw.githubusercontent.com/hasura/graphql-engine/master/community/boilerplates/auth-webhooks/nodejs-express/assets/deploy-glitch.png)](https://glitch.com/~sendgrid-send-email-event)

## Environment variables
After remixing to your own project on Glitch, modify the `.env` file to enter the 
- `SMTP_LOGIN`, 
- `SMTP_PASSWORD`, 
- `SMTP_HOST` 

values appropriately.

Additionally you should also configure the sender and receiver address using 
- `SENDER_ADDRESS` 
- `RECEIVER_ADDRESS` 

env variables.

Congrats! You have written and deployed your first webhook to handle database events.
