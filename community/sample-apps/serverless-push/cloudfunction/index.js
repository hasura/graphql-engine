const fetch = require('node-fetch');

exports.function = async (req, res) => {
  // webhook payload
  const { event: { op, data }, table: { schema, name } } = req.body;

  // env vars
  const FCM_SERVER_KEY = process.env.FCM_SERVER_KEY;

  if (op === 'INSERT' && name === 'message') {
    // get the message title and body
    const { device_token, title, body }= data.new;

    // send a notification using the token
    const fcmResponse = await fetch('https://fcm.googleapis.com/fcm/send', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        Authorization: `key=${FCM_SERVER_KEY}`,
      },
      body: JSON.stringify({
        to: device_token,
        notification: { title, body },
      }),
    });
    const fcmResponseJSON = await fcmResponse.json();
    res.json({error: false, data: fcmResponseJSON});
  } else {
    // ignore if the trigger name is not matched
    res.json({error: false, data: {message: 'ignored event'}});
  }
};
