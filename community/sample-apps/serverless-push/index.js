// Let's check if the browser supports notifications
if (!("Notification" in window)) {
  alert("This browser does not support notifications");
}

const messaging = firebase.messaging();
const screens = ['#loading-screen', '#permission-screen', '#input-screen', '#waiting-screen'];

// Replace with HGE_URL
const HGE_URL = '/v1/graphql';

function showScreen(name) {
  for (screen of screens) {
    screen === name ? $(screen).show() : $(screen).hide();
  }
}

function saveToken(token) {
  window.localStorage.setItem('token', token);
}

function getTokenFromLocal() {
  return window.localStorage.getItem('token');
}

function requestPermission() {
  showScreen('#loading-screen');
  messaging.requestPermission().then(function() {
    console.log('Notification permission granted.');
    $('#loading-text').html('📱 Registering device...');
    getToken();
  }).catch(function(err) {
    console.log('Unable to get permission to notify.', err);
    $('#loading-text').html('😥 Notification permission denied!');
  });
}

function getToken() {
  // Get Instance ID token. Initially this makes a network call, once retrieved
  // subsequent calls to getToken will return from cache.
  messaging.getToken().then(function(currentToken) {
    if (currentToken) {
      console.log('got instance ID token');
      saveToken(currentToken);
      showScreen('#input-screen');
      $('#text-input').focus();
    } else {
      // Show permission request.
      console.log('No Instance ID token available. Request permission to generate one.');
      // Show permission UI.
      showScreen('#permission-screen');
    }
  }).catch(function(err) {
    console.error('An error occurred while retrieving token. ', err);
  });
}


function notify(title) {
  var notification = new Notification(title);
  $('#waiting-ui').hide();
  $('.notifications').show();
  $('#notifications-list').append(`<li class="list-group-item list-group-item-light">${title}</li>`);
}

function tryAgain() {
  resetSendButton();
  showScreen('#input-screen');
  $('#text-input').focus();
}

// Handle incoming messages. Called when:
// - a message is received while the app has focus
// - the user clicks on an app notification created by a service worker
//   `messaging.setBackgroundMessageHandler` handler.
messaging.onMessage(function(payload) {
  console.log('Message received. ', payload);
  notify(payload.notification.title);
});

function resetSendButton() {
  $('#title-submit').html('🔔 Send');
  $('#text-input').val('');
}

function submitText() {
  var textInput = $('#text-input').val();
  if (!textInput) {
    $('#title-submit').html('✋ Type something and click again');
    return;
  }
  $('#title-submit').html('Sending...');
  const r = new Request(HGE_URL);
  const o = {
    method: 'POST',
    body: JSON.stringify({
      query: `
        mutation sendNotification($token: String!, $title: String!, $body: String) {
          insert_message(
            objects:{
              device_token: $token,
            	title: $title,
            	body: $body
          	},
          ) { affected_rows }
        }
      `,
      variables: {title: textInput, token: getTokenFromLocal()}
    })
  };
  fetch(r, o).then(function(response) {
    if (response.status === 200) {
      console.log('request sent to server');
      showScreen('#waiting-screen');
      $('#waiting-ui').show();
    } else {
      console.error('An error happened while sending the request', response.statusText);
      $('#error-text').html('An error happened, check console for details');
    }
  }).catch(function(err) {
    console.error('An error occured while sending the request', err);
    $('#error-text').html('An error happened, check console for details');
  });
}

$( document ).ready(function() {
  navigator.serviceWorker.register('firebase-messaging-sw.js')
    .then((registration) => {
      messaging.useServiceWorker(registration);
      // Replace with FCM_PUBLIC_KEY
      messaging.usePublicVapidKey('BOfaiApSMLjFZVYX_s4gRqb3LbrKcwtbv4qmIYtdZtE6UskL3rxCqBa5hCLhVXjFYsTmA6M8eW-aFZpTQa4B80E');
      // Callback fired if Instance ID token is updated.
      messaging.onTokenRefresh(function() {
        messaging.getToken().then(function(refreshedToken) {
          console.log('Token refreshed.');
          saveToken(refreshedToken);
        }).catch(function(err) {
          console.error('Unable to retrieve refreshed token ', err);
        });
      });
      getToken();
    }).catch(function(err) {
      console.error('An error occurred while registering service worker. ', err);
    });

  $('#text-input').on('keyup', function (e) {
    if (e.keyCode == 13) {
      submitText();
    }
  });

  $('#hge-console-link').attr('href', HGE_URL.replace('v1/graphql', 'console'));
});
