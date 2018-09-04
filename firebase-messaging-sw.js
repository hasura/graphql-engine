// Give the service worker access to Firebase Messaging.
// Note that you can only use Firebase Messaging here, other Firebase libraries
// are not available in the service worker.
importScripts('https://www.gstatic.com/firebasejs/5.4.2/firebase-app.js');
importScripts('https://www.gstatic.com/firebasejs/5.4.2/firebase-messaging.js');

// Initialize the Firebase app in the service worker by passing in the
// messagingSenderId.
firebase.initializeApp({
  'messagingSenderId': '795709254723',
});

// Retrieve an instance of Firebase Messaging so that it can handle background
// messages.
const messaging = firebase.messaging();

messaging.setBackgroundMessageHandler(function(payload) {
  console.log('[firebase-messaging-sw.js] Received background message ', payload);
  // Customize notification here
  var notificationTitle = 'Notification Title';
  var notificationOptions = {
    body: 'Notification body.',
    icon: '/favicon.png'
  };

  return self.registration.showNotification(notificationTitle,
                                            notificationOptions);
});
