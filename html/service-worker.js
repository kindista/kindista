// set the callback for the install step
self.addEventListener('install', function(event) {
    //perform install steps
  self.skipWaiting() // updates service worker, remove for production
  console.log('Installed', event);
});
self.addEventListener('activate', function(event) {
  console.log('Activated', event);
});
self.addEventListener('push', function(event) {
  console.log('Push message recieved', event);
  var title = 'Push message';
  event.waitUntil(
    self.registration.showNotification(title, {
      'body': 'The Message',
      'icon': 'kindista_favicon_180.png'
    }));
});
