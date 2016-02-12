// set the callback for the install step
self.addEventListener('install', function(event) {
    //perform install steps
 return self.skipWaiting() // updates service worker, remove for production
  console.log('Installed', event);
});
self.addEventListener('activate', function(event) {
  console.log('Activated', event);
});
self.addEventListener('push', function(event) {
  event.waitUntil(
    self.registration.pushManager.getSubscription().then(function(subscription) {
     fetch('/send-push-notification', {
     method: 'post',
     headers: {
        //authorization ?
        'Accept': 'application/json', 
        'Content-Type': 'application/json'
     },
     body: JSON.stringify(subscription)
      })
    .then(function(response) {
      if (response.status !== 200) {
        // handle error
        console.log('There was a problem. Status Code: ' + response.status);
        throw new Error();
      }
      // examine text
      return response.json().then(function(data) {
        //if (data.error || !data.notification) {
        //  console.error('The API returned an error.', data.error);
        //  throw new Error();
       // }
        var title = data.title;
        var body = data.body;
        var icon = data.icon;
        var tag = data.tag;
      return self.registration.showNotification(title, {
        body: body,
        icon: icon,
        tag: tag
      });
    });
  }).catch(function(err) {
      // console.error('Unable to retrieve data', err);
       var title = 'You have new message';
       var body = 'A new message is waiting for you in your Kindista inbox.';
       var icon = 'kindista_favicon_180.png';
       var notificationTag = 'notification-error';
       var url = '/messages';
       return self.registration.showNotification(title, {
          body: body,
          icon: icon,
          tag: notificationTag
         });
       })
   })); 
});
self.addEventListener('notificationclick', function(event) {
  console.log('On notification click: ', event.notification.tag); 
  //close the notification when you click on it
  event.notification.close();

  // Check if page is open, if so focus on it
  event.waitUntil(
    clients.matchAll({
      type: "window"
    })
    .then(function(clientList) {
      for (var i = 0; i < clientList.length; i++) {
        var client = clientList[i];
        if (client.url == event.notification.url && 'focus' in client){
            console.log('focus');
            return client.focus();
        }
      }
      if (clients.openWindow) {
        console.log('new window');
        return clients.openWindow(event.notification.url);
      } 
    })
  );
});
