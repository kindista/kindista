self.addEventListener('install', function(event) {
  //return self.skipWaiting() // updates service worker, remove for production
  console.log('Installed', event);
});

self.addEventListener('activate', function(event) {
  console.log('Activated', event);
});

self.addEventListener('push', function(event) {
  event.waitUntil(
    self.registration.pushManager.getSubscription().then(function(subscription) {
    return fetch('/unread-push-notifications', {
      method: 'post',
      credentials: 'include',
      headers: {
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
        var title = data.title;
        var body = data.body;
        var icon = data.icon;
        var notificationData = data;
        var notificationTag = 'new-message-tag';
        var notificationFilter = {
          tag: notificationTag
      };
      return self.registration.getNotifications(notificationFilter)
        .then(function(notifications) {
          if (notifications && notifications.length > 0) {
            //start with one to account for the new notification
            var notificationCount = 1;
            for (var i = 0; i < notifications.length; i++) {
              var existingNotification = notifications[i];
              if (existingNotification.data && existingNotification.data.notificationCount) {
                notificationCount += existingNotification.data.notificationCount;
              } else {
                notificationCount++;
              }
              existingNotification.close();
            }
            body = 'You have ' + notificationCount + ' new messsages waiting for you on Kindista.';
            notificationData.notificationCount = notificationCount;
          }
        return self.registration.showNotification(title, {
                                 body: body,
                                 icon: icon ? icon : 'kindista_favicon_180.png',
                                 tag: 'new-message-tag',
                                 data: data});
      });
    });
  }).catch(function(err) {
      console.error('Unable to retrieve data', err);
      var title = 'You have a new message';
      var body = 'A new message is waiting for you in your Kindista inbox.';
      var icon = 'kindista_favicon_180.png';
      var notificationTag = 'notification-error';
      return self.registration.showNotification(title, {
        body: body,
        icon: icon,
        tag: notificationTag
        });
      })
  }));
});

self.addEventListener('notificationclick', function(event) {
  // Ensures the notification closes
  event.notification.close();
  // Check if message page is open, if so focus on it
  event.waitUntil(
    clients.matchAll({
      type: "window"
    })
    .then(function(clientList) {
      for (var i = 0; i < clientList.length; i++) {
        var client = clientList[i];
        if (client.url == event.notification.data.url && 'focus' in client){
          return client.focus();
        }
      }
      if (clients.openWindow) {
        return clients.openWindow(event.notification.data.url);
      }
    })
  );
});
