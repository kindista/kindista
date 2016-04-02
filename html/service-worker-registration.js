if ('serviceWorker' in navigator) {
  navigator.serviceWorker.register('/service-worker.js').then(function(registration) {
   // Registration was successful
   // console.log('ServiceWorker registration successful with scope: ',    registration.scope);
  }).catch(function(err) {
    // Service Worker registration failed
    console.error('ServiceWorker registration failed: ', err);
   })
}

window.addEventListener('load', function() {
  if (!('serviceWorker' in navigator)) {
    return;
  }
  // unregister the browser from push notifications when logging out
  logout = document.getElementById('logout');
  if (logout) {
    logout.addEventListener('click', function() {
      navigator.serviceWorker.ready.then(function(serviceWorkerRegistration) {
        serviceWorkerRegistration.pushManager.getSubscription().then(function(pushSubscription) {
          if (pushSubscription){
            pushSubscription.unsubscribe({userVisibleOnly: true}).then(function(successful) {}
          ).catch(function(err) {
            console.error('Error thrown while unsubscribing from push messaging.',err);
          })
       }})
     })
   })
  }
});

window.addEventListener('load', function() {
  if (!('serviceWorker' in navigator)) {
    return;
  }
  var homepage = document.getElementsByClassName('right home');
  if (homepage[0]) {
    if(!('showNotification' in ServiceWorkerRegistration.prototype)) {
      return;
    }
      if (Notification.permission === 'granted') {
       // notification permission already granted
       // subscribe on the device and
       // update the server subscription if already allowed
       navigator.serviceWorker.ready.then(function(serviceWorkerRegistration) {
         serviceWorkerRegistration.pushManager.subscribe({userVisibleOnly: true})
         .then(function(subscription) {
           var subscriptionJSON = subscription.toJSON();
           subscriptionJSON.action = 'update';
           fetch('/push-notification-subscription', {
             method: 'post',
             credentials: 'include',
             headers: {
               'Accept': 'application/json',
               'Content-Type': 'application/json'
             },
             body: JSON.stringify(subscriptionJSON)
             })
           .then(function(response) {
             if (response.status !== 200) {
               console.error('There was a problem sending subscription to server: ' + response.status);
               throw new Error();
             }
             return response.json().then(function(data) {
               var subscriptionStatus = data.subscriptionStatus;
               if (subscriptionStatus === 'unsubscribed') {
                 subscription.unsubscribe({userVisibleOnly: true}).then(function(successful) {
                 }).catch(function(err) {
                    console.error('Failure to unsubscribe: ', err);
                 });
               }
             })
         .catch(function(err) {
           console.error('Unabe to subscribe to push. ', err);
         })
       });
      });
    })
  }
}});
