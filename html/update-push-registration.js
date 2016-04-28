window.addEventListener('load', function() {
  if (!('serviceWorker' in navigator)) {
    return;
  }
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
       if (/Android/i.test(navigator.userAgent)) {
         subscriptionJSON.mobile = 'true';
       }
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
           console.error('Problem sending subscription to server: ' + response.status);
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
});
