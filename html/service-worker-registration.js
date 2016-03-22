if ('serviceWorker' in navigator) {
  navigator.serviceWorker.register('/service-worker.js').then(function(registration) {
    // Registration was successful
    console.log('ServiceWorker registration successful with scope: ',    registration.scope);
    //registration.pushManager.subscribe({
    //    userVisibleOnly: true
   // }).then(function(sub) {
   //     console.log('endpoint:', sub.endpoint);
        //send sub.endpoint 
   // })
    //.catch(function(err){
     //push subscription failed
     // if (Notification.permission == 'denied') {
      //  console.warn('Permission for Notifications was denied');
       //disable push button 
     // } else {
     //   console.error('Unable to subscribe to push.' , err); 
     // }
  }).catch(function(err) {
    // Service Worker registration failed
    console.error('ServiceWorker registration failed: ', err);
   })
 // });
}

window.addEventListener('load', function() {
  logout = document.getElementById('logout');
  if (logout) {
    logout.addEventListener('click', function() {
      navigator.serviceWorker.ready.then(function(serviceWorkerRegistration) {
        serviceWorkerRegistration.pushManager.getSubscription().then(function(pushSubscription) {
         if (pushSubscription){
           console.log(pushSubscription);
           pushSubscription.unsubscribe({userVisibleOnly: true}).then(function(successful) {}
          ).catch(function(err) {
            console.error('Error thrown while unsubscribing from push messaging.',err);
           })
        }})
      })
    })
  }
});

function sendSubscriptionToServer(subscription, action) {
  var subscriptionJSON = subscription.toJSON();
  subscriptionJSON.action = action;
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
      console.log('There was a problem sending subscription to server: ' + response.status);
        throw new Error();
      }
    return response.json().then(function(data) {
      var stat = data.stat;
      console.log(stat);
    }); 
  }).catch(function(err) {
      console.log('Error sending suscription to server', err);
  });
}

window.addEventListener('load', function() {
  var foo = document.getElementsByClassName('right home');
  console.log(foo[0]);
  login = document.getElementById('login');
  if (foo[0]) {
    if(!('showNotification' in ServiceWorkerRegistration.prototype)) {
      return
    }
      if (Notification.permission === 'granted') {
       // notification permission already granted
       // subscribe on the device and 
       // update the server subscription if already allowed
       console.log("permision granted");
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
                console.log('There was a problem sending subscription to server: ' + response.status);
                  throw new Error();
                }

              return response.json().then(function(data) {
                console.log(data);
                var stat = data.stat;
                console.log(stat);
                if (stat === 'unsubscribed') {
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
