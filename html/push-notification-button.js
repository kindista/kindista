var isPushEnabled = false;

window.addEventListener('load', function() {
  var pushButton = document.querySelector('.push-notification-button');
  pushButton.addEventListener('click', function() {
    if(isPushEnabled) {
      unsubscribe();
    } else {
      subscribe();
    }
  });
  if ('serviceWorker' in navigator) {
    initialiseState();
  } else {
    console.warn('service workers aren\'t supported in this browser.');
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
   // add reload for flash response??
   //location.reload(true);
   //console.log(response);
     if (response.status !== 204) {
       console.log('There was a problem sending subscription to server: ' + response.status);
       throw new Error();
     }
  }).catch(function(err) {
       console.log('error sending subscription to server', err);
    })
}

function initialiseState() {
  if(!('showNotification' in ServiceWorkerRegistration.prototype)) {
    console.warn('Notifications arent\'t supported.');
    return;
  }

  if (Notification.permission === 'denied') {
    consle.warn('The user has blocked notifications.');
    return;
  }

  if (!('PushManager' in window)) {
    console.warn('Push messaging isn\'t supported.');
    return;
  }

   navigator.serviceWorker.ready.then(function(serviceWorkerRegistration) {
   serviceWorkerRegistration.pushManager.getSubscription().then(function(subscription) {
     var pushButton = document.querySelector('.push-notification-button');
     pushButton.disabled = false;
    
     if(!subscription) {
       return;
    }
    // keep subscription up to date
    sendSubscriptionToServer(subscription, "subscribe");

    pushButton.textContent = 'Disable Push Messages';
    isPushEnabled = true;
  })
    .catch(function(err) {
       console.warn('Error during getSubscription()', err);
    });
  });
}

function subscribe() {
  var pushButton = document.querySelector('.push-notification-button');
  pushButton.disabled = true;

  navigator.serviceWorker.ready.then(function(serviceWorkerRegistration) {
    serviceWorkerRegistration.pushManager.subscribe({userVisibleOnly: true}) 
      .then(function(subscription) {
          // successful subscription
          isPushEnabled = true;
          pushButton.textContent = 'Disable Push Messages';
          pushButton.disabled = false;

          // create sendSubscriptionToServer function
          sendSubscriptionToServer(subscription,"subscribe");
        })
        .catch(function(err) {
          if (Notification.permission === 'denied') {
           // user denied notification permission
           // user must manually change browser notification permissons
           // in order to subscribe.
           console.warn('Permission for Notifications was denied');
           pushButton.disabled = true;
        } else {
          console.error('Unable to subscribe to push.', err);
          pushButton.disabled = false;
          pushButton.textContent = 'Enable Push Messages';
        }
     });
  });
}

function unsubscribe() {
  var pushButton = document.querySelector('.push-notification-button');
  pushButton.disabled = true;

  navigator.serviceWorker.ready.then(function(serviceWorkerRegistration) {
    // get the subscription object
    serviceWorkerRegistration.pushManager.getSubscription().then(function(pushSubscription) {
     if (!pushSubscription) {
        // no subscription allow user to subscribe to push
        isPushEnabled = false;
        pushButton.disabled = false;
        psuhButton.textContent = 'Enable Push Messages';
        return;
      }
     var subscriptionId = pushSubscription.endpoint;
     // make a server request to remove the subscription id from database
     sendSubscriptionToServer(pushSubscription, "unsubscribe");
     pushSubscription.unsubscribe({userVisibleOnly: true}).then(function(successful) {
        pushButton.disabled = false;
        pushButton.textContent = 'Enable Push Messages';
        isPushEnabled = false;
    }).catch(function(err) {
        //failed to unsubscribe
        // remove users subscription from database

        console.log('Unsubscription error: ', err);
        pushButton.disabled = false;
        pushButton.textContent = 'Enable Push Messages';
    });
  }).catch(function(err) {
    console.error('Error thrown while unsubscribing from push messaging.', err);
  });
});
}
