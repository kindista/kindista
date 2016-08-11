if ('serviceWorker' in navigator) {
  navigator.serviceWorker.register('/service-worker.js?v=1.1').then(function(registration) {
   // Registration was successful
    console.log('ServiceWorker registration successful with scope: ',    registration.scope);
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
            console.error('Error while unsubscribing from push notifications.',err);
          })
       }})
     })
   })
  }
});

window.onerror = function(msg, url, lineNo, colNo, error) {
  var errorJSON = {"msg":msg,
                   "url":url,
                   "lineNo":lineNo,
                   "colNo":colNo,
                   "stack":error.stack};
  fetch('/client-side-error-logger', {
    method: 'post',
    credentials: 'include',
    headers: {
      'Accept': 'application/json',
      'Content-Type': 'application/json'
    },
    body: JSON.stringify(errorJSON)
  })
}
