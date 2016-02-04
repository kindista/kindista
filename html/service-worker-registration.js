if ('serviceWorker' in navigator) {
  navigator.serviceWorker.register('/service-worker.js').then(function(registration) {
    // Registration was successful
    console.log('ServiceWorker registration successful with scope: ',    registration.scope);
    registration.pushManager.subscribe({
         userVisibleOnly: true
    }).then(function(sub) {
        console.log('endpoint:', sub.endpoint);
        //send sub.endpoint 
    })
    .catch(function(err){
     //push subscription failed
      if (Notification.permission == 'denied') {
       console.warn('Permission for Notifications was denied');
       //disable push button 
      } else {
       console.error('Unable to subscribe to push.' , err); 
      }
  }).catch(function(err) {
    // Service Worker registration failed
    console.error('ServiceWorker registration failed: ', err);
   }
   )
  });
}
