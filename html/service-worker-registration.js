if ('serviceWorker' in navigator) {
  navigator.serviceWorker.register('/service-worker.js').then(function(registration) {
    // Registration was successful
    console.log('ServiceWorker registration successful with scope: ',    registration.scope);
    registration.pushManager.subscribe({
         userVisibleOnly: true
    }).then(function(sub) {
        console.log('endpoint:', sub.endpoint);
    });
  }).catch(function(err) {
    // registration failed
    console.log('ServiceWorker registration failed: ', err);
  });
}
