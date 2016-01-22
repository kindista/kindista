var CACHE_NAME = 'kindista-cache-v1';
// the files we want to cache
var urlsToCache = [ 
    '/error.html',
];

// set the callback for the install step
self.addEventListener('install', function(event) {
    //perform install steps
    event.waitUntil(
        caches.open(CACHE_NAME)
        .then(function(cache) {
            console.log('Opened cache');
            return cache.addAll(urlsToCache);
        })
    );
});

self.addEventListener('fetch', function(event) {
  event.respondWith(
    caches.match(event.request)
      .then(function(response) {
        // Cache hit - return response
        if (response) {
          return response;
        }
        return fetch(event.request);
      }
    )
  );
});