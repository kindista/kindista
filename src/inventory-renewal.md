## New UI selector for new/edit inventory item
   -  default expiration and options

## Index to store expiration dates
   - maybe share with invitation expirations?

## Keep track of when items expire
   Options:
   - a new thread with an infinite loop checking the index
     - start tread when starting kindista
   - another cronjob sending an http request to trigger checking which should expire

## Send email notification about inventory expiration
   - *production-p* only!

## Deactivate the inventory item

## Reactivate the inventory item with new time

## Testing
   - quickload master
   - run
   - quickload inventory-renewal
   - load-db
   - start inventory-expiration-loop
