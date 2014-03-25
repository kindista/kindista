## Add attributes to Request db items:
- :notify-matches t/nil
- :match-tags
- :match-all-terms
- :match-any-terms
- :match-distance

## Index
- *matching-requests-geo-index*
- *matching-requests-stem-index*

## Search offers
- add matching offers to (db request :matching-offers)
- (push matching-request-result (gethash offer-id
  *offers-with-matching-requests-index*))

## Display matching items on both offer and request pages


## Add Inventory Titles
- require titles on new/edited inventory items
- display titles on inventory activity items when present
- index stemmed words in inventory titles
