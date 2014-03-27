## Add attributes to Request db items:
- :notify-matches t/nil
- :match-tags
- :match-all-terms
- :match-any-terms
- :match-no-terms
- :match-distance


## Matchmaker-requests struct
- :id
- :all-terms
- :any-terms
- :without-terms
- :tags
- :distance
- :tags
- :latitude
- :longitude

## Index
- *matchmaker-requests-geo-index*
- *global-matchmaker-requests-index*

## Search offers
- geo-index search
- variety of stem-index searches
- add matching offers to (db request :matching-offers)
- (push matching-request-result (gethash offer-id
  *offers-with-matching-requests-index*))

## Search requests when new offers are posted
Search:
- *matchmaker-requests-geo-index*
- *global-matchmaker-requests-index*

## Display matching items on both offer and request pages


## Add Inventory Titles
- require titles on new/edited inventory items
- display titles on inventory activity items when present
- index stemmed words in inventory titles
