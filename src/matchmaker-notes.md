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
- :notification
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

## Make sure searching offers/requests respects item-privacy

## Display matching items on both offer and request pages

## Email requestor when new matching offer is made

## Matchmaker should be reindexed with edits
- tags
- change of location (person and group)
- change of privacy
- changes to matching offes

## When user removes their location, delete all matchmakers

## When request is deleted, delete matchmaker

## Show matches on items
- allow admins to see all matches

## Add Inventory Titles
- require titles on new/edited inventory items
- display titles on inventory activity items when present
- index stemmed words in inventory titles
