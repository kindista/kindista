## Add attributes to Request db items: DONE
- :notify-matches t/nil

- :match-tags
- :match-all-terms
- :match-any-terms
- :match-no-terms
- :match-distance


## Matchmaker-requests struct DONE
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

## Index DONE
- *matchmaker-requests-geo-index*
- *global-matchmaker-requests-index*

## Search offers DONE
- geo-index search
- variety of stem-index searches
- add matching offers to (db request :matching-offers)
- (push matching-request-result (gethash offer-id
  *offers-with-matching-requests-index*))

## Search requests when new offers are posted DONE
Search:
- *matchmaker-requests-geo-index*
- *global-matchmaker-requests-index*

## Make sure searching offers/requests respects item-privacy DONE

## Display matching items on both offer and request pages DONE

### Email requestor when new matching offer is made

## Matchmaker should be reindexed with edits DONE
- tags DONE
- change of location (person and group) DONE
- change of privacy DONE
- changes to matching offers DONE

## When request is deleted, delete matchmaker DONE

## When offer is deleted, remove from matching requests DONE

## Show number of matches below all of my items DONE

### Show matches on items
- allow admins to see all matches
- list of requests without matchmakers

## Add Inventory Titles
- require titles on new/edited inventory items
- display titles on inventory activity items when present
- index stemmed words in inventory titles

# Push to server
- DATA MIGRATION OF TOKENS to allow for new token structure
