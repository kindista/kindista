## When posting new gratitude
- select whether it's for an existing offer/request or for something not on kindista
- when it's for an existing item, display potential items that could be associated with the gratitude

## Pending gratitude index
- key user/group id
- value: plist (:offers (results) :requests (results))

## features/messages
- when indexing replies, make sure the result is eq, that is don't call make result twice (once in index-message, once in index-inventory) make the result in one function that is called by both index functions. in that new function, check for the result before creating a new one.
