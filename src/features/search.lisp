;;; Copyright 2012-2013 CommonGoods Network, Inc.
;;;
;;; This file is part of Kindista.
;;;
;;; Kindista is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Affero General Public License as published
;;; by the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Kindista is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public
;;; License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public License
;;; along with Kindista.  If not, see <http://www.gnu.org/licenses/>.

(in-package :kindista)

(defvar *stop-words* (make-hash-table :size 750 :test 'equalp))

(defun add-stop-words (&rest words)
  (dolist (word words)
    (setf (gethash word *stop-words*) nil)))

(defun stop-word-p (word)
  (nth-value 1 (gethash word *stop-words*)))

(add-stop-words
  "a"
  "able"
  "about"
  "above"
  "abroad"
  "according"
  "accordingly"
  "across"
  "actually"
  "adj"
  "after"
  "afterwards"
  "again"
  "against"
  "ago"
  "ahead"
  "ain't"
  "all"
  "allow"
  "allows"
  "almost"
  "alone"
  "along"
  "alongside"
  "already"
  "also"
  "although"
  "always"
  "am"
  "amid"
  "amidst"
  "among"
  "amongst"
  "an"
  "and"
  "another"
  "any"
  "anybody"
  "anyhow"
  "anyone"
  "anything"
  "anyway"
  "anyways"
  "anywhere"
  "apart"
  "appear"
  "appreciate"
  "appropriate"
  "are"
  "aren't"
  "around"
  "as"
  "a's"
  "aside"
  "ask"
  "asking"
  "associated"
  "at"
  "available"
  "away"
  "awfully"
  "back"
  "backward"
  "backwards"
  "be"
  "became"
  "because"
  "become"
  "becomes"
  "becoming"
  "been"
  "before"
  "beforehand"
  "begin"
  "behind"
  "being"
  "believe"
  "below"
  "beside"
  "besides"
  "best"
  "better"
  "between"
  "beyond"
  "both"
  "brief"
  "but"
  "by"
  "came"
  "can"
  "cannot"
  "cant"
  "can't"
  "caption"
  "cause"
  "causes"
  "certain"
  "certainly"
  "changes"
  "clearly"
  "c'mon"
  "co"
  "co."
  "com"
  "come"
  "comes"
  "completely"
  "concerning"
  "consequently"
  "consider"
  "considering"
  "contain"
  "containing"
  "contains"
  "corresponding"
  "could"
  "couldn't"
  "course"
  "c's"
  "currently"
  "dare"
  "daren't"
  "decrease"
  "decreasingly"
  "definitely"
  "described"
  "despite"
  "did"
  "didn't"
  "different"
  "directly"
  "do"
  "does"
  "doesn't"
  "doing"
  "done"
  "don't"
  "down"
  "downwards"
  "during"
  "each"
  "eg"
  "eight"
  "eighty"
  "either"
  "else"
  "elsewhere"
  "end"
  "ending"
  "enough"
  "entirely"
  "especially"
  "et"
  "etc"
  "even"
  "ever"
  "evermore"
  "every"
  "everybody"
  "everyone"
  "everything"
  "everywhere"
  "ex"
  "exactly"
  "example"
  "except"
  "fairly"
  "far"
  "farther"
  "few"
  "fewer"
  "fifth"
  "first"
  "firstly"
  "five"
  "followed"
  "following"
  "follows"
  "for"
  "forever"
  "former"
  "formerly"
  "forth"
  "forward"
  "found"
  "four"
  "from"
  "further"
  "furthermore"
  "get"
  "gets"
  "getting"
  "given"
  "gives"
  "go"
  "goes"
  "going"
  "gone"
  "got"
  "gotten"
  "greetings"
  "had"
  "hadn't"
  "half"
  "happens"
  "hardly"
  "has"
  "hasn't"
  "have"
  "haven't"
  "having"
  "he"
  "he'd"
  "he'll"
  "hello"
  "help"
  "hence"
  "her"
  "here"
  "hereafter"
  "hereby"
  "herein"
  "here's"
  "hereupon"
  "hers"
  "herself"
  "he's"
  "hi"
  "him"
  "himself"
  "his"
  "hither"
  "hopefully"
  "how"
  "howbeit"
  "however"
  "hundred"
  "i"
  "i'd"
  "ie"
  "if"
  "ignored"
  "i'll"
  "i'm"
  "immediate"
  "in"
  "inasmuch"
  "inc"
  "increase"
  "increasingly"
  "indeed"
  "indicate"
  "indicated"
  "indicates"
  "inner"
  "inside"
  "insofar"
  "instead"
  "into"
  "inward"
  "is"
  "isn't"
  "it"
  "it'd"
  "it'll"
  "its"
  "it's"
  "itself"
  "i've"
  "just"
  "keep"
  "keeps"
  "kept"
  "know"
  "known"
  "knows"
  "last"
  "lastly"
  "lately"
  "later"
  "latter"
  "latterly"
  "least"
  "less"
  "lest"
  "let"
  "let's"
  "like"
  "liked"
  "likely"
  "likewise"
  "little"
  "look"
  "looking"
  "looks"
  "low"
  "lower"
  "ltd"
  "made"
  "main"
  "mainly"
  "make"
  "makes"
  "many"
  "may"
  "maybe"
  "mayn't"
  "me"
  "mean"
  "meantime"
  "meanwhile"
  "merely"
  "might"
  "mightn't"
  "mine"
  "minus"
  "miss"
  "more"
  "moreover"
  "most"
  "mostly"
  "mr"
  "mrs"
  "ms"
  "much"
  "must"
  "mustn't"
  "my"
  "myself"
  "name"
  "namely"
  "nd"
  "near"
  "nearly"
  "necessary"
  "need"
  "needn't"
  "needs"
  "neither"
  "never"
  "never"
  "neverless"
  "nevertheless"
  "new"
  "next"
  "nice"
  "nine"
  "ninety"
  "no"
  "nobody"
  "non"
  "none"
  "nonetheless"
  "noone"
  "no-one"
  "nor"
  "normally"
  "not"
  "nothing"
  "notwithstanding"
  "novel"
  "now"
  "nowhere"
  "obviously"
  "of"
  "off"
  "often"
  "oh"
  "ok"
  "okay"
  "old"
  "on"
  "once"
  "one"
  "ones"
  "one's"
  "only"
  "onto"
  "opposite"
  "or"
  "other"
  "others"
  "otherwise"
  "ought"
  "oughtn't"
  "our"
  "ours"
  "ourselves"
  "out"
  "outside"
  "over"
  "overall"
  "own"
  "particular"
  "particularly"
  "past"
  "per"
  "perfectly"
  "perhaps"
  "placed"
  "please"
  "plus"
  "possible"
  "presumably"
  "probably"
  "provided"
  "provides"
  "que"
  "quick"
  "quickly"
  "quite"
  "qv"
  "rather"
  "rd"
  "re"
  "really"
  "reasonably"
  "recent"
  "recently"
  "regarding"
  "regardless"
  "regards"
  "relatively"
  "respectively"
  "right"
  "round"
  "said"
  "same"
  "saw"
  "say"
  "saying"
  "says"
  "second"
  "secondly"
  "see"
  "seeing"
  "seem"
  "seemed"
  "seeming"
  "seems"
  "seen"
  "self"
  "selves"
  "sensible"
  "sent"
  "serious"
  "seriously"
  "seven"
  "several"
  "shall"
  "shan't"
  "she"
  "she'd"
  "she'll"
  "she's"
  "should"
  "shouldn't"
  "since"
  "six"
  "so"
  "some"
  "somebody"
  "someday"
  "somehow"
  "someone"
  "something"
  "sometime"
  "sometimes"
  "somewhat"
  "somewhere"
  "soon"
  "sorry"
  "specified"
  "specify"
  "specifying"
  "still"
  "sub"
  "such"
  "sup"
  "sure"
  "surely"
  "take"
  "taken"
  "taking"
  "tell"
  "tends"
  "th"
  "than"
  "thank"
  "thanks"
  "thanx"
  "that"
  "that'll"
  "thats"
  "that's"
  "that've"
  "the"
  "their"
  "theirs"
  "them"
  "themselves"
  "then"
  "thence"
  "there"
  "thereafter"
  "thereby"
  "there'd"
  "therefore"
  "therein"
  "there'll"
  "there're"
  "theres"
  "there's"
  "thereupon"
  "there've"
  "these"
  "they"
  "they'd"
  "they'll"
  "they're"
  "they've"
  "thing"
  "things"
  "think"
  "third"
  "thirty"
  "this"
  "thorough"
  "thoroughly"
  "those"
  "though"
  "thought"
  "thoughts"
  "three"
  "thrice"
  "through"
  "throughout"
  "thru"
  "thus"
  "thusly"
  "till"
  "to"
  "together"
  "too"
  "took"
  "toward"
  "towards"
  "tried"
  "tries"
  "truly"
  "try"
  "trying"
  "t's"
  "twice"
  "two"
  "un"
  "under"
  "underneath"
  "undoing"
  "unfortunately"
  "unless"
  "unlike"
  "unlikely"
  "until"
  "unto"
  "up"
  "upon"
  "upwards"
  "us"
  "use"
  "used"
  "useful"
  "uses"
  "using"
  "usually"
  "utterly"
  "value"
  "various"
  "versus"
  "very"
  "via"
  "viz"
  "vs"
  "want"
  "wants"
  "was"
  "wasn't"
  "way"
  "we"
  "we'd"
  "welcome"
  "well"
  "we'll"
  "went"
  "were"
  "we're"
  "weren't"
  "we've"
  "what"
  "whatever"
  "what'll"
  "what's"
  "what've"
  "when"
  "whence"
  "whenever"
  "where"
  "whereafter"
  "whereas"
  "whereby"
  "wherein"
  "where's"
  "whereupon"
  "wherever"
  "whether"
  "which"
  "whichever"
  "while"
  "whilst"
  "whither"
  "who"
  "who'd"
  "whoever"
  "whole"
  "wholly"
  "who'll"
  "whom"
  "whomever"
  "who's"
  "whose"
  "why"
  "will"
  "willing"
  "wish"
  "with"
  "within"
  "without"
  "wonder"
  "wondered"
  "wondering"
  "won't"
  "worst"
  "would"
  "wouldn't"
  "yes"
  "yet"
  "you"
  "you'd"
  "you'll"
  "your"
  "you're"
  "yours"
  "yourself"
  "yourselves"
  "you've"
  "zero")

(defparameter *nonword-scanner* (ppcre:create-scanner "[^'a-z ]"))
(defparameter *multispace-scanner* (ppcre:create-scanner " +"))

(defun stem-text (text)
  (let ((words (split " "
                      (ppcre:regex-replace-all *multispace-scanner*
                                               (ppcre:regex-replace-all *nonword-scanner*
                                                                        (string-downcase text)
                                                                        " ")
                                               " "))))
    (remove-duplicates (iter (for word in words)
                         (unless (stop-word-p word)
                           (collect (stem word))))
                       :test #'string=)))
  ; replace non-words with spaces
  ; split on spaces

(defun distance-selection-html (next-url &key text class style (search nil))
  (html
    (:form :method "post" :action "/settings" :style style :class class
      (:strong :class (when search "small") (str text))
      (:input :type "hidden" :name "next" :value next-url)
      (let ((distance (user-distance)))
        (htm
          (:select :class "distance-selection" :name "distance" :onchange "this.form.submit()"
            (:option :value "2" :selected (when (eql distance 2) "") "2 miles")
            (:option :value "5" :selected (when (eql distance 5) "") "5 miles")
            (:option :value "10" :selected (when (eql distance 10) "") "10 miles")
            (:option :value "25" :selected (when (eql distance 25) "") "25 miles")
            (:option :value "100" :selected (when (eql distance 100) "") "100 miles")
            (:option :value "0" :selected (when (eql distance 0) "") "everywhere"))))
      " "
      (:input :type "submit" :class "no-js" :value "apply"))))

(defun rdist-selection-html (next-url &key text class)
  (html 
    (:form :method "post" :action "/settings" :class class
      (:strong :class "small" (str text))
      (:input :type "hidden" :name "next" :value next-url)
      (let ((distance (user-rdist)))
        (htm
          (:select :class "distance-selection" :name "rdist" :onchange "this.form.submit();"
            (:option :value "1" :selected (when (eql distance 1) "") "1 mile")
            (:option :value "2" :selected (when (eql distance 2) "") "2 miles")
            (:option :value "5" :selected (when (eql distance 5) "") "5 miles")
            (:option :value "10" :selected (when (eql distance 10) "") "10 miles")
            (:option :value "25" :selected (when (eql distance 25) "") "25 miles")
            (:option :value "100" :selected (when (eql distance 100) "") "100 miles"))))
      " "
      (:input :type "submit" :class "no-js" :value "apply"))))

(defun search-inventory (type text &key (distance 10))
  ; get all requests within distance
  ; for each stem get matching requests
  ; return intersection
  (with-location
    (sort
      (result-id-intersection
        (geo-index-query (case type
                           (:offer *offer-geo-index*)
                           (t *request-geo-index*))
                         *latitude*
                         *longitude*
                         distance)
        (stem-index-query (case type
                           (:offer *offer-stem-index*)
                           (t *request-stem-index*))
                          text))
      #'> :key #'inventory-rank)))

(defun search-events (text &key (distance 10))
  ; get all events within distance
  ; for each stem get matching events
  ; return intersection
  (with-location
    (if (= distance 0)
      (sort
        (stem-index-query *event-stem-index* text) #'< :key #'event-rank)
      (sort
        (result-id-intersection
          (geo-index-query *event-geo-index*
                           *latitude*
                           *longitude*
                           distance)
          (stem-index-query *event-stem-index*
                            text))
        #'< :key #'event-rank))))

;(defun person-search-rank (id &key (userid *userid*))
;  (let* ((mutuals (mutual-connections id userid))
;         (user (db userid))
;         (contact (member id (getf user :following)))
;         (distance (person-distance (db id) user)))
;
;    (+ mutuals ) ) )

(defun search-people (query &key (userid *userid*))
  (let* ((aliases (remove-if-not #'alias-person-p (metaphone-index-query query)))
         (user (db userid))
         (lat (getf user :lat))
         (long (getf user :long))
         (following (getf user :following)))
    (labels ((person-rank (alias)
               (let* ((result (alias-result alias))
                      (id (result-id result))
                      (mutuals (length (mutual-connections id userid)))
                      (contact (member id following))
                      (result-lat (result-latitude result))
                      (result-long (result-longitude result))
                      (distance (when (and result-lat result-long)
                                  (air-distance lat long result-lat result-long))))
                 (+ (* 9 mutuals)
                    (aif distance
                      (/ 50 (log (+ 4 distance) 10))
                      0)
                    (if contact 100 0)
                    (* -12 (levenshtein:distance query (alias-alias alias))))))
             (person (alias)
               (cons (result-id (alias-result alias))
                     (alias-alias alias))))
      (mapcar #'person
              (sort aliases; (remove userid people :key #'result-id)
                    #'> :key #'person-rank)))))

(defun search-groups (query &key (userid *userid*) lat long)
  (let* ((aliases (remove-if-not #'alias-group-p (metaphone-index-query query)))
         (user (db userid))
         (lat (or lat (getf user :lat)))
         (long (or long (getf user :long))))
    (flet ((group-rank (alias)
             (let* ((result (alias-result alias))
                    (member-count (length (db (result-id result) :members)))
                    (distance (air-distance lat long (result-latitude result) (result-longitude result))))
               (+ (* 9 member-count)
                  (/ 50 (log (+ 4 distance) 10))
                  (* -12 (levenshtein:distance query (alias-alias alias))))))
           (group-id (alias)
             (cons (result-id (alias-result alias))
                   (alias-alias alias))))

      (mapcar #'group-id
            (sort aliases
                  #'> :key #'group-rank)))))

(defun request-results-html (request-list)
  (html
    (dolist (item request-list)
      (str (inventory-activity-item "request" item)))))

(defun offer-results-html (offer-list)
  (html
    (dolist (item offer-list)
      (str (inventory-activity-item "offer" item)))))

(defun event-results-html (event-list)
  (html
    (dolist (item event-list)
      (str (event-activity-item item
                                :truncate t
                                :show-distance t)))))

(defun people-results-html (person-list)
  (html
    (:div 
      (dolist (person person-list)
        (str (person-card (car person) :alias (cdr person)))))))

(defun groups-results-html (group-list)
  (html
    (:div
      (dolist (group group-list)
        (str (group-card (car group)))))))

(defun search-nearby-people (query &key (userid *userid*) (distance 10))
  (let* ((user (db userid))
         (lat (getf user :lat))
         (long (getf user :long)))
    (labels ((result-distance (result)
               (air-distance lat long (result-latitude result) (result-longitude result))))
      (sort
        (remove userid
          (result-id-intersection
            (geo-index-query *people-geo-index* lat long distance)
            (mapcar #'alias-result (metaphone-index-query query)))
          :key #'result-id)
        #'< :key #'result-distance))))

(defun more-results-link (type url count &optional (displayed 5))
  (when (> count displayed)
    (let* ((extra (- count displayed))
           (extra-s (when (> extra 1) "s")))
      (html
        (:div :class "more-results"
          (:a :href url (str (strcat "see "
                                     extra
                                     " more "
                                     (if (string= type "person")
                                       (if extra-s "people" "person")
                                       (s+ type extra-s))))))))))

(defun get-search ()
  (require-user
    (send-metric* :used-search *userid*)
    (let ((scope (or (get-parameter "scope") "all"))
          (q (get-parameter "q")))

      (standard-page
        "Search"
        (html
          (:h1 (str (s+ "Search results for \"" q "\"")))

          (cond
            ((scan +email-scanner+ q)
             (aif (gethash q *email-index*)
               (return-from get-search (see-other (strcat "/people/" (username-or-id it))))
               (htm
                 (:p "Nobody on Kindista has that email address. Would you like to "
                     (:a :href (strcat "/invite?email=" (url-encode q)) "invite them")
                     "?"))))
            ((string= scope "requests")
             (see-other (s+ "/requests?q=" (url-encode q))))

            ((string= scope "offers")
             (see-other (s+ "/offers?q=" (url-encode q))) )

            ((string= scope "events")
             (let ((events (search-events q :distance (user-distance))))
               (htm
                 (:p "Searching for events. Would you like to "
                     (:a :href (s+ "/search?q=" (url-encode q)) "search everything")
                     "?")
                 (if events
                   (htm
                     (str (event-results-html events)))
                   (htm
                     (:p "no results"))))))

            ((string= scope "people")
             (let ((people (search-people q)))
               (htm
                 (:p "Searching for people. Would you like to "
                     (:a :href (s+ "/search?q=" (url-encode q)) "search everything")
                     "?")
                 (if people
                   (htm
                     (str (people-results-html people)))
                   (htm
                     (:p "no results"))))))

            ((string= scope "groups")
             (let ((groups (search-groups q)))
               (htm
                 (:p "Searching for groups. Would you like to "
                     (:a :href (s+ "/search?q=" (url-encode q)) "search everything")
                     "?")
                 (if groups
                   (htm
                     (str (groups-results-html groups)))
                   (htm
                     (:p "no results"))))))

            (t ; all
             (let ((requests (search-inventory :request q :distance (user-rdist)))
                   (offers (search-inventory :offer q :distance (user-rdist)))
                   (events (search-events q :distance (user-distance)))
                   (people (search-people q))
                   (groups (search-groups q)))

               (if (or requests offers events people groups)
                 (progn
                   (when people
                     (htm
                       (:h2 (:a :href (s+ "/search?scope=people&amp;q=" (url-encode q)) "People"))
                       (str (people-results-html (sublist people 0 5)))
                       (when (> (length people) 5)
                         (htm
                           (:div :class "more-results" (:a :href (s+ "/search?scope=people&amp;q=" (url-encode q)) (fmt "see ~d more people" (- (length people) 5))))))))
                   (when groups
                     (htm
                       (:h2 (:a :href (s+ "/search?scope=groups&amp;q=" (url-encode q)) "Groups"))
                       (str (groups-results-html (sublist groups 0 5)))
                       (when (> (length groups) 5)
                         (htm
                           (:div :class "more-results" (:a :href (s+ "/search?scope=groups&amp;q=" (url-encode q)) (fmt "see ~d more groups" (- (length groups) 5))))))))
                   (when requests
                     (htm
                       (:h2 (:a :href (s+ "/requests?q=" (url-encode q)) "Requests")
                         " "
                         (str (rdist-selection-html (request-uri*)
                                                    :class "rdist search"
                                                    :text "show results within ")))
                       (str (request-results-html (sublist requests 0 5)))
                       (str (more-results-link "request"
                                               (s+ "/requests?q="
                                                   (url-encode q))
                                               (length requests)))))
                   (when offers
                     (htm
                       (:h2 (:a :href (s+ "/offers?q=" (url-encode q)) "Offers")
                         " "
                         (str (rdist-selection-html (request-uri*)
                                                    :class "rdist search"
                                                    :text "show results within ")))
                       (str (offer-results-html (sublist offers 0 5)))
                       (str (more-results-link "offer"
                                               (s+ "/offers?q="
                                                   (url-encode q))
                                               (length offers)))))
                   (when events
                     (htm
                       (:h2 (:a :href (s+ "/events?q=" (url-encode q)) "Events")
                         " "
                         (str (distance-selection-html (request-uri*)
                                                       :class "rdist search"
                                                       :search t
                                                       :text "show results within ")))
                       (str (event-results-html (sublist events 0 5)))
                       (str (more-results-link "event"
                                               (s+ "/search?scope=events&amp;q="
                                                   (url-encode q))
                                               (length events))))))
                 (htm
                   (:p "no results :-(")))))))
        :search q
        :class "search"))))
