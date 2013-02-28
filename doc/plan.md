Copyright 2012-2013 CommonGoods Network, Inc.

This file is part of Kindista.

Kindista is free software: you can redistribute it and/or modify it
under the terms of the GNU Affero General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Kindista is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public
License for more details.

You should have received a copy of the GNU Affero General Public License
along with Kindista.  If not, see <http://www.gnu.org/licenses/>.

-----------------------------------------------------------------------

User Interface
    [ ] remove references to "my" "your" "me" "you" etc.

Resources

Requests

Database

Geolocation
    [ ] localize Kindista based on IP address of guest

Donations
    [ ] Write thank you email
    [ ] set :donated flag on donator's account on donation

Invitations
    # A limited resource
    [ ] change site text wording to indicate that accounts require invitations
    [ ] modify account creation workflow to require invitation
    [ ] create 2 new invitations for a new user on creation
    [ ] create 2 new invitations for a user after first request
    [ ] create 2 new invitations for a user after first resource
    [ ] create 2 new invitations for a user after first gratitude given
    [ ] create 2 new invitations for a user after first gratitude received
    [ ] create 8 new invitations for a user after completing profile
    [ ] user gets an invitation credited back to their account when one of their invitees: completes profile, posts a resource, and gives gratitude
    [ ] invitations are stored by codes (preallocated)
    [ ] enabling/validating an invitation sets the :valid-until field to a future date
    [ ] expire initations when :valid-until is in the past
    [ ] modify signup form to require an invite code

Admin Interface

People
    [ ] redesign People page
        [ ] make new more textual layout for person-tile
        [ ] add a "Looking for someone?" wizard/helper form to the top of the page
        [ ] add a "Nearby groups"
    [ ] write meaningful essay questions for the "About" page on profiles
        - "What do you really want?"
        - "What are you learning?"
        - "How do you spend a typical weekday?"

Notices (async system)
    [X] event structure
        - event type
        - timestamp
        - text
        - user
        - IP
    [X] event handler table
    [X] event handling thread

Metrics
    [X] metric logging function
    [X] enable metric log on (run)

    - donations
    - Daily signups
    - Weekly signups
    - Daily logins

Task Queue
    # For follow-up and background actions that do not take place inside a request
    [ ] create task queue
        [ ] create task runner thread loop

Renew SSL
    [ ] renew SSL certificate for kindista.org

Help and Feedback
  [ ] index page written
    - Collect comments
    - Index of active topics
        - Bug fixes
        - New features

Acknowledgements
    - Create activity view for acknowledgements (K1 gifts)

Notifications
    - Emailing

Importing -- depends on Acknowledgements
    - Add tags to K1 offers and requests
    - Import K1 offers and requests
    - Import K1 acknowledgements
    - Import K1 users

Follow-up
    - Newsletter to existing users
    - Automated message from one of us to new signups
    - Invitation a few days later to complete more steps and let person know what has been happening

Flagging
    - What happens when something gets flagged?
      - Log the id of the flagger, of the item that is flagged.
      - Flagger indicates why it was flagged (inappropriate, inaccurate, unclear, etc) and has a text field to giver further info.
      - Flagger can choose whether or not to be anonymous to the flagged user.
      - Flagged user gets a notification detailing what needs to be changed on profile, with the ability to respond to the flagger and/or appeal to us.
      - When item is edited, the flagger gets a notification that the item has been edited and that they need to review the item to mark it resolved.
    - What happens when something gets flagged multiple times?
        - How many flags does it take?

Discussions
    - Person-to-person messages
    - Comments

Settings
    - contact preferences
    - basic information
        - name
        - location
        - aliases
            [ ] index aliases as well as names

Writing
  [ ] About page

################# MAYBE ############################

Groups
    - unified projects and communities
    - profile page
    - activity
    - discussions
    - membership
    - moderation

Discussions
    - Group conversations

################# LAUNCH ############################

Invitations
    - Show who invited a person to Kindista

Profile page
    - Titles (Member, Administrator, Moderator, Facilitator, etc.)

Events
    - Calendar

Donations
    - A/B metrics on ask text

Metrics
    - New data (new messages, resources, etc.)
    - User followthrough on signup
    - donations
        - donation followthrough

Voting (loves)
    [ ] keep track of vote ages (loves)
    [ ] implement sorting algorithm to account for vote ages

Tool Sharing

Facebook App
    - A Kindista user installs the Facebook App
    - user's Facebook friends are invited request an invitation to Kindista

Mobile App / Wrapper

Awards for users (badges)

#### DONE ####

User Interface
    [X] unified visual framework for several closely-related tools
    [X] made user interface "responsive" (adaptible to mobile phones, tablets, and large monitors)
    [X] JavaScript is no longer essential

Changes
    [X] recent activity list (recent changes)
        [X] include new/edited gratitude
        [X] include new/edited resources
        [X] include new/edited requests
        [X] include new/edited announcements

Inventory
    [X] tag system
        [X] top-level tags are automatically calculated for display as categories
        [X] browing inventory items by single tags and tag combinations

Database
    [X] implemented porter stemming full-text search index
    [X] implemented geohash search index
