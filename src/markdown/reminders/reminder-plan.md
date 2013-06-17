- first reminder after 1 week
- remind no more than every other week

# Getting Started

## Complete Profile
- remind every 3 weeks
- no avatar
- no location
- no offers
- no invitations
- no gratitude

## No Inventory
- remind every 13 weeks

## Minimal Activity
- remind every 5 weeks
- no offers
- no invitations
- no gratitude
- no avatar
- no requests

# Keep it going

## More Offers
- after 26 weeks w/o new offers

## More Requests
- after 26 weeks w/o new requests

## More Invites
- if less than 10 guests
    every 26 weeks
- if less than 20 guests or 30 connections
    every 52 weeks

## More Gratitudes
- after 26 weeks w/o new gratitudes

# Update code base
  - unsubscribe from reminders in settings
  - unsubscribe from reminders on account deactivation

# Server actions
  - (stop *acceptor*)
  - load-db to populate people-index
  - (dolist (userid *active-people-index*) (modify-db userid :notify-reminders t)
  - (start *acceptor*)
  - create a daily chron job to make http request to trigger notifications
