Edit requested-invite text for reminder: "Final Reminder"

4068-4778 should get prelaunch-invite-reminders

when sending auto reminder
  make sure people are getting correct invitation file sent with :auto set


Test:

Migrate from master

request invitation
 -signup
 -approve pending account

invite someone from new account

try to invite them again

resend invite

try auto resend invite
 -check invite data
 -check reminder index

invite someone

auto resend invite
  -check reminder index

resend invite

get expired invite reminder

resend expired invite

accept an invitation
 -signup

delete a pending invitation


,b on features/invitations
run (migrate-to-new-invitation-system)
verify (db 3042 :times-sent)
(end)
(ql:quickload :kindista)
(run)

(delete-all-duplicate-invitations)

delete bad email addresses from benjamin and kindista's invites
  mailinator
  mgbox01@gmail.com
