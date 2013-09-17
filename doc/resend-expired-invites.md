Edit requested-invite text for reminder: "Final Reminder"

4068-4778 should get prelaunch-invite-reminders

when sending auto reminder
  make sure people are getting correct invitation file sent with :auto set

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
