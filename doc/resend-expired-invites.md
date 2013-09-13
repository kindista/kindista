prevent users from creating duplicate invites for the same email
  (resend invite instead of recreating it)

create new invitation email templates
   automatic reminder after 1 month
   user-created reminders
   last chance kindista invites
   reminders for expired guest invitations

create reminder timer
  new hash-table index
    created-date: invite id
  loop through the new index until there are no invites to send
  set the new timer based on the created-date of the next email in the list

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
