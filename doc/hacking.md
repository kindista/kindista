Use a package for every file.

Keep the namespace under control. The approved packages for :use are:

  :cl

  and when needed

  :alexandria
  :anaphora-basic
  
  and for ParenScript modules:

  :parenscript
  :ps-dom1-symbols

Do everything else with :import-from.

This is the programming equivalent of "showing your work". It is helpful
for others when they are reading your work and helpful for us when we
are re-reading our old code.
