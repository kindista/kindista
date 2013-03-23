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

(defun terms-html ()
  (standard-page 
    "Terms of Use"
    (terms-page)
    :right (html
             (str (donate-sidebar))
             (when *user* (str (invite-sidebar))))))

(defun terms-page ()
 (html 
  (:div :class "legal"
   (:h1 "Terms of Use")
   (:p (:strong "Effective June 15, 2012"))
   (:p "By using Kindista.org (the \"Site\"), you agree to these Terms of Use "
      "(\"TOU\"), including Kindista's " (:a :href "/privacy" "Privacy Policy ")
      "and any other guidelines and additional terms referenced in these documents. "
      "Please read the TOU carefully. "
      "Your use of the Site constitutes your acceptance of the TOU. "
      "Do not use the Site if you are unwilling or unable to be bound by the TOU.")
   (:ol
    (:li (:p (:strong "Definitions.")) ;1
      (:ol
        (:li
          "A \"Member\" is a User who registers for an account, creates a profile, or posts content to the Site.")
       ;(:li
       ;  "A \"Project\" is a temporary or ongoing group endeavor. "
       ;  "Members can create Project profiles on the Site to represent businesses, organizations, "
       ;  "collectives, associations, cooperatives, or other groups. "
       ;  "Project profiles allow groups of Members to offer and give Gifts of their collective "
       ;  "efforts, and to request and receive Gifts collectively to support the Project's "
       ;  "contributors and its ongoing activities.")
        (:li
          (:p "A \"Resource\" is an asset that a Member " 
            ;or Project 
            "has posted to Kindista "
            "with the intention of sharing it with another User "
            ;or Project 
            "as a gift. "
            "Users can browse and search for Resources posted on Kindista. "
            "If you post a Resource, other Users may contact you to request your Resource. "
            "It is up to you to decide to whom and under what circumstances you will share your Resource. "
            (:strong "Resources shared through Kindista must be given or loaned free of charge, "
              "without any debt incurred to the recipient. "))
          (:p "Resources include, but are not limited to, items, supplies, food, materials, tools, "
            "equipment, aid, tutoring or instruction, guidance, labor, support, housing, "
            "energy, work space, consultation, or art; or the temporary use of tools, equipment, "
            "or living or work space."))
        (:li
          "A \"Request\" is a description of a resource that a User "
          ;or Project 
          "would like to receive from another User "
          ;or Project
          "as a gift. "
          "Users can browse and search for Requests posted on Kindista. "
          "If you post an Request, other Users may contact you to offer you the Request. "
          "It is up to you to decide to whether or not to accept an offer from another User to fulfill your Request. ")
        (:li
          "\"Content\" means text and images, such as:"
          (:ul
            (:li "descriptions of Resources and Requests, "
            (:li "gratitude posted about users "
             ;or projects
             ", ")
            (:li "events posted to the calendar, ")
            (:li "and announcements posted on Kindista.")))
        (:li
          "\"Your Content\" means Content that you submit or post to the Site, "
          "including descriptions of Resources and Requests; "
          "words of gratitude that you post about other Members; "
          "information that you display as part of your account profile; "
          "invitations, events, or announcements; "
          "or other commentary that you may submit or post while using the Site. "
          "Your Content does not include personally identifiable information that you submit "
          "when creating an account on the Site (such personal information is subject to our "
          (:a :href "/privacy" "Privacy Policy") ").")
        (:li
          "\"Kindista\" means features, functionality, and services we make available, including through our "
          "website at kindista.org and any other Kindista branded or co-branded websites "
          "(including sub-domains, international versions, and mobile versions).")
        (:li
          "\"CommonGoods\" and references to \"us,\" \"we,\" or \"our\" mean "
          "CommonGoods Network, Inc. "
          "Commongoods is a 501(c)(3) charitable organization and "
          "the parent organization of Kindista."))))
    (:li :id "restrictions" (:strong "Restrictions on Use. ") ;2
      (:p (:strong "Do not engage in activities that may betray the trust, "
       "goodwill, integrity, safety, or generosity of the Kindista community. ")
       "Certain activities, whether legal or illegal, may be harmful to "
       "other users or to the integrity of the Kindista community as a whole. "
       "Some activities may violate our rules and may also subject you to liability. "
       "Therefore, for your own protection and for that of other users, you "
       "may not engage in such activities on our site. "
       "Specifically:")
      (:ol
        (:li (:strong "Do not engage in commercial or \"for-profit\" activities")
          (:ul
            (:li "You may not use Kindista to offer goods or services for "
             "sale, rent, or barter. "  
             "When sharing a Resource " (:em "you are permitted") " to charge a Resource "
             "recipient for any direct out-of-pocket monetary costs associated "
             "with providing your Resource (e.g. supplies and materials) as well "
             "as a portion of your overhead costs (e.g. insurance, utilities, or rent). "
             "However, nothing can be posted on the site that requires monetary "
             "exchange beyond covering the actual cost of the Resource or event. "
             (:em "Kindista is not about selling, renting, or trading. It's about sharing. " ))
            (:li "Do not collect or record information about Users for commercial purposes.")))
        (:li (:strong "Do not engage in harassment, discrimination, abuse, or "
              "hostility towards other Users.")
          (:ul
            (:li "Do not post content or messages that are malicious, hateful, "
             "inflammatory, discriminatory, divisive, or bigoted.")
            (:li "Do not use the Site to threaten, stalk, defraud, incite, harass, "
             "or advocate the harassment of another person, or otherwise "
             "interfere with a User's enjoyment of the Site.")
            (:li "Do not use Kindista to transmit or post spam, chain letters, "
             "contests, junk email, pyramid schemes, or similar mass messaging, "
             "whether commercial in nature or not.")))
        (:li (:strong "Do not engage in false statements, impersonation, or fraud.")
          (:ul 
            (:li "Do not intentionally post libelous, inaccurate, "
             "exaggerated, or false information about "
             "yourself "
             ;, Projects,
             "or other people on the Site. ")
            (:li "Do not create multiple accounts for yourself "
             ;, a Project 
             "or another Member on the Site.")
            (:li "Do not create shadow or dummy accounts for non-existent Users "
             ;or Projects 
             "for the purpose of developing a false reputation, or for "
             "any other purpose.")
            (:li "Do not use the Site to falsely represent yourself as "
             ;either (1) an administrator or member of a Project, or
             "an agent of another User.")))
        (:li (:strong "Do not use the Site in violation of the TOU or any "
              "applicable local, state, national or international law.")
          (:ul
            (:li "Do not solicit personally identifiable information from anyone under "
             "the age of 18 for an illegal purpose or violating any applicable law "
             "regarding the health or well-being of minors.")
            (:li "Do not post child pornography or any content that violates applicable "
             "law concerning child pornography.")
            (:li "Do not solicit illegal items or materials.")
            (:li "Do not post obscene material that is unlawful under applicable law.")))
        (:li (:strong "Do not engage in disruptive, malicious, or illegal misuse of facilities.")
          (:ul
            (:li "Do not access, crawl, or index any portion of the Site for "
             "purposes of constructing or populating a searchable database of "
             "Content from the Site.")
            (:li "Do not make any action that imposes, or may impose in our sole "
             "discretion, an unreasonable or disproportionately large load on "
             "Kindista's technology infrastructure.")
            (:li "Do not attempt to gain unauthorized access to the Site, "
             "User accounts, computer systems or networks connected to the "
             "Site through hacking, password mining or any other means.")
            (:li "Do not use the Site to transmit any viruses, worms, "
             "defects, Trojan horses, spyware, or otherwise malicious computer "
             "code.")
            (:li "Do not disrupt or interfere with the security or proper "
             "working of the Site, or otherwise cause harm to the Site, "
             "Services, or Content.")))))
    (:li (:strong "INTERACTIONS WITH OTHER MEMBERS") ;3
       (:ol
         (:li (:strong "You Have Sole Responsibility When Interacting with Other Members. ")
           "Kindista provides a platform for members to learn about one "
           "another, arrange the sharing of Resources, organize events and activities, "
           "and communicate with one another. "
           "Kindista is not a party to, has no involvement or interest "
           "in, makes no representations or warranties as to, and has no "
           "responsibility or liability with respect to any communications, "
           "interactions, disputes or any relations whatsoever "
           "between you and any other Member "
           ;, Project,
           "or person. " 
           "You are solely responsible for your interactions with other "
           "Kindista users")
         (:li (:strong "Identity Verification. ")
           "We cannot and do not confirm each Member’s identity. "
           "Although we provide tools intended to assist with identity "
           "verification, you are solely responsible for determining the identity and "
           "suitability of others with whom you may interact through Kindista. "
           "Kindista does not represent or warrant that our tools are "
           "sufficient to determine whether it is appropriate for you to "
           "interact with another member. "
           "Further, we do not endorse any persons who use or register for our Services. "
           "We encourage you to take precautions when interacting with other "
           "members, particularly when meeting a stranger in person for the "
           "first time. ")
         (:li (:strong "Release. ")
           "You understand that Content posted by other members may be offensive, indecent, "
           "inaccurate, objectionable, or otherwise inappropriate. "
           "CommonGoods does not endorse Member Content including but not limited "
           "to the opinions, recommendations, or advice expressed therein."  
           "Because Kindista is merely a platform, in the event that you "
           "have a dispute with one or more members, you release us  "
           "(and our officers, directors, members, employees, agents and affiliates) "
           "from claims, demands and damages (actual and consequential) of "
           "every kind and nature, known and unknown, "
           "disclosed and undisclosed, arising out of or in "
           "any way connected with such disputes. ")))
    (:li  (:p (:strong "Responsibility for Your Content.")) ;4
      (:ol
        (:li
          "You are solely responsible for Your Content and its accuracy. "
          "You assume all risks associated with use of Your Content, "
          "including any reliance on its accuracy, completeness or usefulness "
          "by others. "
          "By using the Site, you hereby affirm and warrant that you own, or "
          "have the necessary licenses, rights, and permissions to use, and "
          "authorize Kindista to use, Your Content as described in the TOU. "
          "You may not imply that Your Content is in any way sponsored or endorsed by Kindista or Commongoods."
          "For more information on your responsibility for Your Content, see "
          "the section entitled \"Restrictions on Use\" above.")
        (:li
          "CommonGoods reserves the right (but has no obligation) to remove "
          "or suppress Member Content from Kindista that it deems "
          "untrustworthy, inappropriate, or in violation of the TOU or guidelines for "
          "Resources, Requests, comments, reviews, photos, or talk threads. "
          "Such removal or suppression of Member Content shall be at "
          "CommonGoods's sole discretion and without notice or liability of "
          "any kind.")))
    (:li (:strong "Member Accounts. ") ;5
      (:p "To access or use many of the Services, you will be required to "
        "register on the Site by creating an account and providing "
        "information about yourself to Kindista. "
        "You are solely responsible for maintaining the confidentiality of "
        "any passwords that you create in connection with the Site, and are "
        "solely responsible for all activities that occur in connection with "
        "your account. "
        "You agree to notify Kindista immediately of any unauthorized use of your account. "
        "Kindista reserves the right to close your account for violations of the TOU. ")
     ;(:p "There are two types of accounts:")
     ;(:ol
        (:p 
         ;"A \"Personal Account\" 
         "Your account is for personal, "
         "non-commercial use only, and may be used (among other things) to "
         "submit or post Resources, Requests, reviews, messages, statements of gratitude, "
         "invitations, commentary, photographs, and other types of User "
         "Content. "
         "In creating or updating an account "
         ;Personal Account
         ", we require that you "
         "provide complete and accurate information about yourself in order to "
         "bolster your credibility as a contributor to the Site. "
          (:u "Notwithstanding the foregoing, you may not impersonate someone "
           "else (e.g., adopt the identity of a celebrity or your next-door "
           "neighbor), provide a false account name (the name you "
           "provide must be either your legal name or the name you "
           "are generally known by), provide an email address that "
           "is not your own, or create multiple "
           ;Personal Accounts.
           "accounts."))
       ;(:li "A \"Project Account\" is an account for use on the Site as a "
       ; "representative of a Project that is listed on the Site. "
       ; "In creating or updating a Project Account, you must be an "
       ; "authorized administrative agent of the group or organization "
       ; "represented by the Project, and you must provide complete and "
       ; "accurate information about yourself and the Project you represent. "
       ; (:u "You may not provide an email address that is not your own or "
       ; "create multiple Project Accounts for the same Project.")
       ; (:strong "If you are using Kindista on behalf of a business, that business "
       ; "accepts these terms. ")))
        )
    (:li  (:p (:strong "Licenses.")) ;6
      (:p "When you post Content to Kindista, "
        "you retain ownership of any intellectual property rights that you "
        "hold in that content. "
        "In short, what belongs to you stays yours. However: ")
      (:ul
        (:li "When you submit Content to Kindista, "
          "you give us (and those we work with) a non-exclusive, worldwide license "
          "to use, host, store, reproduce, create derivative works "
          "(such as those resulting from translations or other changes we make "
          "so that your content works better with our services), "
          "publish, publicly display, and distribute that content. "
          "The rights you grant in this license are for the limited purpose "
          "of operating, promoting, and improving our services, "
          "and to develop new ones. ")
        (:li
          "You also grant other Users a non-exclusive license to access, use, reproduce, "
          "and distribute Your Content for their personal, non-commercial use; "
          "and for the purpose of building a global culture of sharing, "
          "both online, and in the physical world." )
        (:li
          "Make sure you have the necessary rights "
          "to grant us this license for any content that you submit to our "
          "Services."  
          "The licenses you grant to Kindista and to our Users "
          "continue even if you stop using Kindista "
          "Kindista may offer you ways to access "
          "and to remove some of the Content you have submitted. "
          "It is your responsibility to remove such data if you so choose, "
          "should you decide to discontinue use of Kindista. "))
      (:p "You can find more information about how Kindista uses and stores content in the "
          (:a :href "/privacy" "Privacy Policy ") ". "
          "If you submit feedback or suggestions about Kindistta, "
          "we may use your feedback or suggestions without obligation to you."))
    (:li  (:p (:strong "Eligibility. ")  ;7
      "By accessing or using the Site, you represent and warrant that you have "
      "the right, authority, and capacity to enter into the TOU and to abide by "
      "all of the terms and conditions set forth herein. "
     ;"If you access or use the Site on behalf of a Project, you warrant that "
     ;"you are an authorized agent of the group or organization represented by "
     ;"such Project and that you have the authority to bind it to the TOU. "
      "You may not use Kindista or accept the TOU if you are not of legal age "
      "to form a binding contract unless "
      "your parent or legal guardian agrees to assume all responsibility "
      "for actions on Kindista and agrees to these Terms on your behalf."))
    (:li  (:p (:strong "Changes to the TOU or the Site. ") ;8
      "The TOU may be modified by Kindista at any time. "
      "When these changes are made, Kindista will notify you by making a new copy of the TOU available on this page and will indicate at the top of this page the date these terms were last revised. "
      "Any such modification will be effective upon Kindista's posting of new TOU. "
      "Your continued use of the Site after any posted modification to the TOU indicates your acceptance of the modification. "
      "Unless expressly stated otherwise by Kindista, any new services or "
      "functionality implemented after your initial access to the Site shall "
      "be subject to the TOU."))
    (:li  (:p (:strong "Availability of Service. ") ;9
      "CommonGoods reserves the right to modify, update, or discontinue the "
      "Site or any of the services at its sole discretion, at any time, for "
      "any or no reason, and without notice or liability. "
      "If you would like to help ensure that we are able to continue to "
      "provide this service, please consider donating to CommonGoods Network "
      "to support our work."))
    (:li  (:p (:strong "Unauthorized Access. ") ;10
      "CommonGoods reserves the right to exercise whatever lawful means it deems necessary to prevent unauthorized access to or use of the Site, including, but not limited to, technological barriers, IP mapping, and contacting your Internet Service Provider (ISP) regarding such unauthorized use."))
    (:li  (:p (:strong "Feedback.")) ;11
      (:ol
        (:li
          (:em "Dissatisfaction. ") "If you are dissatisfied with the Site, please let us know by providing feedback through our contacts page. "
          "Your input is valuable to us. ")
        (:li
          (:em "Suggestions and Improvements. ") 
          "By submitting ideas, suggestions, documents and/or proposals (\"Feedback\") to Kindista through its suggestion, feedback, forum or similar web pages, you acknowledge and agree that:"
          (:ol
            (:li "your Feedback does not contain the confidential or proprietary information of third parties; ")
            (:li "CommonGoods is not under any obligation of confidentiality with respect to the Feedback;")
            (:li "CommonGoods shall be entitled to use and disclose such Feedback for any purpose, in any way, in any media worldwide; and")
            (:li "your Feedback does not entitle you to any compensation or reimbursement of any kind from CommonGoods under any circumstances.")))))
    (:li (:p (:strong "Termination. ") ;12
             "CommonGoods reserves the right to terminate or suspend your account or your ability to use the Site, in whole or in part, if it discovers that you have violated the TOU. ")      
         (:p "Termination of your account may include:") 
         (:ol 
           (:li "removal of access to all offerings within Kindista;") 
           (:li "deletion of your password and all related information, files and materials associated with or inside your account (or any part thereof), including Your Content; and") 
           (:li " barring of further use of Kindista. "))
         (:p "You agree that CommonGoods shall not be liable to you or any third party for any termination of your account, or access to the Site, Services and Data, including Your Content."))
        ;(:li
        ;  "You may deactivate your account at anytime on the settings page of your profile. "
        ;  "However, if you deactivate your account, sections 1, 5, 7-13, and 15-23 of the TOU shall continue to apply in full until terminated by CommonGoods. "
        ;  "If you wish to reactivate your account you may do so by logging in using your primary email address. "
        ;  "If you reactivate your account, your use of the Site will be bound by the version of the TOU and Privacy Policy current as of the date of your account reactivation.")
    (:li  (:p (:strong "Ownership. ") ;13
      "The name \"Kindista\" and the Kindista logo including the symbol are "
      "proprietary service marks of CommonGoods Network Inc. "  
      "CommonGoods Network Inc. owns Kindista, including but not "
      "limited to visual interfaces, interactive features, graphics, design, "
      "computer code, software, "
      "and all other elements and components of the Site excluding "
      "Member Content. "))
    (:li  (:p (:strong "Copyright Complaints ") ;14
      "If you believe that anything on Kindista infringes "
      "upon any copyright which you own or control, you may "
      "file a notification of such infringement with the "
      "Kindista Copyright Agent as set forth below." 
      "Similarly, if you believe that content has been mistakenly removed from the "
      "Site pursuant to this Copyright policy, you may send a written "
      "counter-notice to the Copyright Agent as set forth below.")
      (:p "Please note that CommonGoods may, at its sole discretion, send a "
          "copy of such notices to third-parties, such as "
          (:a :href "http://www.chillingeffects.org" "Chilling Effects") 
          ", for publication (with personal information removed).")

      (:p "Please see " 
       (:a :href "http://www/copyright.gov/title17/92chap5.html"
                 "17 U.S.C. §512(c)(3)")
       "for the requirements of a proper notification or counter-notice. "
       "You should note that "
       "if you knowingly misrepresent in your notification that the material "
       "or activity is infringing, you will be liable for any damages, "
       "including costs and attorneys' fees, incurred by us or the alleged "
       "infringer as the result of our relying upon such misrepresentation in "
       "removing or disabling access to the material or activity claimed to "
       "be infringing. "))
    (:li  (:p (:strong "Disclaimers.")) ;15
     (:ol
      (:li
        "THE SITE AND ALL SITE CONTENT IS MADE AVAILABLE TO YOU ON AN \"AS IS\" BASIS. "
        "COMMONGOODS MAKES NO WARRANTIES, REPRESENTATIONS, OR CONDITIONS OF "
        "ANY KIND, EXPRESS, STATUTORY OR IMPLIED AS TO (1) THE OPERATION "
        "AND FUNCTIONALITY OF THE SITE, (2) THE ACCURACY, INTEGRITY, "
        "COMPLETENESS, QUALITY, LEGALITY, USEFULNESS, SAFETY, AND IP RIGHTS "
        "OF ANY OF THE SITE CONTENT, INCLUDING BUT NOT LIMITED TO THE "
        "ACCURACY OF RESOURCES, REQUESTS, AND STATEMENTS OF GRATITUDE LISTED ON THE SITE, AND "
        "(3) THE PRODUCTS AND SERVICES ASSOCIATED WITH THE SITE OR SITE "
        "CONTENT, INCLUDING BUT NOT LIMITED TO THE GIFTS GIVEN BY USERS "
        ;AND PROJECTS 
        "LISTED ON THE SITE. "
        "NO ADVICE OR INFORMATION, WHETHER ORAL OR WRITTEN, THAT YOU OBTAIN "
        "FROM COMMONGOODS OR THE SITE SHALL CREATE ANY WARRANTY, "
        "REPRESENTATION, OR CONDITION NOT EXPRESSLY STATED HEREIN.")
      (:li
        "COMMONGOODS SPECIFICALLY DISCLAIMS ALL LIABILITY RELATING TO YOUR USE OF THE SITE, SERVICES, AND SITE CONTENT. "
        "ACCESS TO, AND USE OF, THE SITE, SERVICES, AND SITE CONTENT ARE AT YOUR OWN DISCRETION AND RISK, AND YOU WILL BE SOLELY RESPONSIBLE FOR ANY DAMAGE TO YOUR COMPUTER SYSTEM OR LOSS OF DATA RESULTING THEREFROM. "
        "COMMONGOODS ASSUMES NO LIABILITY FOR ANY COMPUTER VIRUS OR SIMILAR CODE THAT IS DOWNLOADED TO YOUR COMPUTER FROM THE SITE OR IN CONNECTION WITH THE SERVICES.")
      (:li
        "YOUR COMMUNICATIONS OR DEALINGS WITH ANY OF "
        ;THE PROJECTS OR 
        "USERS ON THE SITE, ARE SOLELY BETWEEN YOU AND SUCH "
        ;PROJECTS AND 
        "USERS. "
        "COMMONGOODS IS NOT RESPONSIBLE, AND DISCLAIMS ALL LIABILITY, FOR ANY LOSS OR DAMAGE ARISING OUT OF ANY SUCH COMMUNICATIONS OR DEALINGS. COMMONGOODS RESERVES THE RIGHT (BUT HAS NO OBLIGATION) TO MONITOR DISPUTES BETWEEN YOU AND SUCH "
        ;PROJECTS AND 
        "USERS.")
      (:li
        "SOME JURISDICTIONS DO NOT ALLOW THE EXCLUSION OF CERTAIN WARRANTIES, REPRESENTATIONS, OR CONDITIONS, THE LIMITATION OR EXCLUSION OF IMPLIED WARRANTIES, OR LIMITATIONS ON HOW LONG AN IMPLIED WARRANTY MAY LAST, SO THE ABOVE LIMITATIONS MAY NOT APPLY TO YOU. "
        "IF YOU RESIDE IN SUCH A JURISDICTION, THE ABOVE LIMITATIONS SHALL APPLY TO YOU TO THE FULLEST EXTENT PERMITTED UNDER APPLICABLE LAW.")))
    (:li  (:p (:strong "Limitation on Liability. ") ;16
      "WITHOUT LIMITING THE FOREGOING, COMMONGOODS WILL NOT BE LIABLE UNDER "
      "ANY THEORY OF LAW, FOR ANY INDIRECT, INCIDENTAL, PUNITIVE, EXEMPLARY, "
      "RELIANCE, OR CONSEQUENTIAL DAMAGES, INCLUDING, BUT NOT LIMITED TO LOSS "
      "OF PROFITS, BUSINESS INTERRUPTION, AND/OR LOSS OF INFORMATION OR DATA. "
      "NOTWITHSTANDING ANYTHING TO THE CONTRARY CONTAINED HEREIN, "
      "COMMONGOODS'S MAXIMUM AGGREGATE LIABILITY TO YOU FOR ANY CAUSES "
      "WHATSOEVER, AND REGARDLESS OF THE FORM OF ACTION, WILL AT ALL TIMES BE "
      "LIMITED TO THE GREATER OF (i) THE AMOUNT PAID, IF ANY, BY YOU TO "
      "COMMONGOODS IN CONNECTION WITH THE SITE IN THE 12 MONTHS PRIOR TO THE "
      "ACTION GIVING RISE TO LIABILITY OR (ii) US$100.00. "
      "SOME JURISDICTIONS DO NOT ALLOW THE EXCLUSION OR LIMITATION OF "
      "INCIDENTAL, CONSEQUENTIAL, OR PUNITIVE DAMAGES, SO THE ABOVE "
      "LIMITATIONS AND EXCLUSIONS MAY NOT APPLY TO YOU. "
      "IF YOU RESIDE IN SUCH A JURISDICTION, THE ABOVE LIMITATIONS AND "
      "EXCLUSIONS SHALL APPLY TO YOU TO THE FULLEST EXTENT PERMITTED UNDER "
      "APPLICABLE LAW."))
    (:li (:p (:strong "DISPUTES")) ;17
     (:ol
      (:li (:strong "Jurisdiction. ")
        "If there is any dispute about or involving Kindista or CommonGoods "
        "Network Inc., you agree that any such dispute will be governed by "
        "the laws of the State of Oregon, without regard to conflict of "
        "law provisions. "
        "You agree to personal jurisdiction by and venue in the state and "
        "federal courts in Salem, Oregon. "
        "Some jurisdictions prohibit the use of a choice of law clause that "
        "would prevent a consumer from having recourse to courts in the "
        "consumer's local jurisdiction, so this governing law clause may not "
        "apply to you.")
      (:li  (:p (:strong "Indemnity. ") 
        "You agree to indemnify and hold CommonGoods and its officers, directors, "
        "volunteers, and employees harmless from and against all damages, "
        "losses, and expenses "
        "of any kind (including reasonable legal fees and costs ) arising from any "
        "claim related to your actions, content, or information "
        "on Kindista, or your use of the services. "))))
    (:li  (:p (:strong "Third Parties. ") ;18
      "Site Content may include links to other websites (the \"Third Party Sites\"). "
      "Kindista does not control or endorse any Third Party Site, and you "
      "agree that CommonGoods is not responsible for the availability or "
      "contents of any Third Party Site.")
      (:p "The Site uses a mapping feature powered by Google Inc. Your use of "
          "the mapping feature is governed by Google Inc.'s terms of use "
          "located at "
          (:a :href "http://maps.google.com/help/terms_maps.html" 
           "http://maps.google.com/help/terms_maps.html")
          " (or such other URL as may be updated by Google Inc.)."))
    (:li  (:p (:strong "Miscellaneous.")) ;19
     (:ol
      (:li
        "No agency, partnership, joint venture, or employment is created as "
        "a result of the TOU, and you do not have any authority of any kind "
        "to bind CommonGoods in any respect whatsoever.")
      (:li
        "Kindista may provide you with notices, including those regarding "
        "changes to the TOU by email, regular mail or postings on the "
        "Site.")
      (:li
        "Except as otherwise stated in Section 18 above, nothing herein is intended, nor will be deemed, to confer rights or remedies upon any third party.")
      (:li
        "The TOU and Privacy Policy contain the entire agreement between "
        "you and CommonGoods regarding the use of Kindista, and supersede "
        "any prior agreement between you and CommonGoods on such subject "
        "matter.")
      (:li
        "The failure of CommonGoods to exercise or enforce any right or provision of the TOU shall not constitute a waiver of such right or provision. The failure of either party to exercise in any respect any right provided for herein shall not be deemed a waiver of any further rights hereunder.")
      (:li
        "If any provision of the TOU is found to be unenforceable or invalid, that provision shall be limited or eliminated to the minimum extent necessary so that the TOU shall otherwise remain in full force and effect and enforceable.")
      (:li
        "The TOU are not assignable, "
        "transferable or sub-licensable by you except with CommonGoods's prior "
        "written consent or as expressly indicated above. "
        "However, the TOU may be assigned or transferred by CommonGoods " 
        "without restriction. Any assignment attempted to be made in violation "
        "of the TOU shall be void.")
      (:li
        "The section titles in the TOU are for convenience only and have no legal or contractual effect.")))
    (:li  (:p (:strong "Contact and Violations. ") ;20
      "Please contact us with any questions regarding the TOU. Please report "
      "any violations of the TOU to our legal department through the contacts "
      "page."))))))
