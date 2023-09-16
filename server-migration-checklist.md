## Install
- nginx
- sbcl (through apt-get)
- git
- postfix
- ca-certificates
- spamassassin, pyzor, razor
- tarsnap
- vim-nox
- certbot
- openssl
- imagemagick
- screen
- libssl-dev (if quicklisp can't find package libssl.so)
- python3-pip
- git clone https://github.com/miracle2k/tarsnapper
- pip3 install tarsnapper

## Edit settings.lisp file
- new local ip address
- correct path to image magic convert
- *production-p* t
- new "+base-url+"

## Create cron jobs
- for all routes that happen periodically (activity reminders, etc)
- for tarsnap

## After server switchover
- deactivate old server cron jobs
- stop and disable nginx on old server
- stop and disable postfix on old server

## Tarsnap
- https://www.tarsnap.com/pkg-deb.html
- https://www.tarsnap.com/faq.html#moving-computers

## SlimV
- make sure correct version of python is installed

## SSH and SSL? what's needed from
- /etc/ssh
- /etc/ssl
- sudo certbot --nginx -d kindista.org -d www.kindista.org
- sudo certbot delete --cert-name k4.kindista.org

## DNS
- make a temporary k#.kindista.org A and AAA record to point to the new server
- after installation and testing change A and AAAA records for kindista.org to point to the new server
- change DNS for kindista.com, kindista.net

## Git repositories
- kindista directory
- books directory (financial statments)
- test push git origin new server
- test git pull master

## Make sure root directory has all necessary subdirectories
- media/blog
- media/avatar
- media/images
- data/
- rsync -av lisp@kindista.org:kindista/data/images/ /home/lisp/kindista/data/images/
- scp lisp@kindista.org:kindista/data/db\* .
- (load-db)

## Make sure to transfer
- /etc/ssl
- /usr/local/etc/nginx/nginx.conf or /etc/nginx/conf
- enable letsencrypt and dhparam lines in nginx.conf
- enable certbot in nginx.conf after transfering domain
- /etc/tarsnap.conf (or /usr/local/etc/tarsnap/tarsnap.conf)
- /usr/local/tarsnap-cache
- /etc/aliases
- /etc/postfix/main.cf
- /etc/postfix/header_checks
- /etc/postfix/local_domains
- /etc/postfix/sasl_passwd
- /etc/spamassassin/local.cf
- /etc/default/spamassassin
- spamassassin lines in /etc/postfix/master.cf

## NGINX
- systemctl enable nginx.service

## Changing IP address
- new local ip address in settings.lisp (same as above)
? change IP in https://developers.facebook.com/apps/779034685520810/settings/advanced/ (when signed in as a developer)
- change IP in https://console.developers.google.com/apis/credentials/key/0?project=kindista-1296&authuser=2 (when signed in as commongoods' google account)
- Add new IP addresses to googlemaps api key settings: https://console.cloud.google.com/apis/credentials?project=kindista-1296&supportedpurview=project

## Postfix and Mailgun
- sudo postmap /etc/postfix/sasl_passwd
- touch /etc/postfix/generic
- postmap /etc/postfix/generic (to create error db file)
- postmap /etc/postfix/local_domains (local_domains needed to prevent spoofing of kindista.org)
- sudo service postfix restart
- systemctl enable postfix.service

## Install spamassassin
- apt-get install spamc spamassassin
- edit /etc/default/spamassassin 'ENABLED=1'
- groupadd -g 5555 spamd
- useradd -u 5555 -g spamd -s /sbin/nologin -d /usr/local/spamassassin spamd
- mkdir -p /usr/local/spamassassin/log
- chown spamd:spamd -R /usr/local/spamassassin
- systemctl enable spamassassin.service
- https://syslint.com/blog/tutorial/how-to-install-and-configure-spamassassin-with-postfix-in-debian-8/
- mkdir /etc/spamassassin/.razor
- razor-admin -home=/etc/spamassassin/.razor -register
- razor-admin -home=/etc/spamassassin/.razor -create
- razor-admin -home=/etc/spamassassin/.razor -discover
- test spamassassin https://www.alibabacloud.com/blog/how-to-setup-spamassassin-with-postfix-on-ubuntu-16-04_594878

## Testing
- FB login
- Stripe Donations
- Google maps add address
- LetsEncrypt
