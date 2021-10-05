## Install
x nginx
x sbcl (through apt-get)
x git
x postfix
x ca-certificates
- spamassassin, pyzor, razor
x tarsnap
x vim-nox
x certbot
x openssl
x imagemagick
x screen
x libssl-dev (if quicklisp can't find package libssl.so)
x python3-pip
x git clone https://github.com/miracle2k/tarsnapper
x pip3 install tarsnapper

## Edit settings.lisp file
x new local ip address
x correct path to image magic convert
x *production-p* t
x new "+base-url+"

## Create cron jobs
x for all routes that happen periodically (activity reminders, etc)
x for tarsnap

## After server switchover
x deactivate old server cron jobs
x stop and disable nginx on old server
X stop and disable postfix on old server

## Tarsnap
- https://www.tarsnap.com/pkg-deb.html
- https://www.tarsnap.com/faq.html#moving-computers

## SlimV
x make sure correct version of python is installed

## SSH and SSL? what's needed from
x /etc/ssh
x /etc/ssl
x sudo certbot --nginx -d kindista.org -d www.kindista.org
x sudo certbot delete --cert-name k4.kindista.org

## DNS
x make a temporary k#.kindista.org A and AAA record to point to the new server
x after installation and testing change A and AAAA records for kindista.org to point to the new server
x change DNS for kindista.com, kindista.net

## Git repositories
x kindista directory
x books directory (financial statments)
x test push git origin new server
x test git pull master

## Make sure root directory has all necessary subdirectories
x media/blog
x media/avatar
x media/images
x data/
x rsync -av lisp@kindista.org:kindista/data/images/ /home/lisp/kindista/data/images/
x scp lisp@kindista.org:kindista/data/db\* .
x (load-db)

## Make sure to transfer
x /etc/ssl
x /usr/local/etc/nginx/nginx.conf or /etc/nginx/conf
x enable letsencrypt and dhparam lines in nginx.conf
x enable certbot in nginx.conf after transfering domain
x /etc/tarsnap.conf (or /usr/local/etc/tarsnap/tarsnap.conf)
- /usr/local/tarsnap-cache
x /etc/aliases
x /etc/postfix/main.cf
x /etc/postfix/header_checks
x /etc/postfix/local_domains
x /etc/postfix/sasl_passwd
x /etc/spamassassin/local.cf
x /etc/default/spamassassin
x spamassassin lines in /etc/postfix/master.cf

## NGINX
x systemctl enable nginx.service

## Changing IP address
x new local ip address in settings.lisp (same as above)
? change IP in https://developers.facebook.com/apps/779034685520810/settings/advanced/ (when signed in as a developer)
x change IP in https://console.developers.google.com/apis/credentials/key/0?project=kindista-1296&authuser=2 (when signed in as commongoods' google account)
x Add new IP addresses to googlemaps api key settings: https://console.cloud.google.com/apis/credentials?project=kindista-1296&supportedpurview=project

## Postfix and Mailgun
x sudo postmap /etc/postfix/sasl_passwd
X touch /etc/postfix/generic
x postmap /etc/postfix/generic (to create error db file)
x postmap /etc/postfix/local_domains (local_domains needed to prevent spoofing of kindista.org)
x sudo service postfix restart
x systemctl enable postfix.service

## Install spamassassin
x apt-get install spamc spamassassin
X edit /etc/default/spamassassin 'ENABLED=1'
x groupadd -g 5555 spamd
x useradd -u 5555 -g spamd -s /sbin/nologin -d /usr/local/spamassassin spamd
x mkdir -p /usr/local/spamassassin/log
x chown spamd:spamd -R /usr/local/spamassassin
x systemctl enable spamassassin.service
- https://syslint.com/blog/tutorial/how-to-install-and-configure-spamassassin-with-postfix-in-debian-8/
- mkdir /etc/spamassassin/.razor
- razor-admin -home=/etc/spamassassin/.razor -register
- razor-admin -home=/etc/spamassassin/.razor -create
- razor-admin -home=/etc/spamassassin/.razor -discover
- test spamassassin https://www.alibabacloud.com/blog/how-to-setup-spamassassin-with-postfix-on-ubuntu-16-04_594878

## Testing
x FB login
x Stripe Donations
x Google maps add address
x LetsEncrypt
