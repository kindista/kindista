# ![Kindista](http://media.kindista.org/email-logo.png)

[![Join the chat at https://gitter.im/kindista/kindista](https://badges.gitter.im/kindista/kindista.svg)](https://gitter.im/kindista/kindista?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Kindista ([kindista.org](https://kindista.org)) is a social network for local resource sharing. 
It helps people develop trust, opportunities, and mutual generosity within their real-world communities. 

With Kindista you can:

- Share resources (such as skills, tools, materials, living space, or work space)
- Show gratitude for what others have shared
- Connect with other people who are contributing to a new culture of sharing in your community
- See in real-time what kinds of sharing are happening near you.

## Getting Started ##

This document assumes that you have a running Common Lisp development environment. 

Kindista has dependencies on a few [Steel Bank Common Lisp](http://www.sbcl.org) libraries so you will have to be running SBCL as your lisp implementation for Kindista to work.

We also recommend that you install [slimv](http://kovisoft.bitbucket.org/tutorial.html) (for [vim](http://www.vim.org) users) or [slime](http://common-lisp.net/project/slime/) (for [Emacs](http://www.gnu.org/software/emacs/) users) before trying to get Kindista up and running.

You will need to copy the settings.lisp.example file to "settings.lisp" and replace "/srv" in the +db-path+ and +avatar-path+ constants to the name of the directory where you put your copy of Kindista.

All of Kindista's dependencies are listed in the [kinidista.asd](http://https://github.com/kindista/kindista/blob/master/kindista.asd) file.
We recommend using [quicklisp](http://www.quicklisp.org/beta/) to load Kindista and its packages in your slime or slimv [REPL](http://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop).

Create a symbolic link from somewhere on your Quicklisp load-path to the directory containing kindista.asd.

Kindista does not serve static media. You will need to configure your webserver (e.g. nginx) as follows:

    server {
        listen       80 default;
        server_name  localhost;

        charset utf-8;

        location @proxy {
            proxy_pass http://localhost:5000;
        }

        location / {
            root   /srv/kindista/html/;
            index  index.html;
            try_files $uri @proxy;
        }

        location /media/ {
            alias /srv/kindista/media/;
            index index.html;
            error_page 404 /404.html;
        }

        location /media/avatar/ {
            alias /srv/kindista/media/avatar/;
            index index.html;
            error_page 404 /media/avatar/none.png;
        }

        error_page   500 502 503 504  /50x.html;
        location = /50x.html {
            root   /srv/kindista/html/;
        }
    }

If you run multiple nginx servers, you may find it convenient to reconfigure the header as follows: 

    server {
        listen       80;
        server_name  kindista.local;

    ...

And add a line to your /etc/hosts file:

    # kindista development
    127.0.0.1 kindista.local

After you've completed the above steps, open one of the Kindista lisp files and fire up slime or slimv (type ",c" to start slimv).

In the REPL, type:
    
    (ql:quickload :kindista)

    (in-package :kindista)

    (run)

That's it.
Assuming you made the correct changes to your server configuration files, Kindista should be up and running on your local server.

## Contributions ##

We love community contributions!  
If you want to help out, please contact us at [info@kindista.org](mailto:info@kindista.org).
