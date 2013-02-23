__Kindista__

Kindista is a social network for local resource sharing. 
It helps people develop trust, opportunities, and mutual generosity within their real-world communities. 

With Kindista you can:

- Share resources (such as skills, tools, materials, living space, or work space)
- Show gratitude for what others have shared
- Connect with other people who are contributing to a new culture of sharing in your community
- See in real-time what kinds of sharing are happening near you.

A pilot version of Kindista was released for testing in Eugene, OR in June 2012.
Visit [kindista.org](http://kindista.org) for more information.
The pilot version has not been released and is fundamentally incompatible with the version available on GitHub. 
The Kindista pilot software will be replaced by this version upon its completion and user data will be migrated.

Please check out [the plan](https://github.com/kindista/kindista/blob/master/PLAN) for information regarding what needs to be finished in this version of Kindista before it will be released publicly.


## Getting Started ##

This document assumes that you have a running Common Lisp development environment. 

We recommend that you first install [slimv](http://kovisoft.bitbucket.org/tutorial.html) (for [vim](http://www.vim.org) users) or [slime](http://common-lisp.net/project/slime/) (for [Emacs](http://www.gnu.org/software/emacs/) users).

You will need to copy the settings.lisp.example file to "settings.lisp" and replace "/srv" in the +db-path+ and +avatar-path+ constants to the name of the directory where you put your copy of Kindista.

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

All of Kindista's dependencies are listed in the [kinidista.asd](http://https://github.com/kindista/kindista/blob/master/kindista.asd) file.
We recommend using [quicklisp](http://www.quicklisp.org/beta/) to load Kindista and its packages in your slime or slimv [REPL](http://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop).

After you've completed the above steps, open one of the Kindista lisp files and fire up slime or slimv (type ",c" to start slimv).

In the REPL, type:
    
    (ql:quickload :kindista)

    (in-package :kindista)

    (run)

That's it.
Assuming you made the correct changes to your server configuration files, Kindista should be up and running on your local server.

## Contributions ##

We love community contributions!  
If you want to help out, please contact us at [info@kindista.org](mailto:info@kindista.org) or hit us up on IRC (#kindista on Freenode).
