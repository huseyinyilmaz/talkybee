talkybee
========
|build|_

An instant chat room service. (Source code for http://www.talkybee.com)

How to build a release
----------------------

To use talkybee in a production system, you should package it. Your package will include all dependencies including erlang runtime itself. So you won't need erlang installed on your production system in order to run it. To generate a package first download source code from github.

::

   $ git clone git://github.com/huseyinyilmaz/talkybee.git

After this you should download dependencies

::

   $ make init

Now you can generate a package for your system.

::

   $ make pack

talkybee.tar.gz will be generated in project root directory (current directory). You can untar it anywhere in your target system and run it with 

::

   $ bin/talkybee start

How to initialize for development
---------------------------------

If you want to checkout the code you can initialize system for development. In addition to production dependencies.
talkybee uses sync to reload the a module when a code change occurs. So you should also download that development dependency. Also some development specific code must be compiled.

::

   $ make initdev

This will download development dependencies. After this you can start server in development mode with

::

   $ make start   

You can also run eunit tests with the following.

::

   $ make eunit

.. |build| image:: https://travis-ci.org/huseyinyilmaz/talkybee.png
.. _build: https://travis-ci.org/huseyinyilmaz/talkybee
