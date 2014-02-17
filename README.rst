talkybee
========

An instant chat room service. (Source code for http://www.talkybee.com)

How to get the code
-------------------

First of all clone the repository to your local file system.

::

   $ git clone git://github.com/huseyinyilmaz/talkybee.git

After that get javascript dependencies with bower (bower must be installed.)

::
   $ bower install

How to configure
----------------

talkybee is a full client side application that uses publicator as backend. In order to use talkybee, You must configure js/chat.js and change publicator address. Here is the variable that you should change in js/chat.js:

::

   (function(){
       "use strict";

       // Change this variable to connect to different publicator host
       var _PUBLICATOR_SERVER = 'www.talkybee.com:8766';
       ...

Change _PUBLICATOR_SERVER variable to your server address.

How to run
----------

Since talkybee is a static app you can just serve project from your favorite web server.

Important notice
----------------

This version of talkybee requires publicator v0.1.0
