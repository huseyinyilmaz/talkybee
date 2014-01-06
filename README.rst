talkybee
========

An instant chat room service. (Source code for http://www.talkybee.com)

How to get the code
-------------------

Just clone the repository to your local file system.

::

   $ git clone git://github.com/huseyinyilmaz/talkybee.git

How to configure
----------------

talkybee is a full client side application that uses publicator as backend. In order to use talkybee:

* You must configure js/chat.js and change publicator address. Here is the variable that you should change

::

   (function(){
    "use strict";
    // var _PUBLICATOR_SERVER = 'localhost:8766';
    var _PUBLICATOR_SERVER = 'www.talkybee.com:8766';
    ...

Change _PUBLICATOR_SERVER variable to your server address.

* Change publicator client and publicator chat client addresses from html. Just goto end of the index.html file and change publicator-chat and publicator-client scripts to your publicator server. Publicator do not generate those scripts (Which means they are static files), so you can copy them with your static files here is the links that you should change in your html file

::

    <!-- <script src="http://localhost:8766/js/publicator-client.js"></script>
    <script src="http://localhost:8766/js/chat-client.js"></script> -->
    <script src="http://www.talkybee.com:8766/js/publicator-client.js"></script>
    <script src="http://www.talkybee.com:8766/js/chat-client.js"></script>


How to run
----------

Since talkybee is a static app you can just serve project from your favorite web server.
