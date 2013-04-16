talkybee
========
|build|_

An instant chat room service. (Source code for http://www.talkybee.com)

How to build
------------

 To build the project first call 'make init' to download dependencies and then call 'make pack' to generate package. After that you should have talkybee.tar.gz package on your current directory. You can use 'bin/talkybee start' to start the project.

::

   $ # initialize project
   $ make init
   ...
   ...
   $ # generate package
   $ make pack
   ...
   ...

.. |build| image:: https://travis-ci.org/huseyinyilmaz/talkybee.png
.. _build: https://travis-ci.org/huseyinyilmaz/talkybee
