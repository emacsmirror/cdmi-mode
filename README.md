CDMI Major Mode
===============

An Emacs major mode for [CDMI][cdmi].

Provides syntax highlighting of CDMI content and functions for loading CDMI
content.

The intent is to create an Emacs working environment for CDMI.

Configuration
-------------

You can customize `cdmi-mode` for your use through Emacs by running:

    cdmi-mode-customize
    
At a minimum, you will need to specify the server to connect to. This can
be accomplished either through the customize interface or may be set
directly in your Emacs initialization:

    (setq cdmi-server "cdmi.example.com")
    
Authentication to the server is handled by Emacs' URL package. If required,
you should be prompted for credentials.


Bugs and Known issues
---------------------

When using GnuTLS to connect, I have seen errors dumped into the output. I
have added code to eliminate this error messages, but it is completely a
hack.

I have found that initial calls to `cdmi-open` fail to retrieve data at
times. Executing the command a second time resolves this. I'm continuing to
look into why this happens.

Please file bugs at <http://github.com/jeastman/cdmi-mode/issues>

[cdmi]: http://cdmi.sniacloud.org/
