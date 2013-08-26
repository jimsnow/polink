# Polink  
## The Social Network Anyone Can Edit
### http://polink.org

## About

This is the software behind the polink.org website.  It is written in Haskell and uses [acid-state](http://acid-state.seize.it/) for persistence and [Yesod](http://www.yesodweb.com/) as the web framework.

## Compiling

I use ghc version 7.6.1; you may have trouble with older versions.  There's a list of dependencies a mile long; the major once are acid-state, yesod, and lens.  I never bothered to create a cabal file, instead using just "ghc --make Polink.hs", which has worked for me so far.  (You might want to throw a -O2 in there, but so far I haven't had any reason to worry about performance.)

## Running your own instance

If you want to run your own instance of Polink, you'll have to do a bit of work to search and replace wherever we hard-code "http://polink.org", and all the site documentation is part of the application as well.

The css and js files are not served up by Polink directly, you'll need to run nginx or something for static content.

The Polink binary doesn't require any command line arguments.  It just starts up and listens on port 3001.  (You can change the default in Polink.hs or use nginx or similar as a proxy.)

## About the reputation system

The reputation system we use on Polink.org is a proprietary software package called Pariah.  You can read about it [here](http://metamocracy.com/technology).  Polink was designed in part as a technology demo to show how Pariah works in a practical setting.

Fortunately, you don't need to run Pariah in order to have a working, usable instance of Polink.  The file polink.js handles the UI aspects of the reputation system; if you disable the javascript, it won't affect anything else on the site.

## Details

This software was created by Metamocracy LLC and is available under a GPLv2 license.
If you have any questions, please email jsnow@metamocracy.com.
