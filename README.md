**Note:** This branch is based on an old version of the code from 2012 and probably won't build on modern systems. (WX is a real pain!) The `master` branch has a new version of the code I wrote for a 2016 talk, updated to use `threepenny-gui`.

# A Reactive Game of Life

This is an old demo of functional reactive programming (FRP), originally built to illustrate the abstraction for a class project in CS 263 at Berkeley. It uses `reactive-banana` and wxWidgets, which might be hard to build.

[1]: http://en.wikipedia.org/wiki/Conway%27s_Game_of_Life

The actual—surprisingly simple—implementation of the game itself was largely influenced by [some slides][2] by a kizzx2 I found online. Repa's stencil API is natural for the Game of Life, and the Game of Life is a nice illustration of the API.

[2]: http://illustratedhaskell.org/index.php/2011/09/24/conways-game-of-life-with-repa/

The interesting part of the project is the interface, which is written in a functional reactive style using [reactive-banana][3]. This is really primarily a demo of how simple and concise functional reactive programming can be. 

[3]: http://www.haskell.org/haskellwiki/Reactive-banana

This is actually my very first FRP program, so it's entirely a learning experience; I've found FRP to be very easy to pick up so far. It's certainly easier than the old event-driven style I used in the past with frameworks like Swing and Tk. In fact, the FRP code has been extremely simple to write—all the actual difficulties come from having to interface with wxWidgets.
