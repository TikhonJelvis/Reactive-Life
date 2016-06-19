# A Reactive Game of Life

This is an interactive implementation of John Conway's [Game of Life][1], used as a demo to explain (simple) functional reactive programming (FRP). It was written for my talk on the subject at the East Bay Haskell meetup in Berkeley.

[1]: http://en.wikipedia.org/wiki/Conway%27s_Game_of_Life

The interface is written using the `threepenny-gui` library which uses a web browser as a display. It works by running a webserver locally and sending commands to a simple client running in JavaScript. It's easy to setup and doesn't require any tricky dependencies.

As a nice bonus to this particular approach, it comes with an FRP library built in (heavily based on `reactive-banana`—both are by the same author).
