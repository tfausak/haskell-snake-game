# [haskell-snake-game][]

This is a version of [the snake game][] written in Haskell. It is rendered
using OpenGL.

I wrote this to familiarize myself with [the Gloss package][] and to dip my
toes into game programming. I have never written a game before, so the code is
probably atrocious. I apologize in advance.

This package will probably never be on Hackage, so you'll get [Git][] to
install it. I also recommend [Stack][], but you should be able to use [Cabal][]
if you want to.

Here's how to get started:

``` sh
> git clone https://github.com/tfausak/haskell-snake-game
> cd haskell-snake-game
> stack build --install-ghc
> stack exec haskell-snake-game
```

![Gameplay screenshot][]

The head of the snake is orange. The food is green. Use the arrow keys to move
around. Try not to run into walls or eat yourself.

[haskell-snake-game]: https://github.com/tfausak/haskell-snake-game
[the snake game]: https://en.wikipedia.org/wiki/Snake_(video_game)
[the gloss package]: https://hackage.haskell.org/package/gloss
[git]: https://git-scm.com
[stack]: https://github.com/commercialhaskell/stack
[cabal]: https://www.haskell.org/cabal/
[gameplay screenshot]: https://i.imgur.com/C1pWF0Y.png
