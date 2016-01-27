

Math game written in [Elm](http://www.elm-lang.org)
===================================================

###[Play it here!](http://arturopala.github.io/math-game-elm/) or find it [here](http://builtwithelm.co/)

##Prerequisites

-   Node.js > 4.0
-   run `npm install` or `npm install -g elm onchange ws`

##Build

-   Run `elm-make` to compile `elm.js`, or

-   Run `npm run compile` to start watching sources and recompile on the fly.

```
onchange '*.elm' -v -- elm-make Main.elm --output elm.js
```

##Test

-   Run `npm test`

##Run
-   Run `npm run start` or `ws` to run embeded http server, then see game in the browser <http://127.0.0.1:8000>

##Deploy

[![Deploy](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy)
