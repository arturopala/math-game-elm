

Math game written in [Elm](http://www.elm-lang.org)
===================================================

###[Play it here!](http://arturopala.github.io/math-game-elm/) or find it [here](http://builtwithelm.co/)

##Prerequisites
-   Node.js > 4.0
-   run `npm install` or `npm install -g elm onchange ws`

##Build
-   run `elm-make src/Main.elm --output assets/elm.js`, or
-   run `npm run compile` to start watching sources and recompile on the fly.

```
onchange '**/*.elm' -v -- elm-make src/Main.elm --output assets/elm.js
```

##Test
-   run `npm test`

##Run
-   run `npm run start` or `ws` to run embeded http server, then try game in the browser <http://127.0.0.1:8000>

##Deploy

[![Deploy](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy)
