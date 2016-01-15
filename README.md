

Math game written in (Elm)[http://www.elm-lang.org]
===================================================

##Build

`npm install -g elm onchange ws`

##Development

Run `elm-make` to compile `elm.js`.

Run `onchange` to recompile on fly.
```
onchange '*.elm' -v -- elm-make Main.elm --output elm.js
```

Run `ws` to see game in browser.

##Deploy

[![Deploy](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy)