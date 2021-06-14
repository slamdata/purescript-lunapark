# purescript-lunapark

[![Latest release](http://img.shields.io/github/release/slamdata/purescript-lunapark.svg)](https://github.com/slamdata/purescript-lunapark/releases)
![Build Status](https://github.com/slamdata/purescript-lunapark/actions/workflows/ci.yml/badge.svg)

## Disclaimer

+ This is WIP: some things weren't implemented, some might have bugs. Filling issues and prs are welcomed!

## Purescript W3C webdriver protocol bindings

The main purpose of this library is to provide purescript webdriver/json-wire protocol bindings, such that
won't involve any JavaScript dependencies.

## Architecture

+ There is a module that encodes any possible error returned by Selenium.
+ Low-level API uses `purescript-argonaut`'s `Json` type and `purescript-affjax`s helper functions (`get`, `post` etc)
+ Endpoints are constructed using `Endpoint@Lunapark.Endpoint` type. This module also provides helper functions
returning `Aff (Either Error Json)`
+ Types of requests and resposes live in `Lunapark.Types` module.
+ High-level API uses `purescript-run`. There are several functors, but the main two are: `LunaparkF` and `ActionF`.
This functor embed in `Run` encode almost all posible interactions with w3c/selenium server.
+ At first `ActionF` functor (that is a functor that encodes all posible actions like click, hover) is interpreted to
`LunaparkF`, then `LunaparkF` is interpreted into `Run` with `EXCEPT` and `AFF|EFF` effects.
+ `Lunapark` is facade library on top of api, functors and error.
+ Capabilities are represented via gradually typed datastructure `Array Capability`. `init` function takes a record
`{ alwaysMatch: Array Capability, firstMatch: Array (Array Capability) }`. `firstMatch` must have at least on array.
+ To construct interpreter you need call `init@Lunapark.API`. The result would be `Aff (Either Error Interpreter)`,
where `Interpreter` is function taking `Lunapark` monad and unwrapping it into `Run` with `EXCEPT` and `AFF|EFF`. E.g.

```purescript
main = launchAff do
  interpreterOrError ← init "http://localhost:4444/wd/hub" { alwaysMatch: [], firstMatch: [[BrowserName Chrome]] }
  case intepreterOrError of
    Left err → throwError $ error "An error during selenium session initialization occured"
    Right interpret → Run.runBaseAff' $ Run.runExcept $ interpret do
      go "http://my-site.com"
      el ← findElement $ ByCss $ CSS.fromString ".my-class"
      clickEl el
      login ← findElement $ ByCss $ CSS.fromString "#login"
      password ← findElement $ ByCss $ CSS.fromString "#password"
      moveTo { origin: login, x: 0, y: 0, duration: Milliseconds 1000.0 }
      click
      sendKeys "login"
      moveTo { origin: password, x: 0, y: 0, duration: Milliseconds 100.0 }
      sendKeys "secret"
      sendKeys "\\xE007"
```
+ The interpreter takes care about implemented and non-implemented features of particular driver. E.g. if driver supports
`GET@/session/:sessionId/timeouts` via W3C standard it will use it otherwise the interpreter makes it a chain call of
JsonWire commands. The same works for actions: they are interpreted into either `POST@/session/:sessionId/action` or
chains of JsonWire commands.
