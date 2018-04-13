koivu
=====

An interactive tree representation of [AlloMedia](https://www.allo-media.fr/)'s
Customer Path ([demo](https://allo-media.github.io/koivu/)).

![](https://i.imgur.com/SU7mcqK.png)

This repository holds code for:

- An [Elm package] containing the logic of the component
- A [npm package] containing the SASS styles for the component

> Note: while open sourced, these packages are most likely to be addressing the
sole own needs of Allo-Media.

## Minimal program

```elm
module Main exposing (..)

import Koivu
import Koivu.Tree as Tree
import Koivu.Settings exposing (Settings)
import Html

settings : Settings
settings =
    { autoNormalize = False
    , globalQty = 100000
    , minNodeQty = 3000
    , maxChildren = 4
    , maxGlobalQty = 200000
    , maxLevels = 3
    , nodeWidth = 140
    , nodeHeight = 80
    , nodePadding = 10
    }

main : Program Never Koivu.Model Koivu.Msg
main =
    Tree.demoTree
        |> Koivu.setup settings
        |> Html.program
```

You'll find a more elaborate integration example in
[the `example` subfolder](https://github.com/allo-media/koivu/tree/2.0.0/example).

## Running the demo locally

A demo is available from the `example` folder:

```
$ cd example
$ npm i
$ npm start
```

Then head to [localhost:3000](http://localhost:3000).

## License

MIT

[Elm package]: http://package.elm-lang.org/packages/allo-media/koivu
[npm package]: https://www.npmjs.com/package/koivu-styles
