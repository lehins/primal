# primal

**Warning** - it is still in experimental stage.

`primal` is a ground up design of a new interface for mutable and effectful operations
that uses industry best practices as well as novel approaches to improve usability and
performance.

## Status

| Language | Travis | Azure | Coveralls |
|:--------:|:------:|:--------:|:---------:|
| ![GitHub top language](https://img.shields.io/github/languages/top/lehins/primal.svg) | [![Travis](https://travis-ci.com/lehins/primal.svg?branch=master)](https://travis-ci.com/lehins/primal) | [![Build Status](https://dev.azure.com/kuleshevich/primal/_apis/build/status/lehins.primal?branchName=master)](https://dev.azure.com/kuleshevich/primal/_build?branchName=master) | [![Coverage Status](https://coveralls.io/repos/github/lehins/primal/badge.svg?branch=master)](https://coveralls.io/github/lehins/primal?branch=master)

|      Package       | Hackage | Nightly | LTS |
|:-------------------|:-------:|:-------:|:---:|
|  [`primal`](https://github.com/lehins/primal/tree/master/primal)|                                       [![Hackage](https://img.shields.io/hackage/v/primal.svg)](https://hackage.haskell.org/package/primal)|                                                                                                        [![Nightly](https://www.stackage.org/package/primal/badge/nightly)](https://www.stackage.org/nightly/package/primal)|                                                                                         [![Nightly](https://www.stackage.org/package/primal/badge/lts)](https://www.stackage.org/lts/package/primal)|
|  [`primal-memory`](https://github.com/lehins/primal/tree/master/primal-memory)|                                       [![Hackage](https://img.shields.io/hackage/v/primal.svg)](https://hackage.haskell.org/package/primal-memory)|                                                                                                        [![Nightly](https://www.stackage.org/package/primal-memory/badge/nightly)](https://www.stackage.org/nightly/package/primal-memory)|                                                                                         [![Nightly](https://www.stackage.org/package/primal-memory/badge/lts)](https://www.stackage.org/lts/package/primal-memory)|
|  [`primal-test`](https://github.com/lehins/primal/tree/master/primal-test)|                            [![Hackage](https://img.shields.io/hackage/v/primal-test.svg)](https://hackage.haskell.org/package/primal-test)|                                                                                              [![Nightly](https://www.stackage.org/package/primal-test/badge/nightly)](https://www.stackage.org/nightly/package/primal-test)|                                                                               [![Nightly](https://www.stackage.org/package/primal-test/badge/lts)](https://www.stackage.org/lts/package/primal-test)|


## What is it all about?

"Haskell is the world’s finest imperative programming language." - SPJ

It is hard to argue with this statement. However, despite that Haskell makes effectful
code much safer, it has never been very friendly when it actually came to writing
imperative programs. It seems like that most of the design time is spent on three distinct
spectrums:

* At the very top is the purely functional code. These are your `Num`s, `Foldable`s,
  `Applicative`s etc. In other words the warm and fuzzy world of immutability, idempotency
  and refertial transperency.
* At the very bottom is the code that interfaces with real world. All of the `IO` stuff that
  you can think of: file system, networks, databases, concurrency etc.
* And the stuff in the middle. All the various effect systems that try to somehow marry
  the above two paradigms into one cohesive and usable abstraction. These are your
  `tranformers`, `mtl`s, `polysemy`s, `fused-effects` etc.

Incidentally, there is a whole spectrum that is usually overlooked and neglected. Mutable
data structures. Mutable operations on them run in `ST` monad or possibly in `IO`,
but often with safe escape hatches to the warm and fuzzy world of immutability. This
spectrum can even sometimes encapsulate parallelization, which can also yield
determinsitic and pure results, when implemented properly.

This project attempts to augment Haskell's imperative capabilities by filling this
mutable spectrum, while providing interoperability with the rest of the Haskell ecosystem.

## Prior art

The notable and predominantly used packages in this area of mutability that we are trying
to improve are:

* `primitive`/`vector` and derivatives
* `basement`/`memory` project.

There are a few other packages that touch up on this space, but none of them succeed in
providing a solution.


Without attacking the root of the problem we cannot make the situation better. For that
reason we also will look at some of the techniques, which were borrwoed from other
packages: `unliftio`, `unlift`, `exceptions`.

## New approach

There are a few crucial and novel approaches this package employs:

* All mutable types have their state token type argument ordered at the end: `MArray a s`
  vs `MArray s a`
* Unboxing of complex types

