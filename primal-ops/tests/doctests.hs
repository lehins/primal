{-# LANGUAGE CPP #-}

module Main where

#if __GLASGOW_HASKELL__ >= 802 && __GLASGOW_HASKELL__ < 810

import Test.DocTest (doctest)

main :: IO ()
main = doctest ["-fobject-code", "src", "internal"]

#else

-- TODO: fix doctest support
main :: IO ()
main = putStrLn "\nDoctests are not supported for ghc version 8.2 and prior as well as 8.10 onwards\n"

#endif