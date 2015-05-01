module Main where

lazyEval 0 b = 1
lazyEval _ b = b
