module Main where

import Data.List
np a b = let l = [c^d | c<-[2..a],d<-[2..b]] in length $ nub l
