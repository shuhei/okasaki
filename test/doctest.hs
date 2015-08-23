module Main where

import Test.DocTest

main :: IO ()
main = doctest[ "src/Okasaki/List.hs"
              , "src/Okasaki/BinaryTree.hs"
              ]
