module Main where

-- import Lib (mainFunc)
import Ex1 (mainFunc)
import Prelude

main :: IO ()
main = putStrLn "hello" >> 
    Ex1.mainFunc
