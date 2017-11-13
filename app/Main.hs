{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

runApp :: IO ()
runApp =  putStrLn "Hello world!"

main :: IO ()
main = runApp
{-# INLINABLE main #-}

foreign export ccall runApp :: IO ()