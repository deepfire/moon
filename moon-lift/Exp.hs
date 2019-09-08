{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import           Data.Dynamic
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Maybe (fromMaybe)
import           Data.Proxy
import           Type.Reflection

import           ExpMod

pp :: Pipe -> (PipeKey, Pipe)
pp p = (pKey p, p)

pipes :: Pipes
pipes = Pipes $ Map.fromList
  [
    pp $ mkFull (pure (1 :: Int))
  , pp $ mkLink (\(x::Int)-> pure $ show x)
  , pp $ mkLink (\(x::String)-> putStrLn x)
  ]

findPipe :: String -> Pipe
findPipe name = fromMaybe (error $ "No pipe: " <> name)
  (Map.lookup (readPipeName name) pipeMap)
  where
    Pipes pipeMap = pipes
    readPipeName "Int"  = KeyFull (someTypeRep $ Proxy @Int)
    readPipeName "show" = KeyLink (someTypeRep $ Proxy @String) (someTypeRep $ Proxy @Int)
    readPipeName "puts" = KeyLink (someTypeRep $ Proxy @())     (someTypeRep $ Proxy @String)

runPipe :: Pipe -> IO ()
runPipe (Pipe dyn _) =
  case fromDynamic dyn :: Maybe (PipeFun () ()) of
    Nothing       -> error "Incomplete pipe."
    Just (Full x) -> x

c :: Pipe -> Pipe -> Pipe
c x y = case compose x y of
          Left e  -> error e
          Right x -> x

f = findPipe

main :: IO ()
main = do
  runPipe $ f "Int" `c` f "show" `c` f "puts"
