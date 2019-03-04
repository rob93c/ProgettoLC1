module Main where

import AbsPascal
import Parser
import Checker
import Printer

--Se la lista contenente gli errori è vuota,  manda in esecuzione il metodo prettyPrinter
--Altrimenti vengono stampati a console gli errori contenuti nella lista
main = do 
   inStr <- getContents
   let syntaxTree = myparse inStr in case (execution syntaxTree) of 
  	[] -> putStrLn(prettyPrinter syntaxTree) 
  	xs ->  putStrLn $ foldl (\st err -> st ++ "\n" ++ err) "" xs ++ "\n" 

   