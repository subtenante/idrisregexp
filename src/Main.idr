module Main

import Effects
import Effect.File
import Effect.StdIO
import Effect.System

import RegExpParser
import RegExp
import Substring


printUsage : {[STDIO]} Eff ()
printUsage = putStrLn "Usage: igrep [REGEXP] [FILELIST]"

implicit str2regexp : String -> RegExp
str2regexp s = case (parse pExp s) of
               Right r => r
               _       => Zero

search : RegExp -> String -> String
search e s with (subStringDec e (map toNat (unpack s)))
  search e s | Yes _ = s
  search e s | No _ = ""


readFile : { [FILE R] } Eff (List String)
readFile = readFile' []
           where
             readFile' : List String -> { [FILE R] } Eff (List String)
             readFile' acc = if (not (! eof)) then
                                do Result line <- readLine | _ =>  pure (reverse acc)
                                   readFile' (line :: acc)
                             else pure (reverse acc)

searchLines : RegExp -> { [FILE R] } Eff (List String)
searchLines e = do
                 ls <- readFile
                 pure (filter (not . isNil . unpack) (map (search e) ls))

searchFile : RegExp -> String -> {[FILE ()]} Eff (List String)
searchFile e f
     = case !(open f Read) of
         Success => do
           xs <- searchLines e
           close
           pure xs


searchFiles : RegExp -> List String -> {[STDIO, FILE ()]} Eff (List String)
searchFiles _ [] = pure []
searchFiles e (f::fs)
     = do
         r <-  searchFile e f
         rs <- searchFiles e fs
         pure (r ++ ["\n"] ++ rs)


process : List String -> {[STDIO, FILE ()]} Eff ()
process [] = pure ()
process [ x ] = printUsage
process (x :: e :: []) = printUsage
process (x :: e :: fs) with (parse pExp e)
  process (x :: e :: fs) | Left err = putStrLn ("Parser error on:" ++ err)
  process (x :: e :: fs) | Right r
          = do
             printLn (show r)
             ss <- searchFiles r fs
             putStrLn (concat ss)

interface_ : {[STDIO, SYSTEM, FILE ()]} Eff ()
interface_ = do
              putStrLn "IGrep - grep for Idris"
              args <- getArgs
              process args

main : IO ()
main = run interface_
