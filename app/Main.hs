-- Sisserou -- A linguistic toy based on System F Omega   --
--                                                        --
-- Copyright 2014--2024 K.D.P.Ross <KDPRoss@gmail.com>    --
--                                                        --
-- This codebase is licensed for the following purposes   --
-- only:                                                  --
--                                                        --
-- - study of the code                                    --
--                                                        --
-- - use of the unaltered code to compile the interpreter --
--   for noncommercial educational and entertainment      --
--   purposes only                                        --
--                                                        --
-- - gratis redistribution of the code in entirety and in --
--   unaltered form for any aforementioned purpose        --
--                                                        --
-- The code may not be used for any other purposes,       --
-- including but not limited to:                          --
--                                                        --
-- - any commercial purpose                               --
--                                                        --
-- - use by any governmentally-affiliated organisation    --
--                                                        --
-- - connection to any external system for any useful     --
--   purpose whatsoever                                   --



module Main where

import Control.Monad(foldM)
import Control.Exception(try, SomeException)
import Sisserou((+>), (|-), (|:-), (=~=), (==>), envEmpty,
       getSt, runTypingMonad, liftTypingMonad, Input(..),
       Exp(Fix), Typing(..), Type, Kind, TypingMonad,
       inNormalForm, Kind(Star), Env, partialEvaluateTypes)
import Text.ParserCombinators.HuttonMeijer(papply)
import Parser(pInput)
import System.IO(hFlush, stdout)
import Eval(eval)

-- ===== The REPL ===== --

main :: IO ()
main = do mapM_ putLn banner
          putLn ""
          loopDefault
          return ()
         where loopDefault  = loop envEmpty envEmpty envEmpty 0
               loop d g s i = do putStr "$> "
                                 hFlush stdout
                                 text <- getLine
                                 let quitAction   = do putLn "Exiting!"
                                                       return ()
                                 let resetAction  = do putLn "State has been reset."
                                                       loopDefault
                                 let helpAction   = do mapM_ putLn [ "help / :h / ?          - display this help",
                                                                     "{load / :l} <filename> - load a file" ,
                                                                     "quit / :q              - exit the interpreter",
                                                                     "reset / :r             - reset the interpreter state"
                                                                   ]
                                                       loop d g s i
                                 let loadAction f =  let foldableProcessLine (d, g, s, i) inp = case runTypingMonad (processLine inp (d, g, s)) i of
                                                                                                  TypeData (d', g', s', i', msg) -> do putLn msg
                                                                                                                                       return (d', g', s', i')
                                                                                                  TypeError ss                   -> do mapM_ putLn ss
                                                                                                                                       return (d, g, s, i)
                                                     in
                                                     do ssMaybe <- readLinesAutoextension f
                                                        case ssMaybe of
                                                          Nothing -> putLn $ "Unable to read file `" ++ f ++ "`."
                                                          Just ss -> do inps             <- mapM parseString . stripComments $ ss
                                                                        (d', g', s', i') <- foldM foldableProcessLine (d, g, s, i) $ successes inps
                                                                        loop d' g' s' i'
                                 case text of
                                   "quit"  -> quitAction
                                   ":q"    -> quitAction
                                   "reset" -> resetAction
                                   ":r"    -> resetAction
                                   "help"  -> helpAction
                                   "?"     -> helpAction
                                   ":h"    -> helpAction
                                   _       | "load " == take 5 text -> loadAction . drop 5 $ text
                                   _       | ":l " == take 3 text   -> loadAction . drop 3 $ text
                                   _                                ->  do m <- parseString text
                                                                           case m of
                                                                             Just inp -> case runTypingMonad (processLine inp (d, g, s)) i of
                                                                                           TypeData (d, g, s, i, msg) -> do putLn msg
                                                                                                                            loop d g s i
                                                                                           TypeError ss               -> do mapM_ putLn ss
                                                                                                                            loop d g s i
                                                                             Nothing  -> loop d g s i

banner = [ "Welcome to Sisserou",
           "  Copyright 2K14--2K18 K.D.P.Ross"
         ]



-- ===== Process a Single Line ===== --

processLine :: Input -> (Env Kind, Env Type, Env Exp) -> TypingMonad (Env Kind, Env Type, Env Exp, Integer, String)
processLine (NewType t k) (d, g, s)  = do d' <- case d ==> t of
                                                  TypeError _ -> return $ (t, k) +> d
                                                  _           -> fail $ "Cannot create type constructor `" ++ t ++ "`; it already exists."
                                          i' <- getSt
                                          return (d', g, s, i', "Type constructor `" ++ t ++ " : " ++ show k ++ "` is defined.")
processLine (NewCons c t) (d, g, s)  = do g' <- case g ==> c of
                                                  TypeError _ -> if inNormalForm t
                                                                    then liftTypingMonad (d |:- t) >>= \ k ->
                                                                         case k of
                                                                           Star -> return $ (c, t) +> g
                                                                           _    -> fail $ "Cannot  create data constructor `" ++ c ++ " : " ++ show t ++ " : " ++ show k ++ "`; because type must have kind `*`."
                                                                    else fail $ "Cannot create data constructor `" ++ c ++ " : " ++ show t ++ "` because type must be in PNF."
                                                  _           -> fail $ "Cannot create data constructor `" ++ c ++ "`; it already exists."
                                          i' <- getSt
                                          return (d, g', s, i', "Value constructor `" ++ c ++ " : " ++ show t ++ "` is defined.")
processLine (NewBind x t e) (d, g, s) = do let eTemp = Fix x t e
                                           e' <- partialEvaluateTypes d eTemp
                                           k  <- liftTypingMonad $ d |:- t
                                           t' <- (d, g) |- e'
                                           eq <- t =~= t'
                                           i' <- getSt
                                           let res = (d, (x, t) +> g, (x, e) +> s, i', "Binding `" ++ x ++ " = " ++ show e ++ "` is defined.")
                                           if eq
                                              then case g ==> x of
                                                     TypeError _  -> return res
                                                     TypeData t'' -> do eq' <- t =~= t''
                                                                        if eq'
                                                                           then return res
                                                                           else fail $ "Incompatible type for binding `" ++ x ++ " : " ++ show t ++ "`; it must have already-bound type `" ++ show t'' ++ "`."
                                              else fail $ "Cannot create binding for `" ++ x ++ "` because inferred type `" ++ show t' ++ "` does not match declared type `" ++ show t ++ "`."
processLine (EvalExp e) (d, g, s)     = do e' <- partialEvaluateTypes d e
                                           t  <- (d, g) |- e'
                                           v  <- eval s e'
                                           i' <- getSt
                                           return (d, g, s, i', "Inferred: `" ++ show v ++ " : " ++ show t ++ "`.")



-- ===== Tie in the Parser ===== --

parseString :: String -> IO (Maybe Input)
parseString text = case papply pInput text of
                     [ (inp, "") ] -> do putLn $ " => " ++ show inp
                                         return $ Just inp
                     [ (e, txt) ]  -> do putLn . show $ e
                                         putLn $ "  with `" ++ txt ++ "` unread"
                                         putLn "(ignoring this line of input)"
                                         return Nothing
                     _             -> do putLn $ "Parse failed for `" ++ text ++ "`."
                                         return Nothing

readLines :: FilePath -> IO (Maybe [ String ])
readLines f = do res <- (try . readFile $ f :: IO (Either SomeException String))
                                            -- Need this to resolve otherwise-ambiguous instance.
                 return $ case res of
                           Left _  -> Nothing
                           Right s -> Just . lines $ s

readLinesAutoextension :: FilePath -> IO (Maybe [ String ])
readLinesAutoextension f = do firstGo <- readLines f
                              case firstGo of
                                Nothing -> let f' = f ++ ".sro" in
                                           do putLn $ "Unable to read `" ++ f ++ "`; trying `" ++ f' ++ "`."
                                              readLines f'
                                _       -> return firstGo

-- Remove `--`-style comments and blank lines.
stripComments :: [ String ] -> [ String ]
stripComments = groupLines . filter (not . all (== ' ')) . filter ((/= "-- ") . take 3)

groupLines :: [ String ] -> [ String ]
groupLines = loop [] []
               where loop []  out []                       = reverse out
                     loop cur out []                       = reverse (cur : out)
                     loop cur out (x : xs) | head x == ' ' = loop (cur ++ x) out xs
                     loop []  out (x : xs)                 = loop x out xs
                     loop cur out xs                       = loop "" (cur : out) xs



-- ===== General Utilities ===== --

putLn :: String -> IO ()
putLn s = do putStr $ s ++ "\n"
             hFlush stdout

successes :: [ Maybe a ] -> [ a ]
successes (Just x : ms)  = x : successes ms
successes (Nothing : ms) = successes ms
successes []             = []
