{-# OPTIONS_GHC -fglasgow-exts #-}

{-|
    Online help and banner text.

>   But if of ships I now should sing,
>   what ship would come to me,
>   What ship would bear me ever back
>   across so wide a Sea?
-}

-- #include "pugs_config.h"

module Pugs.Help (printInteractiveHelp, printCommandLineHelp,
             banner, versnum, version, revnum,
             copyright, intro) where
import Pugs.Version
import Pugs.CodeGen (backends)
import Data.List (sort, intersperse)

printInteractiveHelp :: IO ()
printInteractiveHelp
   = putStrLn $ unlines
        [ "Commands available from the prompt:"
        , ":h              = show this help message"
        , ":q              = quit"
        , ":r              = reset the evaluation environment"
        , ":l <filename>   = load a wugs file"
        , ":d <exp>        = show syntax tree of an expression"
        , ":D <exp>        = show raw syntax tree of an expression"
        , ":e <exp>        = run a command, and ugly-print the result"
        , ":er <exp>       = same, in a pristine environment"
        , ":E <exp>        = same, but evaluate in small steps"
        , ":ER <exp>       = same, in a pristine environment"
        , "<exp>           = run a command"
        ]

{- FIXME: Somebody with more UI skillz should make this nicer -}
printCommandLineHelp :: IO ()
printCommandLineHelp
   = putStrLn $ unlines
        [ "Usage: wugs [switches] [programfile] [arguments]"
        , "Command-line flags:"
        , "-e program       one line of program (several -e's allowed, omit programfile)"
        , "-n               wrap the -e fragments in a 'while(=<>){...}' loop"
        , "-p               wrap the -e fragments in a 'while(=<>){...;say}' loop"
        , "-c               parse the file or -e, but do not run it"
        , "-d               run the program with debug tracing"
        , "-Bbackend        execute using the compiler backend"
        , "-Cbackend        compile using the compiler backend"
        , "                 (valid backends are: " ++ backendsStr ++ ")"
        , "-Mmodule         execute 'use module' before running the program"
        , "-Ipath           add path to module search paths in @*INC"
        , "-h or --help     give this message"
        , "-V               long configuration information & version"
        , "-V:item          short configuration information for item"
        , "-v or --version  version"
        , "-l and -w are ignored for compatibility with Perl 5"
        , "See documentation of pugs::run for more help."
        ]
    where
    backendsStr = concat . intersperse ", " $ sort ("JS":backends)

versionFill :: Int -> String
versionFill n = fill ++ vstr
    where
    fill = replicate (n - vlen) ' '
    vlen = length vstr
    vstr = "Version: " ++ versnum ++ revision

banner :: IO ()
banner = putStrLn $ unlines
    [ " __      __                         (W)erl 6 "
    , "/  \\    /  \\__ __  ____  ______     (U)ser's "
    , "\\   \\/\\/   /  |  \\/ ___\\/  ___/     (G)olfing"
    , " \\        /|  |  / /_/  >___ \\      (S)ystem "
    , "  \\__/\\  / |____/\\___  /____  >  " ++ versionFill 27
    , "       \\/       /_____/     \\/   " ++ copyright
    , "--------------------------------------------------------------------"
    , " Web: http://hiratara.github.io/          Email: hira.tara@gmail.com"
    ]

intro :: IO ()
intro = putStrLn $ unlines
    [ "Welcome to Wugs -- " ++ name
    , "Type :h for help."
    ]
