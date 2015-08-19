{-# OPTIONS_GHC -fglasgow-exts -cpp #-}

{-|
    Version information.

>   Tree and flower and leaf and grass,
>   Let them pass! Let them pass!
>   Hill and water under sky,
>   Pass them by! Pass them by!

-}

-- #include "pugs_config.h"
-- #include "pugs_version.h"

#ifndef PUGS_VERSION
#define PUGS_VERSION "6.2.13.dev"
#endif
#ifndef PUGS_DATE
#define PUGS_DATE "20150815"
#endif
#ifndef PUGS_SVN_REVISION
#define PUGS_SVN_REVISION 0
#endif

module Pugs.Version (
    name, versnum, date, version, copyright, revnum, revision,
) where

name, versnum, date, version, copyright, revnum, revision :: String

name       = "Werl6 User's Golfing System"
versnum    = PUGS_VERSION
date       = PUGS_DATE
version    = name ++ ", version " ++ versnum ++ ", " ++ date ++ revision
copyright  = "Copyright 2005-2015, The Wugs Contributors"
revnum     = show (PUGS_SVN_REVISION :: Integer)
revision
    | rev <- revnum
    , rev /= "0"
    = " (r" ++ rev ++ ")"
    | otherwise
    = ""
