-- | Module    : VariksGPTools.List
-- Description : List crap
-- Copyright   : (c) Varik Valefor 2022
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : stable
-- Portability : portable
--
-- = Lojban
--
-- .ni'o ti poi me'oi .module. cu vasru le su'o fancu poi filri'a lo nu
-- co'e lo me'oi .'List'.
--
-- = English
--
-- This module contains some functions which facilitate doing stuff with
-- 'List's.
module VariksGPTools.List where
import Data.Bool;
import Data.Maybe;
import Data.Either;

-- | = la .lojban.
--
-- .ni'o go la'oi .@a@. cu me'oi .sublist. la'oi .@b@. gi me'oi
-- la'oi .'True'. cu .@sublist@. la'oi .@a@. la'oi .@b@.  .i go la'oi
-- .@a@. cu me'oi .sublist.  la'oi .@b@. gi jetnu le du'u cumki fa le
-- nu jmina zo'e la'oi .@a@.  la'oi .@b@. kei .a le du'u la'oi .@a@. du
-- la'oi .@b@.
--
-- = English
--
-- @a `sublist` b@ iff @a@ is a sublist of @b@.  @a@ is a sublist of @b@
-- iff ((@b@ is the result of adding some values to @a@) or (@b@ is
-- @a@)).
sublist :: Eq a => [a] -> [a] -> Bool;
sublist [] _ = True;
sublist (x:xs) c = x `elem` c && xs `sublist` (drop 1 c);

-- | = la .lojban.
--
-- .ni'o ganai le sumti cu vasru lo me'oi .'Left'. gi me'oi .output. le
-- pamoi me'oi .'Left'.  .i ganai le sumti cu vasru lo no me'oi .'Left'.
-- gi me'oi .output. lo ro me'oi .'Right'.
--
-- = English
--
-- If @k@ contains a 'Left' value, then @unfurl k@ 'Left'-outputs the
-- first 'Left' value of @k@.  If @k@ contains no 'Left' values, then
-- @unfurl k@ 'Right'ly outputs all elements of @k@.
unfurl :: [Either a b] -> Either a [b];
unfurl k = bool lefts' rights' $ null $ lefts k
  where
  rights' = Right $ rights k
  lefts' = Left $ head $ lefts k;
