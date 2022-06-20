module VariksGPTools.List where

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
