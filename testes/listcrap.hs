-- | = la .lojban.
--
-- ni'o le ti me'oi .module. vasru le fancu poi facki le jei le fancu
-- po se me'oi .export. la'o zoi. "VariksGPTools.List" .zoi drani
--
-- = English
--
-- This module contains some functions which determine whether or not
-- the functions which are exported by "VariksGPTools.List" work.
module Main where
import Data.Bool;
import System.Exit;
import VariksGPTools.List;

-- = la .lojban.
--
-- ni'o go ge la'oi .@main@. me'oi .equivalent. la'oi .'exitSuccess'. gi
-- le fancu poi se me'oi .export. la'o zoi. "VariksGPTools.List" .zoi
-- drani gi la'oi .@main@. me'oi .equivalent. la'oi .'exitFailure'.
--
-- = English
--
-- If the functions which are exported by "VariksGPTools.List" work,
-- then @main@ is equivalent to 'exitSuccess'.  @main@ is otherwise
-- equivalent to 'exitFailure'.
main :: IO ExitCode;
main = bool exitFailure exitSuccess $ and correctness
  where {
  correctness :: [Bool];
  correctness = [testUnfurl, testSublist];
};

-- | = la .lojban.
--
-- ni'o go la'oi .@testUnfurl@. me'oi .'True'. gi la'oi .'unfurl'.
-- drani
--
-- = English
--
-- @testUnfurl@ is 'True' iff 'unfurl' works.
testUnfurl :: Bool;
testUnfurl = all (\a -> unfurl (fst a) == snd a) unfurlables
  where {
  unfurlables :: [([Either Integer Integer], Either Integer [Integer])];
  unfurlables = [
    ([Right 1, Right 2, Right 3], Right [1,2,3]),
    ([Right 1, Right 2, Left 3], Left 3),
    ([Right 1, Left 2, Right 3], Left 2)
  ];
};

-- | = la .lojban.
--
-- ni'o go la'oi .@testSublist@. me'oi .'True'. gi la'oi .'sublist'.
-- drani
--
-- = English
--
-- @testSublist@ is 'True' iff 'sublist' works.
testSublist :: Bool;
testSublist = all sublist' truePairs && all (not . sublist') falsePairs
  where {
  sublist' :: ([Integer], [Integer]) -> Bool;
  sublist' a = sublist (fst a) (snd a);

  truePairs :: [([Integer], [Integer])];
  truePairs = [
    ([1,2,3], [5,1,5,2,3,5]),
    ([0], [0])
  ];

  falsePairs :: [([Integer], [Integer])];
  falsePairs = [
    ([0], [1,2,3]),
    ([1,2,3,4], [1,2,3])
  ];
};
