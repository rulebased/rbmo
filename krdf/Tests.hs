{-
    General Flows in Phase Space -- Test Suite
    Copyright (C) 2014 William Waites

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
module Main (main) where

import Test.HUnit
import System.Exit

import Test.KappaT
import Test.ReactionT

allTests :: Test
allTests = TestList [kappaTests, reactionTests]
 
main :: IO ()
main = do c <- runTestTT $ allTests
          putStr $ show c
          let errs = errors c
              fails = failures c
          exitWith (exitGet errs fails)

exitGet :: Int -> Int -> ExitCode
exitGet errs fails
 | fails > 0       = ExitFailure 2
 | errs > 0        = ExitFailure 1
 | otherwise       = ExitSuccess

-- Local Variables:
-- compile-command: "cabal build && cabal test"
-- End:
