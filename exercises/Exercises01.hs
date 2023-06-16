{-|
Module      : HaskellExercises01.Exercises01
Copyright   :  (c) Curtis D'Alves 2020
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Haskell exercise template Set 01 - McMaster CS 1JC3 2021
-}
module Exercises01 where

import Prelude hiding (last,init,(!!))

-----------------------------------------------------------------------------------------------------------
-- INSTRUCTIONS              README!!!
-----------------------------------------------------------------------------------------------------------
-- 1) DO NOT DELETE/ALTER ANY CODE ABOVE THESE INSTRUCTIONS OR ADD/ALTER ANY IMPORTS
-- 2) DO NOT REMOVE / ALTER TYPE DECLERATIONS (I.E THE LINE WITH THE :: ABOUT THE FUNCTION DECLERATION)
--    IF YOU ARE UNABLE TO COMPLETE A FUNCTION, LEAVE IT'S ORIGINAL IMPLEMENTATION (I.E. THROW AN ERROR)
-- 3) MAKE SURE THE PROJECT COMPILES (I.E. RUN STACK BUILD AND MAKE SURE THERE ARE NO ERRORS) BEFORE
--    SUBMITTING, FAILURE TO DO SO WILL RESULT IN A MARK OF 0
-- 4) REPLACE macid = "TODO" WITH YOUR ACTUAL MACID (EX. IF YOUR MACID IS jim THEN macid = "jim")
-----------------------------------------------------------------------------------------------------------
macid = "dengs32"

-- NOTE: THE PRELUDE FUNCTIONS YOU'RE REQUIRED TO IMPLEMENT HAVE BEEN REMOVED, HOWEVER OTHER PRELUDE
--       FUNCTIONS ARE STILL AVAILABLE, E.I. drop,take,reverse,head,tail,length,div
--       YOU ARE ALSO FREE TO USE FUNCTIONs AFTER THEY'VE BEEN DEFINED

-- Exercise A
-----------------------------------------------------------------------------------------------------------
-- Implement the Prelude function last (which returns the last element of a list) using a combination of
-- other available Prelude functions
-----------------------------------------------------------------------------------------------------------
last :: [a] -> a
last xs = head (reverse xs)

-- Exercise B
-----------------------------------------------------------------------------------------------------------
-- Implement the Prelude function init (which returns a list with the last element removed) using a
-- combination of other available Prelude functions
-----------------------------------------------------------------------------------------------------------
init :: [a] -> [a]
init xs = reverse (tail (reverse xs))

-- Exercise C
-----------------------------------------------------------------------------------------------------------
-- Implement the index function !! (which is used to access a specific element of a list) using a
-- combination of other Prelude functions
-- For example:
-- xs = ['a','b','c']
-- xs !! 0 == 'a'
-- xs !! 2 == 'c'
-- xs !! 4 == error "out bounds"
-----------------------------------------------------------------------------------------------------------
(!!) :: [a] -> Int -> a
xs !! n = last (take (n + 1) xs)

-- Exercise D
-----------------------------------------------------------------------------------------------------------
-- Implement the functions firstHalf and lastHalf, that take a list and return the first and last halfs
-- respectively. If the list is uneven, the first half should be one element smaller than the last half
-- HINT: use the `div` function instead of / to do integer division when dividing the length of the list
--       by 2, then take or drop those amount of elements from the list
firstHalf :: [a] -> [a]
firstHalf xs = if even (length xs)
  then take (length xs `div` 2) xs 
  else reverse (drop ((length xs `div` 2) + 1) (reverse xs))

lastHalf :: [a] -> [a]
lastHalf xs = drop (length xs `div` 2) xs

--reverse $ take (length xs `div` 2) (reverse xs)

-- Exercise E
-----------------------------------------------------------------------------------------------------------
-- Implement the function inners that returns the inside of a list (i.e. without the first and last elements)
-- For example:
--   inners []        == []
--   inners [1]       == []
--   inners [1,2]     == []
--   inners [1,2,3,4] == [2,3]
inners :: [a] -> [a]
inners [] = []
inners xs = reverse (drop 1 (reverse (drop 1 xs)))

-- Exercise F
-----------------------------------------------------------------------------------------------------------
-- Implement a function that computes the Euclidean distance in 2 dimensions of two points p and q
-- See https://en.wikipedia.org/wiki/Euclidean_distance for details
distance :: (Float,Float) -> (Float,Float) -> Float
distance (p1,p2) (q1,q2) = sqrt((p1-q1)**2 + (p2-q2)**2)

-- Exercise F
-----------------------------------------------------------------------------------------------------------
-- Write a function nthRoot that computes the nth root (i.e. n==2 square root, n==3 cube root, etc)
-- using the fact that the nth root of a number is the same as performing a power to 1/n
-- NOTE: you have to use the ** operator instead of ^ when using floating point numbers for powers
--       you'll also need fromIntegral to convert from integers to a floating point
nthRoot :: Float -> Int -> Float
nthRoot x n = x**(1 / fromIntegral n)
