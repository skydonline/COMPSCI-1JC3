{-|
Module      : 1JC3-Assign1.Assign_1.hs
Copyright   :  (c) Curtis D'Alves 2021
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable
Description:
  Assignment 1 - McMaster CS 1JC3 2022
-}
module Assign_1 where

import Prelude hiding (sin,cos,tan)

-----------------------------------------------------------------------------------------------------------
-- INSTRUCTIONS              README!!!
-----------------------------------------------------------------------------------------------------------
-- 1) DO NOT DELETE/ALTER ANY CODE ABOVE THESE INSTRUCTIONS OR ADD/ALTER IMPORTS
-- 2) DO NOT REMOVE / ALTER TYPE DECLERATIONS (I.E THE LINE WITH THE :: ABOUT THE FUNCTION DECLERATION)
--    IF YOU ARE UNABLE TO COMPLETE A FUNCTION, LEAVE IT'S ORIGINAL IMPLEMENTATION (I.E. THROW AN ERROR)
-- 3) MAKE SURE THE PROJECT COMPILES (I.E. RUN STACK BUILD AND MAKE SURE THERE ARE NO ERRORS) BEFORE
--    SUBMITTING, FAILURE TO DO SO WILL RESULT IN A MARK OF 0
-- 4) REPLACE macid = "TODO" WITH YOUR ACTUAL MACID (EX. IF YOUR MACID IS jim THEN macid = "jim")
-----------------------------------------------------------------------------------------------------------
-- Name: TODO add name
-- Date: TODO add date
macid :: String
macid = "dengs32"


{- -----------------------------------------------------------------
 - factorial
 - -----------------------------------------------------------------
 - Description:
 -    Computes the factorial of any Integer n
 - -----------------------------------------------------------------
 - |   Input     |                                                 |
 - |      n      | Integer input                                   |
 - -----------------------------------------------------------------
 - |   Output    |                                                 |
 - |      n <= 1 | 1                                               |
 - |      n >  1 | n * (n-1) ...  * 1   while (n-k) > 0            |
 - -----------------------------------------------------------------
 -}
factorial :: Integer -> Integer
factorial n = if n > 0
              then n * factorial (n-1)
              else 1


{- ------------------------------------------------------------------
 - sinTaylor
 - ------------------------------------------------------------------
 - Description:
Inputs:
x -> the actual point where it is evaluated at (Dobule)
a -> the point where the Taylor polynomial is centered (Double)
cos_a -> cos evaluated at "a" (Double)
sin_a -> sin evaluated at "a" (Double)

Output: The 4th Taylor polynomial approxiation

Other Comments:
I used fromIntegral to convert it to the appropiate data type, which is Double. I called the previous factorial function created above. The number beside the function is the "n" value used in the function (e.g. factorial 3 is 3!). The rest is just the 4th Taylor polynomial of sin(x) at "a". (-1 * ___) converts the value to a negative number, just like how it is in the 4th Taylor polynomail equation. Note: even though ^0 and ^1 are redundant (^0 = 1 and anything to ^1 is just itself), I just place those there to better illustrate the equation.
 -}
sinTaylor :: Double -> Double -> Double -> Double -> Double
sinTaylor a cos_a sin_a x = sin_a / fromIntegral(factorial 0) * (x - a)^0 
                          + cos_a / fromIntegral(factorial 1) * (x - a)^1 
                          + (-1 * sin_a) / fromIntegral(factorial 2) * (x - a)^2 
                          + (-1 * cos_a) / fromIntegral(factorial 3) * (x - a)^3 
                          + sin_a / fromIntegral(factorial 4) * (x - a)^4


{- -----------------------------------------------------------------
 - fmod
 - -----------------------------------------------------------------
 - Description:
Inputs:
x -> the number that is being divided (Double)
y -> the number that is doing the dividing (Double)

Output: The smallest remainder of x divided by some amount of y.

Other Comments:
Floor returns largest integer that is not greater than input (e.g. 3.1 -> 3, 4.9 -> 4, etc.). fromIntegral changes it to the appropiate data type, which is Double. (x/y) gives us how many times x can be divided by y. x - z*y gives us the remainder, since z*y will always be less than x.
 -}
fmod :: Double -> Double -> Double
fmod x y =
  let
    -- z is the largest integer s.t. z*y <= x
    -- HINT use floating point division, then round down
    z = fromIntegral(floor(x/y))
  in x - z*y 


{- ----------------------------------------------------------------------
 - sinApprox
 - ----------------------------------------------------------------------
 - Description:
Input:
x -> the value we are evaluating

Output: The approximate sin value

Other Comments:
I made z = values of x, to clean up code visually and make it easier to manipulate in the future if necessary. | checks if the left side is True, then the corresponding function body is used. If it is False, it moves onto the next guard and evaluates it, and so on (they are much more readable than "if statements"). && checks for if both conditions are true. It returns true if both are true and returns false if any are false. In this instance, it just checks if the x values satisfy the conditions in the chart. If both conditions are satisfied, then appropiate sinTaylor values are inputted into the function.
 -}
sinApprox :: Double -> Double
sinApprox x 
  | ((0 <= z) && (z < (pi/4))) = (sinTaylor 0 1 0 z) 
  | (((pi/4) <= z) && (z < (3/4*pi))) = (sinTaylor (pi/2) 0 1 z) 
  | (((3/4*pi) <= z) && (z < (5/4*pi))) = (sinTaylor pi (-1) 0 z) 
  | (((5/4*pi) <= z) && (z < (7/4*pi))) = (sinTaylor (3/2*pi) 0 (-1) z) 
  | otherwise = (sinTaylor (2*pi) 1 0 z)
    where z = (fmod x (2*pi))


{- ---------------------------------------------------------------------
 - cosApprox
 - ---------------------------------------------------------------------
 - Description:
Input:
x -> the value we are evaluating

Output: The approximate cos value

Other Comments:
We minus x by pi/2, then take the sin of it, multiply it by -1. This is just the equation for cos (according to the assignment document).
 -}
cosApprox :: Double -> Double
cosApprox x = (-1) * sinApprox (x - (pi/2))


{- ---------------------------------------------------------------------
 - tanApprox
 - ---------------------------------------------------------------------
 - Description:
Input:
x -> the value we are evaluating

Output: The approximate tan value

Other Comments:
We  divide the sinApprox function by cosApprox function, both evaluated at point x. This is just the equation for tan (according to the assignment document)
 -}
tanApprox :: Double -> Double
tanApprox x = (sinApprox x) / (cosApprox x)
