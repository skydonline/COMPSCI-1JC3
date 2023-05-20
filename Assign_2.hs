{-|
Module      : 1JC3-Assign2.Assign_2.hs
Copyright   :  (c) Curtis D'Alves 2021
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable
Description:
  Assignment 2 - McMaster CS 1JC3 2022
-}
module Assign_2 where

import Data.List
import Data.Maybe

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

-- Name: TODO add name
-- Date: TODO add date
macid :: String
macid = "dengs32"

type Vector3D = (Double,Double,Double)


{- -----------------------------------------------------------------
 - getX
 - -----------------------------------------------------------------
 - Description:
Input: a vector (x,y,z) (type: Vector3D)

Output: the x coordinate in the vector (type: Double)

Other Comments: function obtains the "x" component of the vector
 -}
getX :: Vector3D -> Double
getX (x, y, z) = x


{- -----------------------------------------------------------------
 - getY
 - -----------------------------------------------------------------
 - Description:
Input: a vector (x,y,z) (type: Vector3D)

Output: the y coordinate in the vector (type: Double)

Other Comments: function obtains the "y" component of the vector
 -}
getY :: Vector3D -> Double
getY (x, y, z) = y


{- -----------------------------------------------------------------
 - getZ
 - -----------------------------------------------------------------
 - Description:
Input: a vector (x,y,z) (type: Vector3D)

Output: the z coordinate in the vector (type: Double)

Other Comments: function obtains the "z" component of the vector
 -}
getZ :: Vector3D -> Double
getZ (x, y, z) = z


{- -----------------------------------------------------------------
 - scalarMult
 - -----------------------------------------------------------------
 - Description:
Inputs:
s -> scalar multiple, the value we want to multiply our vector by (type: Double)
v -> the vector we want to multiply (type: Vector3D)

Output: the scalar multiplication of its inputs, which is the vector multiplied by the scalar

Other Comments: scalar multiplication is calculated by multiplying the scalar "s" to every coordinate in the vector. The "get" functions are used to obtain the respective coordinates from the input vector
 -}
scalarMult :: Double -> Vector3D -> Vector3D
scalarMult s v = (s*(getX v), s*(getY v), s*(getZ v))


{- -----------------------------------------------------------------
 - add
 - -----------------------------------------------------------------
 - Description:
Inputs:
v0 -> the first vector (type: Vector3D)
v1 -> the second vector (type: Vector3D)

Output: 3D vector addition of the input vectors (type: Vector3D)

Other Comments: vector addition is calculated by adding each respective coordinate (x,y,z) of both vectors to form one resultant vector. The "get" functions are used on both input vectors to obtain the respective coordinates from each input vector
 -}
add :: Vector3D -> Vector3D -> Vector3D
add v0 v1 = ((getX v0)+(getX v1), (getY v0)+(getY v1), (getZ v0)+(getZ v1))


{- -----------------------------------------------------------------
 - innerProduct
 - -----------------------------------------------------------------
 - Description:
Inputs:
v0 -> the first vector (type: Vector3D)
v1 -> the second vector (type: Vector3D)

Output: the inner product of the input vectors in a 3D vector space (type: Double)

Other Comments: the inner product is calculated by multiplying each respective coordinate (x,y,z) of both vectors together and then adding up all the coordinates. The "get" functions are used on both input vectors to obtain the respective coordinates from each input vector
-}
innerProduct :: Vector3D -> Vector3D -> Double
innerProduct v0 v1 = (getX v0)*(getX v1) 
                      + (getY v0)*(getY v1)
                      + (getZ v0)*(getZ v1)


{- -----------------------------------------------------------------
 - distance
 - -----------------------------------------------------------------
 - Description:
Inputs:
v1 -> the first vector (type: Vector3D)
v2 -> the second vector (type: Vector3D)

Output: the distance between the input vectors (type: Double)

Other Comments: the distance between both input vectors is calculated by obtaining the square root of the difference between every respective coordinate (x,y,z) of the 2 vectors squared (i.e. sqrt((x1 - x2)^2 + (y1 - y2)^2 + (z1 - z2)^2)). The "get" functions are used on both input vectors to obtain the respective coordinates from each input vector
-}
distance :: Vector3D -> Vector3D -> Double
distance v1 v2 = ((getX v1 - getX v2)^2 
                  + (getY v1 - getY v2)^2
                  + (getZ v1 - getZ v2)^2)**(1/2)

{- ------------------------------------------------------------------------
 - maxDistance
 - ------------------------------------------------------------------------
 - Description:
Inputs: takes a list of the type Vector3D

Possible inputs and respective outputs:
Empty list [] -> output is (0,0,0) (Output type: Vector3D)

List containing only 1 vector [v] -> output is [v] (this is the base case for our recursive function, and is the only vector in the list, so of course it is the furthest from the origin. Output type: Vector3D)

List with 2 or more elements (v:vs) -> output is vector furthest from origin (v represents the first element of the list, vs represents the rest of the list. Output type: Vector3D)


Other Comments:
First pattern match is incase we are given an empty list, which returns (0,0,0).

Second pattern match is incase we are given a list with only 1 element, which returns the vector in the list. It is our base case for our recursive function. Additionally, if we are provided with only 1 item in the original list, obviously it will be the furthest vector from the origin since there is no other vector in that provided list.

Third clause is checking if the distance between the vector and the origin is larger than the distance between the distance between any of the vectors in the rest of the list. v represents the first element of the list, while vs represents the rest. The "distance" function is used to find the distance between said vector and the origin, which I have defined as "z" since we use it multiple times, just to clean up code. maxDistanceTail is defined as the maxDistance of the rest of the list (meaning the first element is excluded). This goes through a loop through the entire list, and then compares them. When we find the furthest vector (or two vectors that tie for being the furthest), that vector is returned.
-}

maxDistance :: [Vector3D] -> Vector3D
maxDistance [] = (0,0,0)
maxDistance [v] = v
maxDistance (v:vs) 
  | distance v z >= distance maxDistanceTail z = v
  | otherwise = maxDistanceTail
  where maxDistanceTail = maxDistance vs
        z = (0,0,0)