{-|
Module      : 1JC3-Assign4.Assign_4.hs
Copyright   :  (c) Curtis D'Alves 2021
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable
Description:
  Assignment 4 - McMaster CS 1JC3 2022
-}
module Assign_4 where

import Test.QuickCheck

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

-- Name: Sky Deng
-- Date: June 15 2023
macid :: String
macid = "dengs32"

{- --------------------------------------------------------------------
 - Datatype: MathExpr
 - --------------------------------------------------------------------
 - Description: An Abstract Syntax Tree (AST) for encoding mathematical
 -              expressions
 - Example: The expression
 -                (abs (2*X + 1)) ^ 3
 -          can be encoded as
 -                Power 3 (Func1 Abs
 -                              (Func2 Add (Func2 Mult (Coef 2) X)
 -                                         (Coef 1)))
 - --------------------------------------------------------------------
 -}
data MathExpr a =
    X
  | Coef a
  | Func1 UnaryOp (MathExpr a)
  | Func2 BinOp (MathExpr a) (MathExpr a)
  deriving (Eq,Show,Read)

data BinOp = Add | Mult
  deriving (Show,Eq,Read)

data UnaryOp = Cos | Sin | Abs | Power Int
  deriving (Show,Eq,Read)

{- -----------------------------------------------------------------
 - eval
 - -----------------------------------------------------------------
 - Description:
Possible inputs and respecitve outputs:
Case 1:
Inputs: the variable X (type: MathExpr a) and v is the value (type: a)
Output: returns the value "v", since we are just subbing in v for X (type: a)

Case 2:
Inputs: the number/coefficient "c" (type: MathExpr a) and v is the value (type: a)
Output: returns the value "c", since the number will remain the same (type: a)

Case 3:
Inputs: a operation that uses a unary operator, that has the format "Func1 UnaryOp (MathExpr a)" (type: MathExpr a) and v is the value (type: a)
Output: goes to the applyUnaryEval function (returns type: a), with the inputs to that function being the operator and evaluating the internal "MathExpr a" at v. However, if the operator is the Power operator, it goes to the applyPowerEval, which takes the integer power and expression as inputs. The function eval eventually returns the evaluation of the function at value v (type: a)

Case 4:
Inputs: a operation that uses a binary operator, that has the format "Func2 BinOp (MathExpr a) (MathExpr a)" (type: MathExpr a) and v is the value (type: a)
Output: goes to the applyBinEval function (returns type: a), with the inputs to that function being the operator and evaluating both of the internal "MathExpr a" at v. The function eval eventually returns the evaluation of the function at value v (type: a)

Other comments: I created seperate functions, applyUnaryEval, applyPowerEval, applyBinEval to help the eval function. As their names suggest, they complete the operation that they're supposed to do (unary operation, power operation, binary operation respectively). This was to help clean up code, and make it simplier/readable.
 -}
eval :: (Floating a, Eq a) => MathExpr a -> a -> a
eval expr v = case expr of
  X -> v
  Coef c -> c
  Func1 op e -> applyUnaryEval op (eval e v)
  Func2 op e1 e2 -> applyBinEval op (eval e1 v) (eval e2 v)

applyUnaryEval :: Floating a => UnaryOp -> a -> a
applyUnaryEval op val = case op of
  Cos -> cos val
  Sin -> sin val
  Abs -> abs val
  Power n -> applyPowerEval n val

applyPowerEval :: Floating a => Int -> a -> a
applyPowerEval n val
  | n >= 0 = val ^ n
  | otherwise = 1 / (val ^ abs n)

applyBinEval :: Num a => BinOp -> a -> a -> a
applyBinEval op val1 val2 = case op of
  Add -> val1 + val2
  Mult -> val1 * val2



{- -----------------------------------------------------------------
 - instance Num a => Num (MathExpr a)
 - -----------------------------------------------------------------
 - Description:
Possible inputs and respecitve outputs:
Case 1: x + y -> Func2 Add x y
Purpose: shows how the addition operator between 2 variables is expressed

Case 2: x * y -> Func2 Mult x y
Purpose: shows how the multiplcation operator between 2 variables is expressed

Case 3: negate x -> Func2 Mult x (-1)
Purpose: shows how the variable is negated (multiply by -1)

Case 4: abs x -> Func1 Abs x
Purpose: shows how the absolute value is expressed

Case 5: fromInteger i -> Coef (fromInteger i)
Purpose: shows how fromInteger is expressed (convert to the appropriate type that is an instance of the Num typeclass)

Other comments: the purpose of this section is to show how mathematical operations from the Num typeclass behave for values of the "MathExpr a" type.
 -}
instance Num a => Num (MathExpr a) where
  x + y         = Func2 Add x y
  x * y         = Func2 Mult x y
  negate x      = Func2 Mult x (-1)
  abs x         = Func1 Abs x
  fromInteger i = Coef (fromInteger i)
  signum _      = error "signum is left un-implemented"



{- -----------------------------------------------------------------
 - instance Fractional a => Fractional (MathExpr a)
 - -----------------------------------------------------------------
 - Description:
Possible inputs and respecitve outputs:
Case 1: recip e -> Func1 (Power (-1)) e
Purpose: shows how the reciprocal of a variable is expressed

Case 2: fromRational e -> Coef (fromRational e)
Purpose: shows how fromRational is expressed (convert to the appropriate type that is an instance of the Fractional typeclass)

Other comments: the purpose of this section is to show how mathematical operations from the Fractional typeclass behave for values of the "MathExpr a" type.
 -}
instance Fractional a => Fractional (MathExpr a) where
  recip e        = Func1 (Power (-1)) e
  fromRational e = Coef (fromRational e)



{- -----------------------------------------------------------------
 - instance Floating a => Floating (MathExpr a)
 - -----------------------------------------------------------------
 - Description:
Possible inputs and respecitve outputs:
Case 1: pi -> Coef pi
Purpose: shows how pi is expressed

Case 2: sin -> Func1 Sin
Purpose: shows how sin is expressed

Case 3: cos -> Func1 Cos
Purpose: shows how cos is expressed

Other comments: the purpose of this section is to show how mathematical operations from the Floating typeclass behave for values of the "MathExpr a" type.
 -}
instance Floating a => Floating (MathExpr a) where
  pi      = Coef pi
  sin     = Func1 Sin
  cos     = Func1 Cos
  log     = error "log is left un-implemented"
  asin _  = error "asin is left un-implemented"
  acos _  = error "acos is left un-implemented"
  atan _  = error "atan is left un-implemented"
  sinh _  = error "sinh is left un-implemented"
  cosh _  = error "cosh is left un-implemented"
  tanh _  = error "tanh is left un-implemented"
  asinh _ = error "asinh is left un-implemented"
  acosh _ = error "acosh is left un-implemented"
  atanh _ = error "atanh is left un-implemented"
  exp _   = error "exp is left un-implemented"
  sqrt _  = error "sqrt is left un-implemented"



{- -----------------------------------------------------------------
 - diff
 - -----------------------------------------------------------------
 - Description:
Possible inputs and respecitve outputs:
Case 1:
Input: the variable "X" (type: MathExpr a)
Output: returns Coef 1 (type: MathExpr a), since the derivative of "X" is 1

Case 2:
Input: any possible number, "Coef _" (type: MathExpr a)
Output: returns Coef 0 (type: MathExpr a), since the derivative of any number is 0

Case 3:
Input: any expression with a unary operator, "Func1 _ _" (type: MathExpr a)
Output: calls for the applyUnaryDiff function, which then takes the operator and returns it's respective derivative (type: MathExpr a)

Case 4:
Input: any expression with a binary operator, "Func2 _ _ _" (type: MathExpr a)
Output: calls for the applyBinaryDiff function, which then takes the operator and returns it's respective derivative (type: MathExpr a)

Other comments: I created separate functions, applyUnaryDiff and applyBinaryDiff, to help clean up code and make it cleaner. They just compute the derivative for the respective operator, according to the derivative rules.
 -}
diff :: (Floating a, Eq a) => MathExpr a -> MathExpr a
diff e = case e of
  X -> Coef 1
  Coef _ -> Coef 0
  Func1 op e -> applyUnaryDiff op e
  Func2 op e1 e2 -> applyBinaryDiff op e1 e2

applyUnaryDiff :: (Floating a, Eq a) => UnaryOp -> MathExpr a -> MathExpr a
applyUnaryDiff op e = case op of
  Cos -> Func1 (Power 1) (Func1 Sin e)
  Sin -> Func1 (Power 1) (Func1 Cos e)
  Abs -> Func1 (Power (-1)) (Func1 (Power 2) e)
  Power n -> Func2 Mult (Coef (fromIntegral n)) (Func1 (Power (n - 1)) e)

applyBinaryDiff op e1 e2 = case op of
  Add -> Func2 Add (diff e1) (diff e2)
  Mult -> Func2 Add (Func2 Mult (diff e1) e2) (Func2 Mult e1 (diff e2))


  
{- -----------------------------------------------------------------
 - pretty
 - -----------------------------------------------------------------
 - Description:
Possible inputs and respecitve outputs:
Case 1:
Inputs: only the variable X (type: MathExpr a)
Output: returns "X" (type: string)

Case 2:
Inputs: any number, Coef _ (type: MathExpr a)
Output: returns "(c)" (type: string)

Case 3:
Inputs: the addition operator, Func2 Add u0 u1 (where u0 and u1 are the subexpressions) (type: MathExpr a)
Output: returns "(u0 + u1)" (type: string)

Case 4:
Inputs: the multiplicaton operator, Func2 Mult u0 u1 (where u0 and u1 are the subexpressions) (type: MathExpr a)
Output: returns "(u0 * u1)" (type: string)

Case 5:
Inputs: the power operator, Func1 (Power d) u0 (where d is the power integer, and u0 is the subexpression) (type: MathExpr a)
Output: returns "(u0 ^^ (d))"  (type: string)

Case 6:
Inputs: the cos operation, Func1 Cos u0 (where u0 is the subexpression) (type: MathExpr a)
Output: returns "cos(u0)" (type: string)

Case 7:
Inputs: the sin operation, Func1 Sin u0 (where u0 is the subexpression) (type: MathExpr a)
Output: returns "sin(u0)" (type: string)

Case 8:
Inputs: the absolute operation, Func1 Abs u0 (where u0 is the subexpression) (type: MathExpr a)
Output: returns "abs(u0)" (type: string)

Other comments: just displays the appropiate strings for each expression, according to the assignment document.
 -}
pretty :: (Show a) => MathExpr a -> String
pretty e = case e of
  X -> "X"
  Coef c -> "(" ++ show c ++ ")"
  Func2 Add u0 u1 -> "(" ++ pretty u0 ++ " + " ++ pretty u1 ++ ")"
  Func2 Mult u0 u1 -> "(" ++ pretty u0 ++ " * " ++ pretty u1 ++ ")"
  Func1 (Power d) u0 -> "(" ++ pretty u0 ++ " ^^ (" ++ show d ++ "))"
  Func1 Cos u0 -> "cos(" ++ pretty u0 ++ ")"
  Func1 Sin u0 -> "sin(" ++ pretty u0 ++ ")"
  Func1 Abs u0 -> "abs(" ++ pretty u0 ++ ")"



{- -----------------------------------------------------------------
 - Test Cases

Function: eval
Test Case Number: 1
Input: X 2
Expected Output: 2.0 
(since it will just substitute the value in for "X")
Actual Output: 2.0

Function: eval
Test Case Number: 2
Input: (Func1 Cos (Coef 1)) 2
Expected Output: Cos 1 = 0.5403023058681398 
(since there is no variable "X", it will just compute cos(1))
Actual Output: 0.5403023058681398

Function: eval
Test Case Number: 3
Input: (Func2 Add (Coef 2) X) 4
Expected Output: 6.0 
(the expreesion just means 2 + X, where X = 4. So 2 + 4 = 6.0)
Actual Output: 6.0

Function: diff
Test Case Number: 1
Input: X
Expected Output: Coef 1.0
(since the derivative of "X" will just be 1)
Actual Output: Coef 1.0

Function: diff
Test Case Number: 2
Input: (Coef 4)
Expected Output: Coef 0.0
(since the derivative of any number is just 0)
Actual Output: Coef 0.0

Function: diff
Test Case Number: 3
Input: (Func2 Add X (Coef 2))
Expected Output: Func2 Add (Coef 1.0) (Coef 0.0)
(The derivative of X is 1, the derivative of any number is 0. The derivative of any sum of 2 numbers is just both the derivatives added together, so the derivative is 1 + 0)
Actual Output: Func2 Add (Coef 1.0) (Coef 0.0)

Function: pretty
Test Case Number: 1
Input: Coef 2
Expected Output: "(2)"
(since the output is just supposed to be the number wrapped around brackets and quotations)
Actual Output: "(2)"

Function: pretty
Test Case Number: 2
Input: (Func2 Add (Coef 2) (X))
Expected Output: "((2) + X)"
(since it is just 2 added to X, and then surround with brackets and quotations)
Actual Output: "((2) + X)"

Function: pretty
Test Case Number: 3
Input: (Func1 Sin (Func2 Add (Coef 0) (Coef 1)))
Expected Output: "sin(((0) + (1)))"
(since it is just sin of 0 added to 1, then each expression is wrapped around brackets, and the whole expression is wrapped in quotations)
Actual Output: "sin(((0) + (1)))"
 - -----------------------------------------------------------------
 -}
infix 4 =~
(=~) :: (Floating a,Ord a) => a -> a -> Bool
x =~ y = abs (x - y) <= 1e-4

{- EXAMPLE
 - Function: eval
 - Property: eval (Func2 Add (Coef x) X) y is correct for all x,y
 - Actual Test Result: Pass
 -}
evalProp0 :: (Float,Float) -> Bool
evalProp0 (x,y) = (x + y) =~ eval (Func2 Add (Coef x) X) y

runEvalProp0 :: IO ()
runEvalProp0 = quickCheck  evalProp0


{- QUICKCHECK TEST CASES
Function: eval
Property: eval (Func2 Mult (Coef x) (Coef x)) y is correct for all x,y. It is checking if the multiplication function is implemented correctly, seeing if x * x is equivalent to the "MathExpr a" version.
Actual Test Result: Pass
-}
evalProp :: (Float,Float) -> Bool
evalProp (x,y) = (x * x) =~ eval (Func2 Mult (Coef x) (Coef x)) y

runEvalProp :: IO ()
runEvalProp = quickCheck evalProp


{-
Function: diff
Property: diff (Coef x) (where "x" is any number) is correct for all numbers. It is checking if the derivative is 0, since that is the derivative for any number.
Actual Test Result: Pass
-}
diffProp :: Float -> Bool
diffProp x = 
  let result = diff (Coef x)
  in case result of
    Coef val -> val == 0
    _ -> False

runDiffProp :: IO ()
runDiffProp = quickCheck diffProp
