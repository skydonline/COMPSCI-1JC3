{-|
Module      : 1JC3-Assign3.Assign_3.hs
Copyright   :  (c) Curtis D'Alves 2021
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable
Description:
  Assignment 3 - McMaster CS 1JC3 2022
-}
module Assign_3 where

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
-- Date: June 11th 2023
macid :: String
macid = "dengs32"

{- -----------------------------------------------------------------
 - Datatypes
 - -----------------------------------------------------------------
 -}
data Graph a = Graph { getNodes :: [Node a]
                     , getEdges :: [Edge] }
  deriving (Show,Eq)

type Edge = (NodeID,NodeID)
type NodeID = Int

data Node a = Node { getNodeID  :: NodeID,
                     getNodeVal :: a }
  deriving (Show,Eq,Ord)


{- -----------------------------------------------------------------
 - Example Graph
 - -----------------------------------------------------------------
 -              -----        ----------
 -              | A |------->| C |    |
 -              -----        ----- <---
 -                |           |
 -                |     ------|
 -                v     |
 -              ----- <-|
 -              | B |
 -              -----
 -}
nodeA,nodeB,nodeC :: Node Char
nodeA = Node 0 'A'
nodeB = Node 1 'B'
nodeC = Node 2 'C'

exGraph :: Graph Char
exGraph =
  let
    nodes = [nodeA,nodeB,nodeC]
    edges = [(0,1),(0,2),(2,2),(2,1)]
  in Graph nodes edges

{- -----------------------------------------------------------------
 - maxNodeID
 - -----------------------------------------------------------------
 - Description:
Possible inputs and respective outputs: 
Case 1: 
Input: Graph with an empty list of nodes (type: Graph)
Output: returns Nothing (type: Maybe NodeID)

Case 2: 
Input: Graph with a list of nodes (type: Graph)
Output: returns the largest NodeID in the list (type: Maybe NodeID)

Other comments: retrieves the largest NodeID (integer) in the Graph.
 -}
maxNodeID :: Graph a -> Maybe NodeID
maxNodeID (Graph [] edge) = Nothing
maxNodeID (Graph ((Node x _):xs) edge) = case maxNodeID (Graph xs edge) of
                                        Nothing -> Just x
                                        Just y -> Just (max x y)



{- -----------------------------------------------------------------
 - insertNode
 - -----------------------------------------------------------------
 - Description:
Possible inputs and respective outputs: 
Case 1: 
Inputs: "v" is the node value we are trying to insert (type: a), and the input Graph has no nodes (type: Graph)
Output: returns the Graph with a node of NodeID of 0 and a value of "v" added to it (type: Graph)

Case 2:
Inputs: "v" is the node value we are trying to insert (type: a), and the input Graph has a list of nodes (type: Graph)
Output: returns the Graph with an added node with the "v" value, and the NodeID is the previous largest NodeID plus 1

Other Comments: created a maybeToInt function to reuse the maxNodeID function from above, to make the code more simple. maybeToInt turns the "Maybe a" type into the "a" (integer) type, and then is used in the insertNode function. This is to find the highest NodeID, then insert a new NodeID that is the previously largest NodeID plus 1.
 -}
insertNode :: a -> Graph a -> Graph a
insertNode v (Graph [] edge) = (Graph [Node 0 v] edge)
insertNode v (Graph (xs) edge) = (Graph ((Node (m+1) v):xs) edge) 
  where m = maybeToInt (maxNodeID (Graph xs edge))

maybeToInt :: Num a => Maybe a -> a
maybeToInt Nothing = 0
maybeToInt (Just x) = x



{- -----------------------------------------------------------------
 - removeNode
 - -----------------------------------------------------------------
 - Description:
Possible inputs and respective outputs:
Case 1:
Inputs: a NodeID (type: NodeID) that will be removed from both nodes and edges, Graph with no nodes or edges (type: Graph)
Output: returns a graph with no nodes or edges, since there is nothing to remove anyways (type: Graph)

Case 2:
Inputs: a NodeID (type: NodeID) that will be removed from both nodes and edges, Graph with nodes and/or edges (type: Graph)
Output: returns a graph that has all the nodes and edges with the NodeID removed (type: Graph)

Other comments: created a removeNodes and removeEdges function to help remove the nodes and edges with the NodeID. This makes the code a lot cleaner, since you can see how they recusrive iterate through the list of nodes/edges in the original graph, and then return the new list of nodes/edges without that NodeID to the original removeNode function.
 -}
removeNode :: NodeID -> Graph a -> Graph a
removeNode nID (Graph [] []) = (Graph [] [])
removeNode nID (Graph (xs) (es)) = (Graph (removeNodes nID xs) (removeEdges nID es))

removeNodes :: NodeID -> [Node a] -> [Node a]
removeNodes _ [] = []
removeNodes nID ((Node x v):xs)
  | nID == x = removeNodes nID xs
  | otherwise = (Node x v) : removeNodes nID xs

removeEdges :: NodeID -> [Edge] -> [Edge]
removeEdges _ [] = []
removeEdges nID ((e1,e2):es)
  | nID == e1 || nID == e2 = removeEdges nID es
  | otherwise = (e1,e2) : removeEdges nID es



{- -----------------------------------------------------------------
 - lookupNode
 - -----------------------------------------------------------------
 - Description:
Possible inputs and respective outputs:
Case 1: Base Case
Inputs: a NodeID (integer) that we are looking for in the graph, and the graph we are searching through has no nodes (type: Graph)
Output: returns Nothing (type: Maybe (Node a))

Case 2:
Inputs: a NodeID (integer) that we are looking for in the graph, and the graph we are searching through has nodes (type: graph)
Output: returns the Node that has that NodeID value, surrounded by the Maybe type (type: Maybe (Node a))

Other Comments: if the NodeID we are looking for is not found, it recursively goes through the rest of the nodes in the graph to try and find it. If it reaches the end of the list, and the NodeID we are looking for is not found, it returns Nothing
 -}
lookupNode :: NodeID -> Graph a -> Maybe (Node a)
lookupNode nID (Graph [] _) = Nothing
lookupNode nID (Graph ((Node x a):xs)edge)
  | nID == x = Just (Node x a)
  | otherwise = lookupNode nID (Graph xs edge)



{- -----------------------------------------------------------------
 - insertEdge
 - -----------------------------------------------------------------
 - Description:
Possible inputs and respective outputs:
Case 1:
Inputs: a tuple of NodeIDs that corresponds to an edge (type: (NodeID, NodeID)), and a graph with no nodes (type: Graph)
Output: returns Nothing, since the nodes that we are trying to insert an edge to a graph with no nodes (type: Maybe (Graph a))

Case 2:
Inputs: a tuple of NodeIDs that corresponds to an edge (type: (NodeID, NodeID)), and a graph with nodes/edges (type: Graph)
Possible Outputs: 
If either node doesn't exist in the graph, it returns nothing since we cannot attach an edge to a node that doesn't exist in the graph (type: Maybe (Graph a))
If the edge already exists in the graph, a duplicate is not introduced, so it  returns Just original graph (type: Maybe (Graph a))
If the edge doesn't exist and both nodes exist in the graph, it adds the edge to the graph, and returns Just new graph (type: Maybe (Graph a))

Other Comments: the function containsBothNodes checks if there are nodes corresponding to input NodeIDs in the graph, using the lookupNode function. It returns True if the function doesn't return Nothing for both input NodeIDs. If either node doesn't exist in graph, it returns False. If the result is True, then insertEdge doesn't return Nothing due to the guard displaying "not containsBothNodes". It would move onto the next guarded expression. If the result is False, then insertEdge returns Nothing, since the guarded expression would then evaluate to True.
The function containsEdge checks if the edge is an element of the edges list, and returns True if it is. If the edge doesn't already exist, it returns False. If it returns True, insertEdge will return Just the original graph, since the guarded expression would evaluate to True. If it returns False, insertEdge will continue to the otherwise expression, since it is the last guarded expression in the function.
 -}
insertEdge :: Eq a => (NodeID,NodeID) -> Graph a -> Maybe (Graph a)
insertEdge _       (Graph [] _)  = Nothing
insertEdge (n1,n2) (Graph ns es)
  | not containsBothNodes = Nothing
  | containsEdge          = Just (Graph ns es)
  | otherwise             = Just (Graph ns ((n1,n2):es))
  where
    containsBothNodes :: Bool
    containsBothNodes = lookupNode n1 (Graph ns es) /= Nothing && lookupNode n2 (Graph ns es) /= Nothing
    containsEdge :: Bool
    containsEdge = (n1, n2) `elem` es



{-  TESTING

Function: maxNodeID
Test Case Number: 1
Input: (Graph [Node 1 'a'] [(0,0)])
Expected Output: Just 1
(returns the highest NodeID in the list of nodes in the Maybe a type, which is Just 1)
Actual Output: Just 1

Function: maxNodeID
Test Case Number: 2
Input: (Graph [(Node 1 'a'),(Node 2 'b')] [(0,0),(2,3)])
Expected Output: Just 2
(returns the highest NodeID in the list of nodes in the Maybe a type, which is Just 2. Although we have an edge that has a higher value, we are not accounting for values in edges)
Actual Output: Just 2

Function: maxNodeID
Test Case Number: 3
Input: (Graph [] [(1,1),(2,3)])
Expected Output: Nothing
(since there are no nodes in the graph, it should return Nothing. Edges do not matter in this instance)
Actual Output: Nothing

Function: insertNode
Test Case Number: 1
Input: 'C' (Graph [] [(4,2)])
Expected Output: Graph {getNodes = [Node {getNodeID = 0, getNodeVal = 'C'}], getEdges = [(4,2)]} 
(since there are no nodes, we should expect a new node with NodeID of 0, with the value being our other input, 'C'. Edges will remain unchanged)
Actual Output: Graph {getNodes = [Node {getNodeID = 0, getNodeVal = 'C'}], getEdges = [(4,2)]}

Function: insertNode
Test Case Number: 2
Input: 'C' (Graph [(Node 0 'A')] [(1,2),(3,3)])
Expected Output: Graph {getNodes = [Node {getNodeID = 1, getNodeVal = 'C'},Node {getNodeID = 0, getNodeVal = 'A'}], getEdges = [(1,2),(3,3)]} 
(since our previously largest node value was 0, our new node will have a NodeID that is 1 greater than the previous greatest value, so 1. The value will be our other input, 'C'. Again, edges will remain unchanged, as well as the other nodes)
Actual Output: Graph {getNodes = [Node {getNodeID = 1, getNodeVal = 'C'},Node {getNodeID = 0, getNodeVal = 'A'}], getEdges = [(1,2),(3,3)]}

Function: insertNode
Test Case Number: 3
Input: 'C' (Graph [(Node 0 'A'),(Node 7 'B')] [])
Expected Output: Graph {getNodes = [Node {getNodeID = 8, getNodeVal = 'C'},Node {getNodeID = 0, getNodeVal = 'A'},Node {getNodeID = 7, getNodeVal = 'B'}], getEdges = []}
(since our previously largest node value was 7, our new node will have a NodeID that is 1 greater than the previous greatest value, so 8. The value will be our other input, 'C'. Again, edges will remain unchanged, as well as the other nodes)
Actual Output: Graph {getNodes = [Node {getNodeID = 8, getNodeVal = 'C'},Node {getNodeID = 0, getNodeVal = 'A'},Node {getNodeID = 7, getNodeVal = 'B'}], getEdges = []}

Function: removeNode
Test Case Number: 1
Input: 0 (Graph [] [])
Expected Output: Graph {getNodes = [], getEdges = []}
(since there are no nodes/edges in the graph, we cannot remove anything, so it should return a graph with no nodes/edges)
Actual Output: Graph {getNodes = [], getEdges = []}

Function: removeNode
Test Case Number: 2
Input: 0 (Graph [Node 0 'A'] [])
Expected Output: Graph {getNodes = [], getEdges = []}
(since there is a node with the desired NodeID that we want to remove, it will be removed from the graph. Since that is the only node in the graph, and there are no edges, it should return a graph with no nodes/edges)
Actual Output: Graph {getNodes = [], getEdges = []}

Function: removeNode
Test Case Number: 3
Input: 1 (Graph [(Node 1 'A'),(Node 2 'C')] [(0,1),(2,2)])
Expected Output: Graph {getNodes = [Node {getNodeID = 2, getNodeVal = 'C'}], getEdges = [(2,2)]}
(since there is a node and an edge with the desired NodeID that we want to remove, they should both be removed from the graph. It then should return a graph only with nodes and edges without the NodeID that we wanted to remove)
Actual Output: Graph {getNodes = [Node {getNodeID = 2, getNodeVal = 'C'}], getEdges = [(2,2)]}

Function: lookupNode
Test Case Number: 1
Input: 0 (Graph [] [(0,0)])
Expected Output: Nothing
(we are only looking at nodes with that NodeID, and since there are no nodes in the graph, it should return nothing)
Actual Output: Nothing

Function: lookupNode
Test Case Number: 2
Input: 0 (Graph [Node 0 "a"] [(1,0)])
Expected Output: Just (Node {getNodeID = 0, getNodeVal = "a"})
(there is a node with that desired NodeID value, so it should return that node in the graph in a Maybe a type)
Actual Output: Just (Node {getNodeID = 0, getNodeVal = "a"})

Function: lookupNode
Test Case Number: 3
Input: 1 (Graph [(Node 0 "a"),(Node 2 "b")] [])
Expected Output: Nothing
(although we do have nodes in the graph, since there are no nodes with the corresponding NodeID, it should return nothing)
Actual Output: Nothing

Function: insertEdge
Test Case Number: 1
Input: (0,1) (Graph [] [])
Expected Output: Nothing
(since the graph has no nodes, it should return Nothing)
Actual Output: Nothing

Function: insertEdge
Test Case Number: 2
Input: (0,1) (Graph [(Node 1 'A'),(Node 2 'C')] [])
Expected Output: Nothing
(while the graph does contain Node 1, it does not contain Node 0. Since there is a node that does not correspond to the input NodeID, it should return Nothing))
Actual Output: Nothing

Function: insertEdge
Test Case Number: 3
Input: (2,1) (Graph [(Node 1 'A'),(Node 2 'C')] [])
Expected Output: Just (Graph {getNodes = [Node {getNodeID = 1, getNodeVal = 'A'},Node {getNodeID = 2, getNodeVal = 'C'}], getEdges = [(2,1)]})
(since both nodes with the input NodeIDs exist, and the edge does not already exist, it should return Just the original graph, but with the edge added)
Actual Output: Just (Graph {getNodes = [Node {getNodeID = 1, getNodeVal = 'A'},Node {getNodeID = 2, getNodeVal = 'C'}], getEdges = [(2,1)]})
-}