module PE2 where

-- PE2: Dungeon Crawler
-- Dungeon map is :: Tree Chamber [Encounter]
-- Each encounter is either a fight or a treasure
-- Fights deal you damage (reduce HP) but enemies drop some gold (add
-- gold)
-- Tresures just give gold, or potions (which give hp)
-- Nodes hold encounters, when you visit a node you go through all of them in order
-- You start with a certain amount of HP and 0 gold.
-- You lose HP and accumulate gold as you descend the tree and go through encounters

-- Polymorphic tree structure
data Tree a b = EmptyTree | Leaf a b | Node a b [Tree a b] deriving (Show, Eq)

-- Every location in the tree is of some Chamber type.
data Chamber = Cavern |
               NarrowPassage |
               UndergroundRiver |
               SlipperyRocks deriving (Show, Eq)

-- An enemy has a name, an amount of damage that it deals
-- and an amount of gold that it drops (in that order).
data Enemy = Enemy String Integer Integer deriving (Show, Eq)

-- Gold n gives n amount of gold
-- Potion n heals n hp
data Loot = Gold Integer | Potion Integer deriving (Show, Eq)

-- An encounter is either a Fight with an Enemy, or a treasure where
-- you find Loot
data Encounter = Fight Enemy | Treasure Loot deriving (Show, Eq)

-- This is a type synonym for how we will represents our dungeons
type Dungeon = Tree Chamber [Encounter]

name::Enemy->String
name(Enemy a _ _) = a
dam::Enemy->Integer
dam(Enemy _ a _) = a
gol::Enemy->Integer
gol(Enemy _ _ a) = a

tup :: [Integer] -> (Integer,Integer)
tup [x,y] = (x,y)


ch::Tree Chamber [Encounter]->[Tree Chamber [Encounter]]
ch(Leaf a b)=[]
ch(EmptyTree) = []
ch(Node a b c)= c
enc::Tree Chamber [Encounter]->[Encounter]
enc (Node a b c) = b
enc (Leaf a b) = b
enc (EmptyTree) = []
cham::Tree Chamber [Encounter]->Chamber
cham (Node a b c) = a
cham (Leaf a b) = a

-- First argument is starting HP
-- Second argument is the dungeon map
-- Third argument is the path (each integer in the list shows what child
-- you descend into)
-- Calculate how much HP you have left and how much gold you've
-- accumulated after traversing the given path
traversePath :: Integer -> Dungeon -> [Int] -> (Integer, Integer)
traversePath a d (f:r) = tup (zipWith (+) (tr3 a (enc d)) (zipWith (+) (tr4 a d (f:r)) [a,0]))

tr4 ::Integer ->Dungeon -> [Int] -> [Integer]
tr4 a d [] = [0,0]
tr4 a d (f:r) =  zipWith (+) (tr3 a (enc ((ch d)!!f))) (tr4 a ((ch d)!!f) r)

tr1::Loot->[Integer]->[Integer]
tr1 (Gold a) [b,c] = [b,a+c]
tr1 (Potion a) [b,c] = [a+b,c]

tr2::Encounter->[Integer]-> [Integer]
tr2 (Fight a) [b,c] = [b-(dam a),c+(gol a)]
tr2 (Treasure a) b =  (tr1 a b)

tr3::Integer->[Encounter]->[Integer]
tr3 a [] = [0,0]
tr3 a (f:r) =zipWith (+) (tr2 f [0,0]) (tr3 a r)


-- First argument is starting HP
-- Second argument is dungeon map
-- Find which path down the tree yields the most gold for you
-- You cannot turn back, i.e. you'll find a non-branching path
-- You do not need to reach the bottom of the tree
-- Return how much gold you've accumulated
findMaximumGain :: Integer -> Dungeon -> Integer
findMaximumGain a d =let g=(fm [a,0] (enc d) [a,0])
                     in (fm3 (ch d) g g g) !! 1
                         
fm3::[Dungeon]->[Integer]->[Integer]->[Integer]->[Integer]
fm3 [] [a,b] [c,k] [n,m]= [a,b]
fm3 (d:r) [a,b] [c,k] [n,m]= let aa=fm [n,m] (enc d) [n,m]
                                 f1 = fm2 aa aa (ch d) aa
                             in if a>0 && (f1!!1)>b then fm3 r f1 [c,k] [n,m] else fm3 r [a,b] [c,k] [n,m]

fm::[Integer]->[Encounter]->[Integer]->[Integer]
fm [a,b] [] [n,m] = [a,b]
fm [a,b] (f:r) [n,m] =  let c=[a,b]
                            d=(tr2 f c)
                        in if (d!!0)>0 then (fm d r [n,m]) else [n,m]
                 
fm2::[Integer]->[Integer]->[Dungeon]->[Integer]->[Integer]
fm2 [a,b] [c,d] [] [n,m]= [a,b]
fm2 [a,b] [c,k] (d:r) [n,m]=let f1 =  fm [c,k] (enc d) [a,b] 
                            in if (a>0) && (f1!!1)>(b) then fm2 f1 [c,k] r [n,m] else fm2 [a,b] [c,k] r [n,m]

-- First argument is starting HP
-- Second argument is the dungeon map
-- Remove paths that you cannot go thorugh with your starting HP. (By
-- removing nodes from tree).
-- Some internal nodes may become leafs during this process, make the
-- necessary changes in such a case.
findViablePaths :: Integer -> Dungeon -> Dungeon
findViablePaths a d = fv a d
fv::Integer->Dungeon->Tree Chamber [Encounter]
fv a d | a<=((-1)*(fm [0,0](enc d) [0,0])!!0) = EmptyTree
       | a<=((-1)*(((tr3 0 (enc d))!!0)+((tr3 0 (enc ((ch d)!!0)))!!0))) = Leaf (cham d) (enc d)
       | a<35 = Node (cham d) (enc d) (fv2 [a,0] (ch d))
       | otherwise =  d --ATTENTION
       
fv2::[Integer]->[Dungeon]->[Dungeon]
fv2 [a,b] [] = []
fv2 [a,b] (d:r) = let f= fm [a,b] (enc d) [a,b]
                  in if (f!!0)>0 then Node (cham d) (enc d) (fv3  f (ch d)) : fv2 f r else fv2 [a,b] r

fv3::[Integer]->[Dungeon]->[Dungeon]
fv3 [a,b] [] = []
fv3 [a,b] (d:r) = let f= fm [a,b] (enc d) [a,b]
                  in if (f!!0)>0 then Leaf (cham d) (enc d) : fv3 f r else fv3 [a,b] r
-- First argument is starting HP
-- Second Argument is dungeon map
-- Find, among the viable paths in the tree (so the nodes you cannot
-- visit is already removed) the two most distant nodes, i.e. the two
-- nodes that are furthest awat from each other.
mostDistantPair :: Integer -> Dungeon -> (Integer, Dungeon)
mostDistantPair _ _ = (0, EmptyTree)

-- Find the subtree that has the highest total gold/damage ratio
-- Simply divide the total gold in the subtree by the total damage
-- in the subtree. You only take whole subtrees (i.e you can take a new
-- node as the root of your subtree, but you cannot remove nodes
-- below it). Note that the answer may be the whole tree.
mostEfficientSubtree :: Dungeon -> Dungeon
mostEfficientSubtree _ = EmptyTree
