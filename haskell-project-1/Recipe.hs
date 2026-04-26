module PE1 where

import Text.Printf

-- PE1: Recipe Calculator
-- The premise of this homework if to write a recipe calculator that
-- calculates: how much a recipe costs to make, what can be made with the
-- ingredients already available, and how much extra ingredients need to
-- be bought in order to make a recipe.

-- Recipe = Recipe Name [(Ingredient, Quantity)]
data Recipe = Recipe String [(String, Double)] deriving Show
nam :: Recipe -> String
nam (Recipe n _) = n
lst :: Recipe -> [(String, Double)]
lst (Recipe _ ls) = ls

-- Price = Price Ingredient Quantity Price
data Price = Price String Double Double deriving Show
ing :: Price -> String
ing (Price i _ _) = i
qua :: Price -> Double
qua (Price _ aa _) = aa
pr :: Price -> Double
pr (Price _ _ bb) = bb

-- You can use this as-is
getRounded :: Double -> Double 
getRounded x = read s :: Double
               where s = printf "%.2f" x
              

-- Calculate how much the given amount of the given ingredient costs
getIngredientCost :: (String, Double) -> [Price] -> Double
getIngredientCost (a1,a2) [] = 0
--getIngredientCost (0) _ = 0 
getIngredientCost (a1,a2) (f:r) = 
    let c = a2 / (qua f) * (pr f) 
    in  if a1 == (ing f) then getRounded c 
        else getIngredientCost (a1,a2) (r)
                             
gt :: Price -> [(String, Double)] -> Double
gt a [] = 0
gt a (f:r) =
    let c = pr a * (snd f) / (qua a)
    in  if (fst f)==(ing a) then getRounded c
        else gt a r

-- Calculate how much it costs to buy all the ingredients of a recipe
recipeCost :: Recipe -> [Price] -> Double
recipeCost a [] = 0
recipeCost a (f:r) = (gt f (lst a)) + (recipeCost a r)
    

-- Given a list of how much you already have of each ingredient,
-- calculate how much of which ingredients are missing for a recipe
missingIngredients :: Recipe -> [(String, Double)] -> [(String, Double)]
missingIngredients b [] = []
missingIngredients b (f:r) =  (missing b (f:r)) ++ (mi2 (lst b) (f:r))

missing :: Recipe -> [(String, Double)] -> [(String, Double)]
missing b [] = []
missing b (f:r) = (mi f (lst b))++(missing b r) 

mi3 :: [(String,Double)] -> [String]
mi3 [] = []
mi3 (f:r)= (fst f) : mi3 r

mi2 :: [(String,Double)] -> [(String,Double)] -> [(String, Double)]
mi2 [] c = []
mi2 (a:b) c | elem (fst a) (mi3 c) == False = a : mi2 b c
            | otherwise = mi2 b c

mi :: (String, Double) -> [(String,Double)] -> [(String,Double)]
mi (a,b) [] = []
mi (a,b) (f:r) = if a==(fst f) then (if (snd f)>b then (fst f, (snd f)-b):[] else []) else mi (a,b) r 
               

-- Given a list of ingredients in your kitchen, calculate what you would
-- have left after making the given recipe. If there isn't enough of an
-- ingredient, the recipe cannot be made! You shouldn't change the amount
-- of ingredient in that case.
makeRecipe :: [(String, Double)] -> Recipe -> [(String, Double)]
makeRecipe a b = if mr2 (lst b) a == False then a else mr4 a (lst b)

index::[String]->String->Int
index (a:b) s = if s==a then 0 else 1 + index b s

mr4::[(String, Double)] -> [(String, Double)] -> [(String, Double)]
mr4 [] r = []
mr4 (a:b) r = if elem (fst a) (mi3 r)==False then a:mr4 b r else ((fst a),(snd a)-(snd (r !! (index (mi3 r) (fst a) )))):mr4 b r

mr3::[(String, Double)] -> [(String, Double)] -> Bool
mr3 [] a = True
mr3 (a:b) c = if elem (fst a) (mi3 c)==False then False else mr3 b c 

mr2::[(String, Double)] -> [(String, Double)] -> Bool
mr2 [] c = True
mr2 (a:b) c = mr c a && mr2 b c && mr3 (a:b) c
mr::[(String, Double)] -> (String, Double) -> Bool
mr [] a = True
mr (a:b) s = if (fst s)==(fst a) then (if (snd a) - (snd s)>=0 then True else False) else mr b s

-- Given a list of ingredients you already have, and a list of recipes,
-- make a shopping list showing how much of each ingredient you need
-- to buy, and its cost. Each ingredient mush appear in the shopping list
-- at most once (no duplicates!).
makeShoppingList :: [(String, Double)] -> [Recipe] -> [Price] -> [(String, Double,Double)]
makeShoppingList s r p = mk5 (mk3 (rms (mk r ++ mk6(mk r)))s) p

mk::[Recipe]->[(String, Double)]
mk []=[]
mk (f:r)= (lst f) ++ mk r

rms::[(String, Double)]->[(String, Double)]
rms [] = []
rms (f:r)=if elem (fst f) (mi3 r)==False then f:(rms r) else rms r

mk2::(String, Double)->[(String, Double)]->[(String, Double)]
mk2 a [] = [a]
mk2 a (f:r)= if (fst a)==(fst f) then (if (snd a)<=(snd f) then [] else [((fst a),(snd a)-(snd f))]) else mk2 a r

mk3::[(String, Double)]->[(String, Double)]->[(String, Double)]
mk3 [] c = []
mk3 (f:r) c = mk2 f c ++ mk3 r c

mk4::(String, Double)->[Price]->[(String, Double,Double)]
mk4 (a,b) [] = []
mk4 (a,b) (f:r) = if a==(ing f) then [(a,b,b*(pr f)/(qua f))] else mk4 (a,b) r 

mk5::[(String, Double)]->[Price]->[(String, Double,Double)]
mk5 [] c =[]
mk5 (a:b) c= mk4 a c ++ mk5 b c 

mk6::[(String, Double)]->[(String, Double)]
mk6 [] = []
mk6 (f:r)=if elem (fst f) (mi3 r)==True then ((fst f),(snd f)+snd (r!!(index (mi3 r) (fst f)))):mk6 r else mk6 r










