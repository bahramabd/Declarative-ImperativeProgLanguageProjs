{-# LANGUAGE FlexibleInstances #-}
module PE3 where

import Data.List (sort, sortBy)
import Text.Printf (printf)

data Term = Const Integer | Pw Integer Power | Trig Integer Power Trigonometric | Exp Integer Power Exponential

data Power = Power Integer
data Polynomial = Polynomial [(Integer, Power)]
data Exponential = Exponential Polynomial
data Trigonometric = Sin Polynomial | Cos Polynomial

class Evaluable a where
    function :: a -> (Integer -> Double)

class Differentiable a where
    derivative :: a -> [Term]

-- You can use this as is
getRounded :: Double -> Double 
getRounded x = read s :: Double
               where s = printf "%.2f" x

-- You don't have to follow the order the functions appear in the file
-- For example, you could first define all Show instances, then all Eq instances etc.
-- if that implementation order is more convenient for you.

ip::Power->Integer
ip (Power a) = a
 
pshow::Power->String
pshow a| (ip a)==0 = "1" 
       | (ip a)==1 = "x" 
       | otherwise =  "x^" ++ show (ip a)

-- INSTANCES FOR POWER

instance Show Power where
    show a = pshow a
           

instance Eq Power where -- should we write new ones??
    a == b |(ip a)==(ip b) = True
           | otherwise = False
--instance Uneq Power where
  --  a != b |(ip a)==(ip b) = False
    --       | otherwise = True

instance Ord Power where
    a <= b | (ip a) <= (ip b) =True
           | otherwise = False
    a >= b | (ip a) >= (ip b) =True
           | otherwise = False
    a < b | (ip a) < (ip b) =True
           | otherwise = False
    a > b | (ip a) > (ip b) =True
           | otherwise = False
           
funcpow::Integer->Power->Double
funcpow a (Power 0) = 1.0
funcpow a (Power 1) = fromIntegral a::Double
funcpow a (Power b) = (fromIntegral a::Double) * funcpow a (Power (b-1))
 

instance Evaluable Power where
    function b = \a -> funcpow a b
    
difpow::Power->[Term]
difpow (Power a) | a==0 = [Const 0]
                 | a==1 = [Const 1]
                 | a==2 = [Pw 2 (Power 1)]
                 | otherwise = [Pw a (Power (a-1))]



instance Differentiable Power where
    derivative p = difpow p 

join :: String -> [String] -> String
join _ [] = "" -- empty list case, an empty string
join _ [str] = str -- single string case, only that string
join joinStr (firstStr:rest) = firstStr ++ joinStr ++ join joinStr rest

polshow::[(Integer,Power)]->String
polshow [] = ""
polshow ((a,b):[])| (ip b)==0 = (show a) -- a==0
                  | (ip b)==1 = if a==1 then "x" else if a== (-1) then "-x" else (show a)++"x"
                  | otherwise = if a==1 then join " + " ["x^"++(show (ip b))] else if  a== (-1) then join " + " ["-x^"++(show (ip b))] else (show a)++"x^"++(show (ip b))

polshow ((a,b):r) | (ip b)==0 = (show a) ++ polshow r
                  | (ip b)==1 = if a==1 then join " + " ["x",polshow r] else if a== (-1) then join " + " ["-x",polshow r] else join " + " [(show a)++"x",polshow r]
                  | otherwise = if a==1  then join " + " ["x^"++(show (ip b)),polshow r] else if a== (-1) then join " + " ["-x^"++(show (ip b)),polshow r] else join " + " [(show a)++"x^"++(show (ip b)),polshow r]

-- INSTANCES FOR POLYNOMIAL


instance Show Polynomial where
    show (Polynomial a) = polshow a
    

funcpol::Integer->Polynomial->Double
funcpol a (Polynomial []) = 0.0
funcpol a (Polynomial ((i,p):rest)) = (fromIntegral i::Double) * (funcpow a p) +  funcpol a (Polynomial rest)

instance Evaluable Polynomial where
    function p = \a -> funcpol a p
    
difpol::Polynomial->[Term]
difpol (Polynomial []) = []
difpol (Polynomial ((a,Power b):rest)) | b==0 = (difpol (Polynomial rest))
                                       | b==1 = if a==0 then (difpol (Polynomial rest)) else [Pw a (Power 0)]++(difpol (Polynomial rest))
                                       | otherwise = if a==0 then (difpol (Polynomial rest)) else [Pw (a*b) (Power (b-1))]++(difpol (Polynomial rest))

instance Differentiable Polynomial where
    derivative p = difpol p


trigshow::Trigonometric->String --edge cases sin^0 
trigshow (Sin (Polynomial [((-1),(Power 1))])) = "sin(-x)"
trigshow (Cos (Polynomial [((-1),(Power 1))])) = "cos(-x)"
trigshow (Sin (Polynomial a))=if length (polshow a)>2 then "sin(" ++(polshow a)++")" else if (polshow a=="0") then "0" else "sin"++(polshow a)
trigshow (Cos (Polynomial a))=if length (polshow a)>2 then "cos(" ++(polshow a)++")" else if (polshow a=="0") then "1" else "cos"++(polshow a)

-- INSTANCES FOR TRIGONOMETRIC

instance Show Trigonometric where
    show a = trigshow a
    
functrig::Integer->Trigonometric->Double
functrig a (Sin pol) =  getRounded (sin(funcpol a pol))
functrig a (Cos pol) =  getRounded (cos(funcpol a pol)) 

instance Evaluable Trigonometric where
    function t = \a -> functrig a t
diftrig1::Trigonometric->[Term]
diftrig1 (Sin p) = difpol p
diftrig1 (Cos p) = difpol p

diftrig::Trigonometric->[Term]->[Term]
diftrig t [] = [] 
diftrig (Sin p) (a:rest) = [Trig (intr a) (obri a) (Cos p)] ++ diftrig (Sin p) rest
diftrig (Cos p) (a:rest) = [Trig (-1*intr a) (obri a) (Sin p)] ++ diftrig (Cos p) rest

instance Differentiable Trigonometric where
    derivative t = diftrig t (diftrig1 t)


eshow::Exponential->String -- edge cases e^0
eshow (Exponential (Polynomial [((-1),(Power 1))])) = "e^(-x)"
eshow (Exponential (Polynomial a))=if length (polshow a)>2 then "e^(" ++(polshow a)++")" else if (polshow a=="0") then "1" else "e"++(polshow a)


-- INSTANCES FOR EXPONENTIAL

instance Show Exponential where
    show a = eshow a
    
funce::Integer->Exponential->Double
funce a (Exponential pol) = getRounded (exp(funcpol a pol) )

instance Evaluable Exponential where
    function e = \a -> funce a e
    
dife1::Exponential->[Term]
dife1 (Exponential p)  = difpol p
     
dife::Exponential->[Term]->[Term]                
dife e [] = []
dife e (a:rest) = [Exp (intr a) (obri a) e] ++ dife e rest


instance Differentiable Exponential where
    derivative e = dife e (dife1 e)

intr::Term->Integer
intr(Const a) = a
intr(Pw a b) = a
intr(Exp a b c) = a
intr(Trig a b c) = a

obri::Term->Power
obri(Const a) = Power 0
obri (Pw a b) = b
obri(Exp a b c) = b
obri(Trig a b c) = b

-- INSTANCES FOR TERM
termshow::Term->String
termshow(Const a) =(show a)
termshow(Pw a b) | (ip b)==0 = (show a)
                 |  a==1 = pshow b 
                 | a== (-1) = "-"++pshow b 
                 | otherwise = (show a) ++ pshow b -- EDGE CASES FOR PSHOW B == 0 1 and show a (show a)++(pshow b)++(eshow c) Exp (-1) (Power 0) (Exponential (Polynomial [(-1, Power 1)] 
termshow(Exp a b c)  | (ip b)==0 = if a==1 then eshow c else if a==(-1) then "-"++eshow c else  (show a)++(eshow c) 
                     | a==1 = (pshow b)++(eshow c)
                     | a== (-1) = "-"++(pshow b)++(eshow c)
                     | otherwise = (show a)++(pshow b)++(eshow c)
termshow(Trig a b c) | (ip b)==0 = if a==1 then (trigshow c) else if a==(-1) then "-"++trigshow c else (show a)++(trigshow c) 
                     | a==1 = (pshow b)++(trigshow c)
                     | a== (-1) = "-"++(pshow b)++(trigshow c)
                     | otherwise = (show a) ++(pshow b)++(trigshow c)

instance Show Term where
    show a = termshow a -- CONSIDER + CASES FOR ALL !!!!!!!!!!!!!!!!!!!

termfunc::Integer->Term->Double
termfunc i (Const a) = fromIntegral a::Double
termfunc i (Pw a b) = (funcpow i b) * (fromIntegral a::Double)
termfunc i (Exp a b c)  = (funce i c) * (fromIntegral a::Double) * (funcpow i b)
termfunc i (Trig a b c) = (functrig i c) * (fromIntegral a::Double) * (funcpow i b)

instance Evaluable Term where
    function t = \a -> termfunc a t

dt1::Term->[Term]
dt1 (Exp a b c) = difpol (Polynomial [(a,b)])
dt1 (Trig a b c) = difpol (Polynomial [(a,b)])
polm::Term->[Term]->[Term]
polm t [] = []
polm (Pw i (Power p)) ((Pw a (Power b)):rest) = [Pw (a*i) (Power(p+b))]++polm (Pw i (Power p)) rest

difterm::Term->[Term]
difterm (Const a) = []
difterm (Pw a (Power b)) | (intr (Pw a (Power b)))==0 = []
                         | otherwise = if b==0 then [] else [Pw (a*b) (Power (b-1))] 
difterm (Exp a b (Exponential p)) | (intr (Exp a b (Exponential p)))==0 = []
                                  | otherwise = let i = dt1 (Exp a b (Exponential p))
                                                in if (length i)==1 then [Exp (intr  (i!!0)) (obri  (i!!0)) (Exponential p)] ++ dife (Exponential p) (polm (Pw a b) (difpol p))
                                                   else dife (Exponential p) (polm (Pw a b) (difpol p))
difterm (Trig a b (Sin p))  | (intr (Trig a b (Sin p)))==0 = []
                            | otherwise = let i = dt1 (Trig a b (Sin p))
                                          in if (length i)==1 then [Trig (intr  (i!!0)) (obri  (i!!0)) (Sin p)] ++ diftrig (Sin p) (polm (Pw a b) (difpol p))
                                            else diftrig (Sin p) (polm (Pw a b) (difpol p))
difterm (Trig a b (Cos p))  | (intr (Trig a b (Cos p)))==0 = []
                            | otherwise = let i = dt1 (Trig a b (Cos p))
                                          in if (length i)==1 then [Trig (intr  (i!!0)) (obri  (i!!0)) (Cos p)] ++ diftrig (Cos p) (polm (Pw a b) (difpol p))
                                            else diftrig (Cos p) (polm (Pw a b) (difpol p))

instance Differentiable Term where
    derivative t = difterm t


evterm::Integer->[Term]->Double
evterm _ [] = 0.0
evterm i (a:rest) = let aa = (termfunc i a + evterm i rest ) in getRounded aa

-- INSTANCES FOR [TERM]

instance Evaluable [Term] where
    function a = \x -> evterm x a

dterm::[Term]->[Term]
dterm [] = []
dterm (a:r)= difterm a ++ dterm r 

instance Differentiable [Term] where
    derivative t = dterm t
    
    


    
    
    
    
    
    


