module MicroKanren where

import Data.List (transpose)
import Data.Maybe (fromMaybe)

data Term
  = Var Int
  | Atom String [Term]
  deriving (Eq)

instance Show Term where
  show (Var i) = "_." ++ show i
  show (Atom a []) = a
  show (Atom a as) = a ++ show as

sym :: String -> Term
sym a = Atom a []

nil :: Term
nil = sym "Nil"

cons :: Term -> Term -> Term
cons a b = Atom "Cons" [a, b]

car :: Term -> Term
car (Atom "Cons" [a, _]) = a

cdr :: Term -> Term
cdr (Atom "Cons" [_, b]) = b

list :: [Term] -> Term
list = foldr cons nil

append :: Term -> Term -> Term
append l t
  | nil == l = t
  | otherwise = cons (car l) (append (cdr l) t)

type Substitution = [(Int, Term)]

substEmpty :: Substitution
substEmpty = []

substAdd :: Substitution -> Int -> Term -> Substitution
substAdd s i t = (i, t) : s

substApply :: Substitution -> Term -> Term
substApply s t@(Var i) = fromMaybe t (lookup i s)
substApply s (Atom a as) = Atom a (map (substApply s) as)

substApplyRec :: Substitution -> Term -> Term
substApplyRec s t =
  case substApply s t of
    t'@(Var i)
      | t' == t -> t'
      | otherwise -> substApplyRec s t'
    (Atom a as) -> Atom a (map (substApplyRec s) as)

unify :: Substitution -> Term -> Term -> Maybe Substitution
unify s t1 t2 = (++s) `fmap` unify' s (substApply s t1) (substApply s t2)
  where
    unify' s (Var i) b = Just (substAdd s i b)
    unify' s a (Var j) = Just (substAdd s j a)
    unify' s (Atom a as) (Atom b bs)
      | a == b && length as == length bs = unify'' s as bs
      | otherwise = Nothing
      where
        unify'' s [] [] = Just s
        unify'' s (x:xs) (y:ys) = unify' s x y >>= (\s' -> unify'' s' xs ys)

type State = (Substitution, Int)
  
stateEmpty :: State
stateEmpty = (substEmpty, 0)

type Goal = State -> [State]

(===) :: Term -> Term -> Goal
(===) a b (s, c) =
  case unify s a b of
    Nothing -> []
    Just s' -> [(s', c)]

exists :: (Term -> Goal) -> Goal
exists f (s, c) = f (Var c) (s, c + 1)

(/\) :: Goal -> Goal -> Goal
(/\) g1 g2 st = g1 st >>= g2

(\/) :: Goal -> Goal -> Goal
(\/) g1 g2 st = interleave (g1 st) (g2 st)
  where
    interleave x y = concat (transpose [x, y])

conde :: [Goal] -> Goal
conde = foldl1 (\/)

exists2 :: (Term -> Term -> Goal) -> Goal
exists2 f (s, c) = f (Var c) (Var $ c + 1) (s, c + 2)

exists3 :: (Term -> Term -> Term -> Goal) -> Goal
exists3 f (s, c) = f (Var c) (Var $ c + 1) (Var $ c + 2) (s, c + 3)

infixl 5 /\
infixl 5 \/

reifyState :: [Int] -> State -> Substitution
reifyState vars (s, _) = map (\i -> (i, substApplyRec s (Var i))) vars

reify :: [Int] -> [State] -> [Substitution]
reify vars = map (reifyState vars)

run :: Int -> [Int] -> Goal -> [Substitution]
run n vars g = take n (run' vars g)

run' :: [Int] -> Goal -> [Substitution]
run' vars g = reify vars (g stateEmpty)

caro :: Term -> Term -> Goal
caro p a = exists (\b -> p === cons a b)

cdro :: Term -> Term -> Goal
cdro p b = exists (\a -> p === cons a b)

membero :: Term -> Term -> Goal
membero x l = conde [caro l x, exists (\b -> cdro l b /\ membero x b)]

appendo :: Term -> Term -> Term -> Goal
appendo l1 l2 l3 =
  conde
    [ nil === l1 /\ l2 === l3
    , exists3 (\x y z -> l1 === cons x y /\ l3 === cons x z /\ appendo y l2 z)
    ]

-- [[(0,_.0)]]
egMembero1 = run' [0] $ exists (\x -> membero x (list [x]))

-- [[(0,X)],[(0,Y)]]
egMembero2 = run' [0] $ exists (\x -> membero x (list [sym "X", sym "Y"]))

-- [[(0,Cons[X,Cons[Y,Nil]])]]
egAppendo1 = run' [0] $ exists (\x -> appendo (list [sym "X"]) (list [sym "Y"]) x)

-- [[(0,Nil),(1,Cons[X,Cons[Y,Nil]])]
-- ,[(0,Cons[X,Nil]),(1,Cons[Y,Nil])]
-- ,[(0,Cons[X,Cons[Y,Nil]]),(1,Nil)]
-- ]
egAppendo2 = run' [0,1] $ exists2 (\x y -> appendo x y (list [sym "X", sym "Y"]))
