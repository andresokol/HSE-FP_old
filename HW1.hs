module HW01 where

{-
1. Задача на лямбда-исчисление

1.1. Уберите скобки в следующем лямбда-терме, произвести редукцию. Расписать пошагово:

((λ p. (λ q. ((q (p r)) s))) ((q ((λ p. p) r)) s))

((λ p. (λ q. ((q (p r)) s))) ((q (r)) s))

((λ p. (λ q. ((q (p r)) s))) ((q r) s))

((λ p. (λ q. ((q (p r)) s))) (q r s))

(λ p. (λ q. q (p r) s)) (q r s)

(λ p. (λ Q. Q (p r) s)) (q r s)

(λ P.λ Q. Q (P r) s) (q r s)

(λ Q. Q ((q r s) r) s) 

(λ Q. Q (q r s r) s)

1.2. Аналогично:

((λ a. λ b. (λ x. x) b a (a b x) ((λ a. (λ b. a)) x)) (λ b. b)) [x := b]

((λ a. λ b. b a (a b x) ((λ a. (λ b. a)) x)) (λ b. b)) [x := b]

((λ a. λ b. b a (a b x) ((λ a. (λ b. a)) x)) (λ b. b)) [x := b]

((λ a. λ b. b a (a b x) ((λ a. (λ b. a)) x)) (λ Z. Z)) [x := b]

((λ a. λ b. b a (a b x) (λ b. x)) (λ Z. Z)) [x := b]

(λ b. b (λ Z. Z) ((λ Z. Z) b x) (λ b. x)) [x := b]

(λ b. b (λ Z. Z) (b x) (λ b. x)) [x := b]

(λ b. b (λ Z. Z) (b x) (λ Q. x)) [x := b]

(λ b. b (λ Z. Z) (b x) (λ Q. x)) [x := b]

(λ P. P (λ Z. Z) (P x) (λ Q. x)) [x := b]

(λ P. P (λ Z. Z) (P b) (λ Q. b))

-}

{-
Правило редукции:
(λ x. M) N -> M [x := N]

Правила избавления от скобок:
1. M N P = (M N) P
2. λ x. λ y. M = λ x. (λ y. M)
3. λ x y. M = λ x. λ y. M
-}

{-
2. Реализовать алгоритм Евклида:
-}

-- mod :: Integer -> Integer -> Integer
-- mod x y = if x < y then x else mod x-y y

euclid :: Integer -> Integer -> Integer
euclid a 0 = a
euclid a b = euclid b (mod a b)

{-
3. Реализуйте функцию Эйлера:
https://en.wikipedia.org/wiki/Euler%27s_totient_function
-}

reduce :: (a -> b -> b) -> [a] -> b -> b

reduce f [x] acc = f x acc
reduce f (x:xs) acc = reduce f xs (f x acc)

eulerTotient :: Integer -> Integer
eulerTotient n = let relativelyPrime = map (\x -> if euclid n x == 1 then 1 else 0) [1..n-1] in
    reduce (+) (relativelyPrime) 0

{-
4. Не пользуясь стандартными функциями, реализуйте возведение в степень:
-}

my_exp :: Integer -> Integer -> Integer
my_exp x 1 = x
my_exp x n = x * my_exp x (n-1)

fast_exp :: Integer -> Integer -> Integer
fast_exp a 1 = a
fast_exp a b = case mod b 2 of
    0 -> fast_exp a (div b 2) ^ 2
    1 -> a * fast_exp a (b-1)

-- main = print(my_exp 3 7, fast_exp 3 7, 3 ^ 7)

{-
5. Функция integrate принимает одноместную функцию f :: Double -> Double, два вещественных числа a, b :: Double
и возвращает определенный интерграл функции f на отрезке [a,b], который вычисляется методом трапеций:
-}


integrate
    :: (Double -> Double)
    -> Double
    -> Double
    -> Double
integrate f a b = (b - a) * (f a + f b) / 2

integratePrecise
    :: (Double -> Double)
    -> Double
    -> Double
    -> Double
integratePrecise f a b = let c = (a + b) * 0.5 in
    if abs (f a - f b) > 0.01
        then integratePrecise f a c + integratePrecise f c b
        else integrate f a b

main = print(integratePrecise (\x -> x^2*sin(x)) 0.2 20)

{- 6. Заселить следующие типы термами: -}

-- # 6.1:

permute :: (a -> b -> c) -> b -> a -> c
permute f x y = f y x

-- # 6.2:

pairProd :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
pairProd f g (x, y) = (f x, g y)

-- # 6.3:

fix :: (a -> a) -> a
fix f = f (fix f)
-- подсказка к # 6.3: вспомнить про комбинатор неподвижной точки, о котором говорилось на второй лекции:

-- # 6.4

weirdFunction
    :: (d -> d -> b)
    -> (a -> b -> c)
    -> (d -> b)
    -> d -> b
weirdFunction f g h x = f x x

{-
7. Определим тип ко-списков, где, в отличие от обычных списков, новый элемент добавляется не в голову, а
в хвост.
-}

data CoList a = Nil | Snoc (CoList a) a
    deriving (Show, Eq)

{-7.1 Реализовать функцию, которая по ко-списку возвращает список -}

coListToList :: CoList a -> [a]
coListToList (Snoc Nil x) = [x]
coListToList (Snoc xs x) = reverse(x : reverse (coListToList xs))

-- main = print(coListToList (Snoc (Snoc (Snoc (Snoc Nil 1) 2) 3) 4))

{-7.2 Реализовать конкатенацию ко-списков.
Реализация функции должна удовлетворять следующему равенству:
coListToList (coListConcat a b) = (coListToList a) ++ (coListToList b)
-}

coListConcat :: CoList a -> CoList a -> CoList a
coListConcat x Nil = x
coListConcat x (Snoc ys y) = Snoc (coListConcat x ys) y

p = (Snoc (Snoc (Snoc (Snoc Nil 1) 2) 3) 4)
q = (Snoc (Snoc (Snoc (Snoc Nil 5) 6) 7) 8)

-- main = print(coListToList (coListConcat p q), (coListToList p) ++ (coListToList q))

{-
8. Определим тип деревьев с двоичным ветвлением
-}

data Tree a = Leaf | Node (Tree a) a (Tree a)
    deriving (Show, Eq)

-- # 8.1 Реализовать instance класса типов Functor для деревьев

instance Functor Tree where
    fmap f Leaf = Leaf
    fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r) 

-- # 8.2. Реализовать функцию, которая возвращает список элементов дерева

treeToList :: Tree a -> [a]
treeToList Leaf = []
treeToList (Node left x right) = (treeToList left) ++ [x] ++ (treeToList right)

sampleTree = (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf))

-- # 8.3 Аналогично для ко-списков

treeToCoList :: Tree a -> CoList a
treeToCoList Leaf = Nil
treeToCoList (Node left x right) = coListConcat (Snoc (treeToCoList left) x) (treeToCoList right)

-- main = print(treeToCoList d)

{- # 8.4 Реализовать проверку на пустоту -}

isEmpty :: Tree a -> Bool
isEmpty Leaf = True
isEmpty _ = False

{- # 9. В стандартной библиотеке языка Haskell определен двухпараметрический тип Either,
data Either a b = Left a | Right b, семантика которого похожа на семантику Maybe, который обсуждался на семинаре.
Если нужное нам вычисление закончилось хорошо, то мы кладем результат в Right (правое вложение), а если
вычисление закончилось не очень хорошо, то мы применяем Left, который, в отличие от Nothing, еще требует объекта
некоторого типа a
Пример:
divideEither :: (Fractional a, Eq a) => a -> a -> Either String a
divideEither a b =
    if b == 0 then (Left "cho ti delash, ne deli na nol' ples") else Right (a / b)
> divideEither 5 0
Left "cho ti delash, ne deli na nol' ples"
> divideEither 5 6
Right 0.8333333333333334
 -}

-- # 9.1 Заселить данный тип

eitherCommute :: Either a b -> Either b a
eitherCommute (Left x) = (Right x)
eitherCommute (Right x) = (Left x)

-- # 9.2 Аналогично

eitherAssoc :: Either a (Either b c) -> Either (Either a b) c
eitherAssoc = undefined

{- 10. В Haskell определена также конструкция case of, которая позволяет делать паттерн-матчинг
внутри реализации функции.
Примеры:
caseOfListLength :: [a] -> Int
caseOfListLength xs = case xs of
    [] -> 0
    (x:xs) -> 1 + caseOfListLength xs
booleanImplication :: Bool -> Bool -> Bool
booleanImplication x y = case (not x || y) of
    True -> True
    False -> False
Реализовать через case of следующие функции -}

-- # 10.1

listSum :: Num a => [a] -> a
-- listSum [] = 0
-- listSum (x:xs) = x + listSum xs

listSum xs = case xs of
    [] -> 0
    (x:xs) -> x + listSum xs

-- # 10.2

filterList :: (a -> Bool) -> [a] -> [a]
-- filterList predicate [] = []
-- filterList predicate (x:xs) =
--     if predicate x then (x : filterList predicate xs) else filterList predicate xs

filterList predicate (x:xs) = case (predicate x, xs) of
    (_, []) -> []
    (True, xs) -> (x : filterList predicate xs)
    (_, _) -> filterList predicate xs

-- main = print(filterList (<2) [1..10])

-- # 10.3

safeHead :: [a] -> Maybe a
-- safeHead [] = Nothing
-- safeHead (x:xs) = Just x

safeHead xs = case xs of
    [] -> Nothing
    (x:xs) -> Just x

-- # 10.4

distributivity :: (a, Either b c) -> Either (a, b) (a, c)
-- distributivity (x, Left y) = Left (x, y)
-- distributivity (x, Right y) = Right (x, y)

distributivity (x, choice) = case choice of
    (Left y) -> Left (x, y)
    (Right y) -> Right (x, y)
