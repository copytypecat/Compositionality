{-# LANGUAGE  LambdaCase, 
              InstanceSigs, 
              TypeOperators, 
              FunctionalDependencies #-}

module Compositionality where 

import Data.Bifunctor
import Data.Void

-- infix shorthands

infixr 8 //
(//) :: (a -> b) -> (b -> c) -> (a -> c)
(//) = flip (.)

infixr 9 #
(#) :: Bifunctor o => (a -> b) -> (c -> d) -> a `o` c -> b `o` d
(#) = bimap

-- building up monoidal categories 

class Bifunctor o => Assoc o where 
     asc :: (a `o` b) `o` c -> a `o` (b `o` c)
     csa :: a `o` (b `o` c) -> (a `o` b) `o` c

class Assoc o => Tensor o t | o -> t, t -> o where
     lun :: t `o` a -> a
     nul :: a -> t `o` a 

     run :: a `o` t -> a 
     nur :: a -> a `o` t

class Assoc o => Comm o where
     swap :: a `o` b -> b `o` a 

class Comm o => Semicartesian o where 
     copy :: a -> a `o` a

     fork :: (a -> x) -> (a -> y) -> a -> x `o` y 
     fork f g = copy // f # g

     (/\) :: (a -> x) -> (a -> y) -> a -> x `o` y 
     (/\) = fork 

class (Semicartesian o, Tensor o t) => Cartesian o t where
     discard :: a -> t

     projl :: x `o` y -> x  
     projl = id # discard // run 

     projr :: x `o` y -> y
     projr = discard # id // lun 

     unfork :: (a -> x `o` y) -> (a -> x, a -> y)
     unfork h = (h // projl, h // projr)

class Comm o => Semicocartesian o where 
     merge :: a `o` a -> a
     
     join :: (x -> a) -> (y -> a) -> (x `o` y -> a)
     join f g = f # g // merge

     (\/) :: (x -> a) -> (y -> a) -> (x `o` y -> a)
     (\/) = join

class (Semicocartesian o, Tensor o t) => Cocartesian o t where
     create :: t -> a

     incll :: x -> x `o` y  
     incll = nur // id # create

     inclr :: y -> x `o` y
     inclr = nul // create # id

     unjoin :: (x `o` y -> a) -> (x -> a, y -> a)
     unjoin h = (incll // h, inclr // h) 

-- instances for (Hask, (,), ())

instance Assoc (,) where 
     asc :: ((a, b), c) -> (a, (b, c))
     asc    ((a, b), c) =  (a, (b, c))

     csa :: (a, (b, c)) -> ((a, b), c)
     csa    (a, (b, c)) =  ((a, b), c)

instance Tensor (,) () where 
     lun :: ((), a) -> a 
     lun    ((), a) =  a 

     nul :: a -> ((), a)
     nul    a =  ((), a) 

     run :: (a, ()) -> a 
     run    (a,  _) =  a 

     nur :: a -> (a, ())
     nur    a =  (a, ())

instance Comm (,) where 
     swap :: (a, b) -> (b, a)
     swap    (a, b) =  (b, a)

instance Semicartesian (,) where 
     copy :: a -> (a, a)
     copy    a =  (a, a)

instance Cartesian (,) () where 
     discard :: a -> ()
     discard    _ =  ()

-- instances for (Hask, Either, Void)

instance Assoc Either where 
  asc :: (a `Either` b) `Either` c -> a `Either` (b `Either` c)
  asc = \case Left  (Left  a)      -> Left a 
              Left  (Right b)      -> Right (Left  b)
              Right c              -> Right (Right c)

  csa :: a `Either` (b `Either` c) -> (a `Either` b) `Either` c
  csa = \case Left  a              -> Left  (Left a)
              Right (Left  b)      -> Left  (Right  b)
              Right (Right c)      -> Right c

instance Tensor Either Void where 
     lun :: Void `Either` a -> a 
     lun = \case Left  x    -> absurd x
                 Right a    -> a 

     nul :: a -> Void `Either` a
     nul = Right 

     run :: a `Either` Void -> a 
     run = \case Left a     -> a
                 Right x    -> absurd x 

     nur :: a -> a `Either` Void
     nur = Left 

instance Comm Either where 
     swap :: a `Either` b -> b `Either` a 
     swap = \case Left  a -> Right a 
                  Right b -> Left  b

instance Semicocartesian Either where 
     merge :: a `Either` a -> a 
     merge = \case Left  a -> a 
                   Right a -> a 

instance Cocartesian Either Void where 
     create :: Void -> a 
     create = absurd 


-- example I 

flow :: Assoc o => (a       -> m `o` j) -- f
                -> (j `o` b -> q `o` z) -- g 
                -> (m `o` q ->       y) -- h
                -> (a `o` b -> y `o` z)

                    -- a `o` b
flow f g h = f # id -- (m `o` j) `o` b
          // asc    -- m `o` (j `o` b)
          // id # g -- m `o` (q `o` z)
          // csa    -- (m `o` q) `o` z
          // h # id -- y `o` z


f1 :: [a] -> ([a], Int)
f1 xs = (reverse xs, length xs)

g1 :: (Int, Int) -> (Bool, Int)
g1 (n, m) = (n > m, n + m)

h1 :: ([a], Bool) -> [a]
h1 = \case ([]  , _    ) -> []
           (_:xs, True ) -> xs 
           (x:_ , False) -> [x]


f2 :: [a] -> [a] `Either` Int 
f2 = \case []     -> Right 7 
           (_:xs) -> Left $ reverse xs   

g2 :: Int `Either` Int -> Bool `Either` Int 
g2 = \case Left  n -> Right $ n * n 
           Right n -> Left  $ n `mod` 2 == 0

h2 :: [a] `Either` Bool -> [a]
h2 = \case Left (x:(_:xs)) -> x:(x:xs)
           _               -> []
