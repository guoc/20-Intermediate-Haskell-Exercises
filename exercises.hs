class Fluffy f where
  furry :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy [] where
  furry _ [] = []
  furry f (x : xs) = f x : (furry f xs)
-- Or
--  furry = map

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Maybe where
  furry _ Nothing = Nothing
  furry f (Just x) = Just $ f x

-- Exercise 3
-- Relative Difficulty: 5
instance Fluffy ((->) t) where
  furry f x = f . x

newtype EitherLeft b a = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)

-- Exercise 4
-- Relative Difficulty: 5
instance Fluffy (EitherLeft t) where
  furry f (EitherLeft (Left x)) = EitherLeft (Left $ f x)
  furry _ (EitherLeft (Right x)) = EitherLeft (Right x)

-- Exercise 5
-- Relative Difficulty: 5
instance Fluffy (EitherRight t) where
  furry f (EitherRight x) = EitherRight $ fmap f x

class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a
  -- Exercise 6
  -- Relative Difficulty: 3
  -- (use banana and/or unicorn)
  furry' :: (a -> b) -> m a -> m b
  furry' f = banana (unicorn . f)

-- Exercise 7
-- Relative Difficulty: 2
instance Misty [] where
  banana = concatMap
  unicorn x = [x]

-- Exercise 8
-- Relative Difficulty: 2
instance Misty Maybe where
  banana k Nothing = Nothing
  banana k (Just x) = k x
  unicorn = Just

-- Exercise 9
-- Relative Difficulty: 6
instance Misty ((->) t) where
  banana k g = \t -> k (g t) t
  unicorn = const

-- Exercise 10
-- Relative Difficulty: 6
instance Misty (EitherLeft t) where
  banana _ (EitherLeft (Right r)) = EitherLeft (Right r)
  banana k (EitherLeft (Left l)) = k l
  unicorn = EitherLeft . Left

-- Exercise 11
-- Relative Difficulty: 6
instance Misty (EitherRight t) where
  banana _ (EitherRight (Left l)) = EitherRight (Left l)
  banana k (EitherRight (Right r)) = k r
  unicorn = EitherRight . Right

-- Exercise 12
-- Relative Difficulty: 3
jellybean :: (Misty m) => m (m a) -> m a
jellybean = banana id

-- Exercise 13
-- Relative Difficulty: 6
apple :: (Misty m) => m a -> m (a -> b) -> m b
apple mx mk = banana (\x -> banana (\k -> unicorn $ k x) mk) mx
-- Or
--apple = banana . (flip furry')
-- Excerpted from https://github.com/rafalio/20intermediate/blob/master/intermediate_exercises.hs

-- Exercise 14
-- Relative Difficulty: 6
moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy xs k = foldr (\x acc -> banana (\y -> banana (\ys -> unicorn (y:ys)) acc) (k x)) (unicorn []) xs

-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use moppy)
sausage :: (Misty m) => [m a] -> m [a]
sausage mx = moppy mx id

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2 g mx my = apple my $ furry' g mx
-- Or
--banana2 g mx my = apple my $ apple mx $ unicorn g

-- Exercise 17
-- Relative Difficulty: 6
-- (bonus: use apple + banana2)
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3 g mx1 mx2 mx3 = apple mx3 $ banana2 g mx1 mx2

-- Exercise 18
-- Relative Difficulty: 6
-- (bonus: use apple + banana3)
banana4 :: (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 g mx1 mx2 mx3 mx4 = apple mx4 $ banana3 g mx1 mx2 mx3

newtype State s a = State {
  state :: (s -> (s, a))
}

-- Exercise 19
-- Relative Difficulty: 9
instance Fluffy (State s) where
  furry g (State pr) = State $ \s -> fmap g (pr s)

-- Exercise 20
-- Relative Difficulty: 10
instance Misty (State s) where
  banana k (State pr) = State $ \s -> let (s', x) = pr s in state (k x) s'
  unicorn x = State $ \s -> (s, x)
