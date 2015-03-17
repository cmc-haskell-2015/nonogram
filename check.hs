class Functor f where
	fmap ::  (a -> b) -> f a -> f b

instance Functor Maybe where
	fmap _ Nothing = Nothing
	fmap f (Just x) = Just (f x)

 