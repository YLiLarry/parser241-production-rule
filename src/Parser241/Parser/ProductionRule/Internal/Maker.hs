{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Parser241.Parser.ProductionRule.Internal.Maker where

import Parser241.Parser.ProductionRule.Internal
import Data.Set (Set, singleton)
import Control.Monad.Reader (Reader(..), runReader, MonadReader(..), reader)


newtype Maker' a x = Maker {
      unMaker :: (Symbol a, [[Symbol a]])
   } deriving (Functor)

type Maker a = Maker' a ()


maker :: (Symbol a, [[Symbol a]]) -> Maker a
maker = Maker


class Produce l r a where
   (-->) :: FromMaker m => l -> r -> m a ()

instance Produce (Symbol a) a a where
   lhs --> rhs = fromMaker $ maker (lhs, [[UD rhs]])

instance Produce a (Symbol a) a where
   lhs --> rhs = fromMaker $ maker (NT lhs, [[rhs]])

instance Produce a a a where
   lhs --> rhs = fromMaker $ maker (NT lhs, [[UD rhs]])

instance Produce (Symbol a) (Symbol a) a where
   lhs --> rhs = fromMaker $ maker (lhs, [[rhs]])


class Altern r a where
   (|>) :: FromMaker m => Maker a -> r -> m a ()

instance Altern a a where
   m |> a = fromMaker $ maker (lhs, [UD a]:rhs)
      where
         (lhs,rhs) = unMaker m

instance Altern (Symbol a) a where
   m |> Null = fromMaker $ maker (lhs, [Null]:rhs)
      where
         (lhs,rhs) = unMaker m


-- | Use `&` to concatenate two symbols.
--
-- > table :: [Rule MySym]
-- > table = productRules $ do
-- >    Start >>> Null & C'
-- >           |> ...
--
(&) :: FromMaker m => Maker a -> a -> m a ()
a & b = fromMaker $ maker (lhs, (UD b:r):rhs)
   where
      (lhs,r:rhs) = unMaker a


class FromMaker m where
   fromMaker :: Maker a -> m a ()

instance FromMaker Maker' where
   fromMaker = id
