{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Parser241.ProductionRule.Interface where

import Parser241.Parser.ProductionRule.Internal
import Parser241.Parser.ProductionRule.Internal.Maker as M
import Parser241.Parser.ProductionRule.Internal.Manager as M

class Produce l r a where
   (-->) :: FromMaker m => l -> r -> m a ()

instance Produce (Symbol a) (Symbol a) a where
   (-->) = (>>>)

instance Produce (Symbol a) a a where
   (-->) = (--->)

instance Produce a a a where
   (-->) = (M.-->)


class Altern r a where
   (|>) :: FromMaker m => Maker a -> r -> m a ()

instance Altern (Symbol a) a where
   (|>) = (|/)

instance Altern a a where
   (|>) = (M.|>)

