{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Map.Strict.Merge where

import           Control.Lens
import           Data.Aeson
import qualified Data.Map.Strict               as M
import           Data.Monoid
import           GHC.Generics
import           Prelude                           hiding ( round )

newtype Merge k v = Merge { getMerge :: M.Map k v }
  deriving stock (Show, Eq, Generic)
  deriving newtype (ToJSON, FromJSON)

type instance Index (Merge k v) = k
type instance IxValue (Merge k v) = v

instance Ord k => Ixed (Merge k v) where
  ix k f m = Merge <$> ix k f (getMerge m)
  {-# INLINE ix #-}

instance Ord k => At (Merge k v) where
  at k f m = Merge <$> at k f (getMerge m)
  {-# INLINE at #-}

instance (Ord k, Num v) => Semigroup (Merge k v) where
  Merge xs <> Merge ys = Merge $ M.unionWith (+) xs ys

instance (Ord k, Num v) => Monoid (Merge k v) where
  mempty = Merge (M.empty)
  mappend = (<>)
