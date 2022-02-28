{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Generics1 where

import Data.Text hiding (empty)
import Lib

class EmptyConstructor constructorF x where
  empty :: constructorF -> x

instance EmptyConstructor f x => EmptyConstructor (a -> f) x where
  empty f = empty $ f undefined

instance EmptyConstructor a a where
  empty = id

-- what you can do things like

prefix :: Cmd -> Text
prefix = \case
  Plus _ -> "plus"
  Minus _ -> "minus"
  Result -> "result"

description :: Cmd -> Text
description = \case
  Plus _ -> "Adds two numbers"
  Minus _ -> "Substracts two numbers"
  Result -> "Shows the current result"

-- the important bit is that those functions must never touch
