{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

{-|
Module      : Authorizer
Description : Operations for saving and retreiving Starship Builds
Copyright   : (c) Nathan Fairhurst, 2020
License     : MIT
Maintainer  : nathan.p3pictures@gmail.com
Stability   : experimental

This is a basic Sem monad for authorizing actions of arbitrary types.
Provided alwaysAuthed interpretation never checks anything.
-}
module Authorizer where

import Polysemy (Sem, interpret, makeSem)

data Authorizer action m b where
    CheckActionAuthorized :: action -> Authorizer action m ()

makeSem ''Authorizer

alwaysAuthed :: Sem ((Authorizer a) ': r) b -> Sem r b
alwaysAuthed =
    interpret $ \case
        CheckActionAuthorized _ -> return ()
