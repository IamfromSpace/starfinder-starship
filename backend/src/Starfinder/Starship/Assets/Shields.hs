{-# LANGUAGE OverloadedStrings #-}

module Starfinder.Starship.Assets.Shields where

import qualified Data.KeyedSet as KS
import Data.Text (Text)
import Starfinder.Starship.Build (Shields(..))

shields :: KS.KeyedSet Text Shields
shields =
    KS.fromList
        name
        [ Shields
          { name = "Light Shields 60"
          , shieldPoints = 60
          , regenPerMinute = 2
          , powerDraw = 20
          , buildPoints = 10
          }
        ]
