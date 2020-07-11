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
          { name = "Basic Shields 10"
          , shieldPoints = 10
          , regenPerMinute = 1
          , powerDraw = 5
          , buildPoints = 2
          }
        , Shields
          { name = "Basic Shields 20"
          , shieldPoints = 20
          , regenPerMinute = 1
          , powerDraw = 10
          , buildPoints = 3
          }
        , Shields
          { name = "Basic Shields 30"
          , shieldPoints = 30
          , regenPerMinute = 1
          , powerDraw = 15
          , buildPoints = 4
          }
        , Shields
          { name = "Basic Shields 40"
          , shieldPoints = 40
          , regenPerMinute = 1
          , powerDraw = 15
          , buildPoints = 5
          }
        , Shields
          { name = "Light Shields 50"
          , shieldPoints = 50
          , regenPerMinute = 2
          , powerDraw = 20
          , buildPoints = 6
          }
        , Shields
          { name = "Light Shields 60"
          , shieldPoints = 60
          , regenPerMinute = 2
          , powerDraw = 20
          , buildPoints = 8
          }
        , Shields
          { name = "Light Shields 70"
          , shieldPoints = 70
          , regenPerMinute = 2
          , powerDraw = 25
          , buildPoints = 10
          }
        , Shields
          { name = "Light Shields 80"
          , shieldPoints = 80
          , regenPerMinute = 2
          , powerDraw = 30
          , buildPoints = 12
          }
        , Shields
          { name = "Medium Shields 90"
          , shieldPoints = 90
          , regenPerMinute = 4
          , powerDraw = 30
          , buildPoints = 13
          }
        , Shields
          { name = "Medium Shields 100"
          , shieldPoints = 100
          , regenPerMinute = 4
          , powerDraw = 30
          , buildPoints = 15
          }
        , Shields
          { name = "Medium Shields 120"
          , shieldPoints = 120
          , regenPerMinute = 4
          , powerDraw = 35
          , buildPoints = 17
          }
        , Shields
          { name = "Medium Shields 140"
          , shieldPoints = 140
          , regenPerMinute = 8
          , powerDraw = 40
          , buildPoints = 18
          }
        , Shields
          { name = "Medium Shields 160"
          , shieldPoints = 160
          , regenPerMinute = 8
          , powerDraw = 45
          , buildPoints = 20
          }
        , Shields
          { name = "Medium Shields 200"
          , shieldPoints = 200
          , regenPerMinute = 8
          , powerDraw = 50
          , buildPoints = 22
          }
        , Shields
          { name = "Heavy Shields 240"
          , shieldPoints = 240
          , regenPerMinute = 16
          , powerDraw = 55
          , buildPoints = 23
          }
        , Shields
          { name = "Heavy Shields 280"
          , shieldPoints = 280
          , regenPerMinute = 16
          , powerDraw = 60
          , buildPoints = 25
          }
        , Shields
          { name = "Heavy Shields 320"
          , shieldPoints = 320
          , regenPerMinute = 16
          , powerDraw = 70
          , buildPoints = 27
          }
        , Shields
          { name = "Heavy Shields 360"
          , shieldPoints = 360
          , regenPerMinute = 32
          , powerDraw = 80
          , buildPoints = 28
          }
        , Shields
          { name = "Heavy Shields 420"
          , shieldPoints = 420
          , regenPerMinute = 32
          , powerDraw = 90
          , buildPoints = 30
          }
        , Shields
          { name = "Heavy Shields 480"
          , shieldPoints = 480
          , regenPerMinute = 32
          , powerDraw = 110
          , buildPoints = 32
          }
        , Shields
          { name = "Superior Shields 540"
          , shieldPoints = 540
          , regenPerMinute = 64
          , powerDraw = 130
          , buildPoints = 35
          }
        , Shields
          { name = "Superior Shields 600"
          , shieldPoints = 600
          , regenPerMinute = 64
          , powerDraw = 160
          , buildPoints = 40
          }
        ]
