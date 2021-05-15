module BattlePhase exposing (BattlePhase(..), nextPhase, toString)

-- So this is a bit weird, but CombatPhase is a _true_ phase as described by
-- the rules-as-written.  A BattlePhase is the same, but expliticly adds the
-- implicit phase of assigning crew members.

import CombatPhase exposing (CombatPhase(..))


type BattlePhase
    = Assign
    | CP CombatPhase


nextPhase : BattlePhase -> BattlePhase
nextPhase phase =
    case phase of
        Assign ->
            CP Engineering

        CP Engineering ->
            CP Piloting

        CP Piloting ->
            CP Gunnery

        CP Gunnery ->
            Assign


toString : BattlePhase -> String
toString phase =
    case phase of
        Assign ->
            "Assignments"

        CP Engineering ->
            "Engineering"

        CP Piloting ->
            "Piloting"

        CP Gunnery ->
            "Gunnery"
