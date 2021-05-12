module WeaponDescription exposing (view)

import Link exposing (Link(..))
import LinkAndTogglable as LT
import Switch exposing (Switch(..))
import Weapon exposing (Range(..), Type(..), Weapon)


view : LT.LinkAndTogglable Weapon -> String
view tWeapon =
    let
        weapon =
            LT.extract tWeapon

        { link, switch } =
            LT.meta tWeapon

        mDamageText =
            weapon.damage
                |> Maybe.map
                    (\( num, die ) ->
                        String.fromInt
                            (if link == Linked then
                                num * 2

                             else
                                num
                            )
                            ++ "d"
                            ++ String.fromInt die
                            ++ (if link == Linked then
                                    "(linked)"

                                else
                                    ""
                               )
                    )

        rangeText =
            (case weapon.range of
                Short ->
                    "5"

                Medium ->
                    "10"

                Long ->
                    "20"
            )
                ++ " hex"

        typeText =
            case weapon.weaponType of
                DirectFire ->
                    "vsAC"

                Tracking speed ->
                    "vsTL (speed " ++ String.fromInt speed ++ ")"

        mSpecialText =
            if List.length weapon.specialProperties == 0 then
                Nothing

            else
                List.map Weapon.weaponPropertyToString weapon.specialProperties
                    |> List.intersperse ", "
                    |> String.concat
                    |> (\x -> "[ " ++ x ++ " ]")
                    |> Just
    in
    [ Just
        (weapon.name
            ++ (if switch == On then
                    ""

                else
                    " (not powered)"
               )
        )
    , Just rangeText
    , mDamageText
    , Just typeText
    , mSpecialText
    ]
        |> List.filterMap identity
        |> List.intersperse " - "
        |> String.concat
