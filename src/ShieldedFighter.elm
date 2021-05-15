module ShieldedFighter exposing (view, view2)

import Arc
import Color exposing (grey)
import ColorUtils
import CounterArc
import Fighter
import Shielded
import Starship exposing (Starship)
import Status exposing (Status)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE
import Switch exposing (Switch(..))
import Togglable exposing (extract, meta)


getDamagePercent starship status =
    let
        hp =
            Starship.getMaxHitPoints starship
    in
    toFloat (hp - status.damage) / toFloat hp


view : Starship -> Status -> (Arc.AnArc -> a) -> Arc.Arc (Maybe a) -> Arc.Arc (Maybe a) -> Float -> Svg a
view starship status =
    view2 starship status Nothing


view2 : Starship -> Status -> Maybe a -> (Arc.AnArc -> a) -> Arc.Arc (Maybe a) -> Arc.Arc (Maybe a) -> Float -> Svg a
view2 starship status onClick arcOnClick onPlus onMinus size =
    let
        hp =
            Starship.getMaxHitPoints starship

        fontSize =
            size / 12.5

        damagePercent =
            getDamagePercent starship status

        shieldDamagePercents =
            Arc.map
                (\points ->
                    if damagePercent > 0 && meta starship.shields == On then
                        toFloat points
                            / (toFloat (extract starship.shields).shieldPoints / 4)

                    else
                        0
                )
                status.shields
    in
    Svg.g
        (Maybe.withDefault [] <| Maybe.map (\m -> [ SE.onClick m ]) onClick)
        [ Shielded.view
            (\size_ color ->
                Fighter.view
                    { size = size_
                    , color = color
                    }
            )
            { size = size
            , arcColors = Arc.map ColorUtils.colorTransition shieldDamagePercents
            , arcOnClick = arcOnClick
            , shielded = ColorUtils.colorTransition damagePercent
            }
        , CounterArc.view
            { radius = size * 49 / 100
            , size = fontSize
            , offset = ( size / 2, size / 2 )
            , color = Color.black
            , backgroundColor = grey
            , counts = status.shields
            , onPlus = onPlus
            , onMinus = onMinus
            }
        , Svg.text_
            [ SA.fontFamily "mono"
            , SA.textAnchor "middle"
            , SA.alignmentBaseline "middle"
            , SA.fontSize <| String.fromFloat fontSize
            , SA.fill "black"
            , SA.transform <| "translate(" ++ String.fromFloat (size / 2) ++ ", " ++ String.fromFloat (size / 2) ++ ")"
            ]
            [ Svg.text (String.fromInt (max 0 (hp - status.damage)) ++ "/" ++ String.fromInt hp) ]
        ]
