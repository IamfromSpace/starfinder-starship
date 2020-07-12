-- THIS MODULE IS COPIED FROM REACTION TIME PROJECT
-- MAKE CHANGES THERE OR MAKE THIS A DAMN LIBRARY


module Utils exposing (configStyle)


configStyle :
    { init : flags -> ( config, ( model, Cmd msg ) )
    , view : model -> html
    , update : config -> msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    }
    ->
        { init : flags -> ( ( config, model ), Cmd msg )
        , view : ( config, model ) -> html
        , update : msg -> ( config, model ) -> ( ( config, model ), Cmd msg )
        , subscriptions : ( config, model ) -> Sub msg
        }
configStyle { init, view, update, subscriptions } =
    { init =
        \flags ->
            let
                ( cfg, ( m, cmds ) ) =
                    init flags
            in
            ( ( cfg, m ), cmds )
    , view = view << Tuple.second
    , update =
        \msg ( cfg, m ) ->
            let
                ( m2, cmds ) =
                    update cfg msg m
            in
            ( ( cfg, m2 ), cmds )
    , subscriptions =
        \( cfg, m ) -> subscriptions m
    }
