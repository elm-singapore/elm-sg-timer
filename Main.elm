module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Time exposing (Time, every, second)


type alias Model =
    { numberOfParticipants : Int
    , totalTime : Int
    , page : Page
    }


model : Model
model =
    Model 0 0 Setup


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.page == Running then
        every second Tick
    else
        Sub.none


type Page
    = Setup
    | Ready
    | Running


type Msg
    = UpdateParticipants String
    | UpdateTotalTime String
    | MsgReset
    | MsgReady
    | Start
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateParticipants num ->
            let
                finalNum =
                    case String.toInt num of
                        Ok val ->
                            val

                        Err _ ->
                            0
            in
                ( { model | numberOfParticipants = finalNum }, Cmd.none )

        MsgReady ->
            ( { model | page = Ready }, Cmd.none )

        MsgReset ->
            ( { model | page = Setup }, Cmd.none )

        Start ->
            ( { model | page = Running }, Cmd.none )

        Tick _ ->
            ( { model | totalTime = model.totalTime - 1 }, Cmd.none )

        UpdateTotalTime time ->
            let
                finalNum =
                    case String.toInt time of
                        Ok val ->
                            val

                        Err _ ->
                            0
            in
                ( { model | totalTime = finalNum }, Cmd.none )


viewHeader =
    div []
        [ h1 [] [ text "Elm Singapore" ]
        , h2 [] [ text "TIMER" ]
        ]


viewInputs =
    div []
        [ input [ placeholder "# of participants", onInput UpdateParticipants ] []
        , br [] []
        , input [ placeholder "Total time", onInput UpdateTotalTime ] []
        ]


viewReadyButtons =
    div []
        [ button [ onClick MsgReset ] [ text "RESET" ]
        , button [ onClick Start ] [ text "START" ]
        ]


viewSetupButtons =
    div []
        [ button [] [ text "RESET" ]
        , button [ onClick MsgReady ] [ text "READY" ]
        ]


viewCalculatedValues participantSeconds =
    div []
        [ p [] [ (participantSeconds |> toString |> (++)) " seconds " |> text ]
        , p [] [ text "per participant" ]
        ]


viewCountdown : ( Int, Int ) -> Html Msg
viewCountdown ( remainingTime, remainingParticipants ) =
    div []
        [ p [] []
        , p [] [ text ((toString remainingParticipants) ++ " participants left") ]
        , p [] [ text ((toString remainingTime) ++ " time left") ]
        ]


view : Model -> Html Msg
view model =
    let
        body =
            case model.page of
                Setup ->
                    [ viewInputs, viewSetupButtons ]

                Ready ->
                    let
                        _ =
                            ( Debug.log "totalTime" (model.totalTime)
                            , Debug.log "model.numberOfParticipants" (model.numberOfParticipants)
                            , Debug.log "calculating time per participant" (model.totalTime // model.numberOfParticipants)
                            )
                    in
                        [ viewCalculatedValues
                            (model.totalTime // model.numberOfParticipants)
                        , viewReadyButtons
                        ]

                Running ->
                    [ viewCountdown ( model.totalTime, model.numberOfParticipants ) ]
    in
        div []
            (viewHeader :: body)


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
