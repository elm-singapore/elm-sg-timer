module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)


type alias Model =
    { numberOfParticipants : Int
    , totalTime : Int
    , page : Page
    }


model : Model
model =
    Model 0 300 Setup


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


update : Msg -> Model -> Model
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
                { model | numberOfParticipants = finalNum }

        MsgReady ->
            { model | page = Ready }

        MsgReset ->
            { model | page = Setup }

        Start ->
            { model | page = Running }

        _ ->
            model


viewHeader =
    div []
        [ h1 [] [ text "Elm SG" ]
        , h2 [] [ text "TIMER" ]
        ]


viewInputs =
    div []
        [ input [ placeholder "# of participants", onInput UpdateParticipants ] []
        , br [] []
        , input [ placeholder "Total time" ] []
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


viewCountdown : Int -> Html Msg
viewCountdown numParticipants =
    div []
        [ p [] []
        , p [] [ text ((toString numParticipants) ++ " participants left") ]
        ]


view : Model -> Html Msg
view model =
    let
        body =
            case model.page of
                Setup ->
                    [ viewInputs, viewSetupButtons ]

                Ready ->
                    [ viewCalculatedValues (model.totalTime // model.numberOfParticipants)
                    , viewReadyButtons
                    ]

                Running ->
                    [ viewCountdown model.numberOfParticipants ]
    in
        div []
            (viewHeader :: body)


main =
    beginnerProgram
        { model = model
        , view = view
        , update = update
        }
