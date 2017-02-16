module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)


type alias Model =
    { numberOfParticipants : Int
    , totalTime : Int
    , page : Page
    }


type Page
    = Setup
    | Ready
    | Running


viewHeader =
    div []
        [ h1 [] [ text "Elm SG" ]
        , h2 [] [ text "TIMER" ]
        ]


viewInputs =
    div []
        [ input [ placeholder "# of participants" ] []
        , br [] []
        , input [ placeholder "Total time" ] []
        ]


viewButtons =
    div []
        [ button [] [ text "RESET" ]
        , button [] [ text "READY" ]
        ]


viewCalculatedValues participantSeconds =
    div []
        [ p [] [ participantSeconds |> toString |> (++) "seconds " |> text ]
        , p [] [ text "per participant" ]
        ]


viewCountdown : Int -> Html msg
viewCountdown numParticipants =
    div []
        [ p [] []
        , p [] [ text ((toString numParticipants) ++ " participants left") ]
        ]


view : Model -> Html msg
view model =
    let
        body =
            case model.page of
                Setup ->
                    viewInputs

                Ready ->
                    viewCalculatedValues (model.totalTime // model.numberOfParticipants)

                Running ->
                    viewCountdown model.numberOfParticipants
    in
        div []
            [ viewHeader
            , body
            , viewButtons
            ]


main =
    Model 0 0 Running
        |> view
