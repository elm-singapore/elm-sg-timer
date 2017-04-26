port module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Time exposing (Time, every, second)
import Task


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


port playSound : String -> Cmd msg


port soundEnded : (String -> msg) -> Sub msg



-- MODEL ###################################################


type alias Model =
    { page : Page
    , totalNbOfParticipants : Int
    , remainingNbOfParticipants : Int
    , totalTime : Time
    , startTime : Maybe Time
    , remainingTime : Time
    }


type Page
    = PageSetup
    | PageReady
    | PageRunning


init : ( Model, Cmd Msg )
init =
    ( Model PageSetup 0 0 0 Nothing 0
    , Cmd.none
    )



-- UPDATE ##################################################


type Msg
    = Reset
    | SetupMsg SetupMsg
    | ReadyMsg ReadyMsg
    | RunningMsg RunningMsg
    | EndSound String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.startTime ) of
        ( Reset, _ ) ->
            init

        ( SetupMsg setupMsg, _ ) ->
            updateSetup setupMsg model

        ( ReadyMsg readyMsg, _ ) ->
            updateReady readyMsg model

        ( RunningMsg runningMsg, Just startTime ) ->
            updateRunning startTime runningMsg model

        ( EndSound endMsg, _ ) ->
            let
                _ =
                    Debug.log "Timer ended" endMsg
            in
                ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- UPDATE SETUP


type SetupMsg
    = ChangeNumberOfParticipants (Result String Int)
    | ChangeTotalTime (Result String Time)
    | SetupReady


updateSetup : SetupMsg -> Model -> ( Model, Cmd Msg )
updateSetup msg model =
    case msg of
        ChangeNumberOfParticipants (Ok n) ->
            ( { model | totalNbOfParticipants = n }
            , Cmd.none
            )

        ChangeTotalTime (Ok time) ->
            ( { model | totalTime = time }, Cmd.none )

        SetupReady ->
            ( { model | page = PageReady }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- UPDATE READY


type ReadyMsg
    = Start
    | StartedAt Time


updateReady : ReadyMsg -> Model -> ( Model, Cmd Msg )
updateReady msg model =
    case msg of
        Start ->
            ( { model | remainingNbOfParticipants = model.totalNbOfParticipants }
            , Task.perform (ReadyMsg << StartedAt) Time.now
            )

        StartedAt time ->
            ( { model | page = PageRunning, startTime = Just time }
            , Cmd.none
            )



-- UPDATE RUNNING


type RunningMsg
    = Tick Time


updateRunning : Time -> RunningMsg -> Model -> ( Model, Cmd Msg )
updateRunning startTime msg model =
    case msg of
        Tick time ->
            let
                ( timePerParticipant, remainingTime ) =
                    ( model.totalTime / toFloat model.totalNbOfParticipants
                    , model.totalTime - (time - startTime)
                    )

                remainingNbOfParticipants =
                    ceiling (remainingTime / timePerParticipant)
                        |> clamp 0 model.totalNbOfParticipants
            in
                ( { model
                    | remainingNbOfParticipants = remainingNbOfParticipants
                    , remainingTime = remainingTime
                  }
                , if remainingNbOfParticipants == 0 then
                    commandWhenRunningEnd
                  else if remainingNbOfParticipants < model.remainingNbOfParticipants then
                    commandWhenParticipantChange
                  else
                    Cmd.none
                )


{-| Replace this by whatever we want to do when the participant changes
-}
commandWhenParticipantChange : Cmd Msg
commandWhenParticipantChange =
    playSound "bell_ring"


{-| Replace this by whatever we want to do when the timer end
-}
commandWhenRunningEnd : Cmd Msg
commandWhenRunningEnd =
    Cmd.none



-- SUBSCRIPTIONS ###########################################


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        tSub =
            if model.page == PageRunning && model.remainingNbOfParticipants > 0 then
                every (second / 10) (RunningMsg << Tick)
            else
                Sub.none
    in
        Sub.batch [ tSub, soundEnded EndSound ]



-- VIEW ####################################################


view : Model -> Html Msg
view model =
    case model.page of
        PageSetup ->
            viewPage (pageSetupContent model)

        PageReady ->
            viewPage (pageReadyContent model)

        PageRunning ->
            viewPage (pageRunningContent model)


viewPage : Html Msg -> Html Msg
viewPage pageContent =
    div [ id "timer-component" ]
        [ viewHeader
        , pageContent
        ]


viewHeader : Html Msg
viewHeader =
    div []
        [ h1 [] [ text "Elm Singapore" ]
        , h2 [] [ text "- TIMER -" ]
        ]



-- VIEW SETUP


pageSetupContent : Model -> Html Msg
pageSetupContent model =
    div []
        [ p []
            [ label [ for "nb-participants" ] [ text "Number of participants" ]
            , input
                [ id "nb-participants"
                , type_ "number"
                , onInput (SetupMsg << ChangeNumberOfParticipants << String.toInt)
                ]
                []
            ]
        , p []
            [ label [ for "total-time" ] [ text "Total time" ]
            , input
                [ id "total-time"
                , type_ "number"
                , onInput (String.toFloat >> Result.map ((*) second) >> ChangeTotalTime >> SetupMsg)
                ]
                []
            ]
        , p [] [ button [ onClick (SetupMsg SetupReady) ] [ text "Ready" ] ]
        ]



-- VIEW READY


pageReadyContent : Model -> Html Msg
pageReadyContent model =
    let
        timePerParticipant =
            (model.totalTime / toFloat model.totalNbOfParticipants)
                |> Time.inSeconds
                |> round
    in
        div []
            [ p [] [ text <| toString timePerParticipant ++ " seconds per participant" ]
            , p []
                [ button [ onClick Reset ] [ text "Reset" ]
                , button [ onClick (ReadyMsg Start) ] [ text "Start" ]
                ]
            ]



-- VIEW RUNNING


pageRunningContent : Model -> Html Msg
pageRunningContent model =
    let
        timePerParticipant =
            model.totalTime / toFloat model.totalNbOfParticipants

        timeForOtherParticipants =
            timePerParticipant * toFloat (model.remainingNbOfParticipants - 1)

        participantRemainingTime =
            model.remainingTime - timeForOtherParticipants
    in
        if model.remainingNbOfParticipants == 0 then
            div []
                [ p [] [ text "Time's up" ]
                , p [] [ button [ onClick Reset ] [ text "Reset" ] ]
                ]
        else
            div []
                [ p [] [ text (formatTime participantRemainingTime) ]
                , p [] [ text (toString model.remainingNbOfParticipants ++ " participants left") ]
                , p [] [ button [ onClick Reset ] [ text "Reset" ] ]
                ]


formatTime : Time -> String
formatTime time =
    let
        minutes =
            floor <| Time.inMinutes time

        seconds =
            floor <| Time.inSeconds (time - (toFloat minutes) * Time.minute)
    in
        format2 minutes ++ " : " ++ format2 seconds


format2 : Int -> String
format2 n =
    if n >= 10 then
        toString n
    else
        "0" ++ toString n
