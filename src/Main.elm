module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes
import Html.Events exposing (onClick)
import Process
import Random
import Task
import Time exposing (..)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { randomBit : Maybe Int }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { randomBit = Nothing }, Cmd.none )



-- UPDATE


type Msg
    = Destiny Int
    | RandomTime
    | RandomTimeGenerated Int
    | Restart
    | TimeElapsed
    | GotDestinied Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Destiny waitTime ->
            ( { model | randomBit = Just -1 }, wait waitTime )

        RandomTime ->
            ( model, Random.generate RandomTimeGenerated (Random.int 0 5000) )

        RandomTimeGenerated randomTime ->
            ( { model | randomBit = Just -1 }, Task.perform identity (Task.succeed (Destiny randomTime)) )

        Restart ->
            init ()

        TimeElapsed ->
            ( model, Random.generate GotDestinied (Random.int 0 1) )

        GotDestinied randomBit ->
            ( { model | randomBit = Just randomBit }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ Html.Attributes.class "card vh-100"
        ]
        [ case model.randomBit of
            Nothing ->
                button
                    [ Html.Attributes.class "h-100 btn btn-danger fs-1"
                    , onClick RandomTime
                    ]
                    [ text "JA ODER NEIN" ]

            Just destiny ->
                if destiny == 0 then
                    div
                        [ Html.Attributes.class "card-body v-100 text-center bg-success d-flex justify-content-center align-items-center display-5"
                        , onClick Restart
                        ]
                        [ text "JA" ]

                else if destiny == -1 then
                    div [ Html.Attributes.class "d-flex justify-content-center align-items-center vh-100" ]
                        [ div
                            [ Html.Attributes.class "spinner-grow m-0"
                            , Html.Attributes.attribute "role" "status"
                            ]
                            [ Html.span
                                [ Html.Attributes.class "sr-only" ]
                                []
                            ]
                        ]

                else
                    div
                        [ Html.Attributes.class "card-body v-100 text-center bg-warning text-align-center d-flex justify-content-center align-items-center display-5"
                        , onClick Restart
                        ]
                        [ text "NEIN" ]
        ]


wait : Int -> Cmd Msg
wait ms =
    Process.sleep (ms |> toFloat)
        |> Task.perform (\_ -> TimeElapsed)
