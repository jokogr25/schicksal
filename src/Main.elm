module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Browser.Navigation as Nav
import Html exposing (div, text)
import Html.Attributes
import Html.Events exposing (onClick)
import Process
import Random
import Task
import Time exposing (..)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), s)
import Url.Parser.Query as Query



-- MAIN


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = \_ -> NoOp
        , onUrlChange = UrlChanged
        }



-- MODEL


type Model
    = Start (Maybe String)
    | Loading
    | Finished Int


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url _ =
    let
        queryValue =
            Parser.parse questionRoute url
    in
    ( Start (Maybe.andThen (\q -> q) queryValue)
    , Cmd.none
    )



-- UPDATE


type Msg
    = UrlChanged Url
    | Destiny Int
    | RandomTime
    | RandomTimeGenerated Int
    | Restart
    | TimeElapsed
    | GotDestinied Int
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Destiny waitTime ->
            ( model, wait waitTime )

        RandomTime ->
            ( model, Random.generate RandomTimeGenerated (Random.int 0 5000) )

        RandomTimeGenerated randomTime ->
            ( Loading
            , Task.perform identity (Task.succeed (Destiny randomTime))
            )

        Restart ->
            ( Start Nothing
            , Cmd.none
            )

        TimeElapsed ->
            ( model, Random.generate GotDestinied (Random.int 0 1) )

        GotDestinied randomBit ->
            ( Finished randomBit, Cmd.none )

        UrlChanged url ->
            ( Start (Maybe.andThen (\q -> q) (Parser.parse questionRoute url))
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )



-- ROUTES


questionRoute : Parser.Parser (Maybe String -> a) a
questionRoute =
    Parser.map identity
        (s "question"
            <?> Query.string "q"
        )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Schicksal"
    , body =
        [ div
            [ Html.Attributes.class "card vh-100"
            ]
            [ case model of
                Start x ->
                    div
                        [ Html.Attributes.class
                            "card-body h-100 text-center bg-warning d-flex justify-content-center align-items-center display-5"
                        , onClick RandomTime
                        ]
                        [ text (Maybe.withDefault "JA ODER NEIN" x)
                        ]

                Loading ->
                    div
                        [ Html.Attributes.class "card-body h-100 text-center bg-light d-flex justify-content-center align-items-center display-5"
                        , onClick RandomTime
                        ]
                        [ div
                            [ Html.Attributes.class "spinner-grow m-0"
                            , Html.Attributes.attribute "role" "status"
                            , Html.Attributes.attribute "style" "width: 7rem; height: 7rem;"
                            ]
                            [ Html.span
                                [ Html.Attributes.class "sr-only" ]
                                []
                            ]
                        ]

                Finished randomBit ->
                    if randomBit == 0 then
                        div
                            [ Html.Attributes.class "card-body h-100 text-center bg-danger d-flex justify-content-center align-items-center display-5"
                            , onClick Restart
                            ]
                            [ text "NEIN" ]

                    else
                        div
                            [ Html.Attributes.class "card-body h-100 text-center bg-success d-flex justify-content-center align-items-center display-5"
                            , onClick Restart
                            ]
                            [ text "JA" ]
            ]
        ]
    }


wait : Int -> Cmd Msg
wait ms =
    Process.sleep (ms |> toFloat)
        |> Task.perform (\_ -> TimeElapsed)
