port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (div, text)
import Html.Attributes
import Html.Events exposing (onClick)
import Process
import Random
import Serialize as S
import Task
import Time exposing (..)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), s)
import Url.Parser.Query as Query



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = \_ -> NoOp
        , onUrlChange = UrlChanged
        }



-- PORTS


port copyToClipboard : String -> Cmd msg



-- MODEL


type Model
    = Start (Maybe String)
    | AskPage (Maybe String)
    | Loading
    | Finished Int


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url _ =
    let
        queryValue =
            Parser.parse questionRoute url
    in
    ( Start (Maybe.andThen (\q -> q) queryValue), Cmd.none )



-- UPDATE


type Msg
    = UrlChanged Url
    | Destiny Int
    | Ask
    | AskInput String
    | CopyToClipboard String
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
            ( Start (Maybe.andThen identity (Parser.parse questionRoute url))
            , Cmd.none
            )

        Ask ->
            ( AskPage Nothing, Cmd.none )

        AskInput question ->
            ( AskPage (Just question), Cmd.none )

        CopyToClipboard question ->
            ( model, copyToClipboard question )

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
            [ Html.Attributes.class "vh-100"
            ]
            [ case model of
                Start maybeQuestion ->
                    case maybeQuestion of
                        Nothing ->
                            div
                                [ Html.Attributes.class "row vh-100" ]
                                [ div
                                    [ Html.Attributes.class "col-6 h-100 text-center bg-danger d-flex justify-content-center align-items-center display-5"
                                    , onClick RandomTime
                                    ]
                                    [ text "JA ODER NEIN?!" ]
                                , div
                                    [ Html.Attributes.class "col-6 justify-content-center align-items-center text-center d-flex h-100 bg-secondary display-5"
                                    , onClick Ask
                                    ]
                                    [ text "EINE FRAGE BEANTWORTEN LASSEN" ]
                                ]

                        Just question ->
                            let
                                x =
                                    S.decodeFromString S.string question
                            in
                            div
                                [ Html.Attributes.class "row vh-100" ]
                                [ div
                                    [ Html.Attributes.class "col-12 h-100 text-center bg-danger d-flex justify-content-center align-items-center display-5"
                                    , onClick RandomTime
                                    ]
                                    [ text
                                        (case x of
                                            Ok q ->
                                                q

                                            Err _ ->
                                                "JA ODER NEIN?!"
                                        )
                                    ]
                                ]

                AskPage maybeQuestion ->
                    div
                        [ Html.Attributes.class "vh-100" ]
                        [ div
                            [ Html.Attributes.class "row g-0" ]
                            [ div
                                [ Html.Attributes.class "col-12 bg-secondary d-flex align-items-center"
                                , Html.Attributes.style "height" "100vh"
                                ]
                                [ div
                                    [ Html.Attributes.class "input-group w-75 mx-auto my-3 align-middle" ]
                                    [ Html.input
                                        [ Html.Attributes.class "form-control"
                                        , Html.Attributes.placeholder "Was möchtest du wissen?"
                                        , Html.Attributes.type_ "text"
                                        , Html.Events.onInput (\s -> AskInput s)
                                        , Html.Attributes.attribute "aria-describedby" "basic-addon1"
                                        ]
                                        []
                                    , Html.button
                                        [ Html.Attributes.class "input-group-text"
                                        , Html.Attributes.id "basic-addon1"
                                        , case maybeQuestion of
                                            Nothing ->
                                                Html.Attributes.disabled True

                                            Just question ->
                                                let
                                                    url =
                                                        "https://schicksal.jokogr.de/question?q=" ++ S.encodeToString S.string question
                                                in
                                                onClick (CopyToClipboard url)
                                        ]
                                        [ text "Link kopieren" ]
                                    ]
                                ]
                            ]
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
