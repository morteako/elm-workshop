-- Model.elm

module Model exposing (..)


type GameState
    = Choosing Deck
    | Matching Deck Card
    | GameOver


type alias Model =
    { game : GameState
    }


type Msg
    = CardClicked Card
    | DeckGenerated Deck
    | RestartGame


type CardState
    = Open
    | Closed
    | Matched


type alias Deck =
    List Card


type Group
    = A
    | B


type alias Card =
    { id : String
    , group : Group
    , state : CardState
    }


-- Main.elm

module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import DeckGenerator
import Model exposing (..)
import Random


viewCard : Card -> Html Msg
viewCard card =
    case card.state of
        Open ->
            img
                [ class "card open"
                , src ("/static/cats/" ++ card.id ++ ".jpg")
                ]
                []

        Closed ->
            img
                [ class "card closed"
                , onClick (CardClicked card)
                , src ("/static/cats/closed.png")
                ]
                []

        Matched ->
            img
                [ class "card matched"
                , src ("/static/cats/" ++ card.id ++ ".jpg")
                ]
                []


viewCards : Deck -> Html Msg
viewCards cards =
    div []
        [ h1 [] [ text "Memory Meow" ]
        , div [ class "cards" ]
            (List.map viewCard cards)
        ]


setCard : CardState -> Card -> Deck -> Deck
setCard state card deck =
    List.map
        (\c ->
            if c.id == card.id && c.group == card.group then
                { card | state = state }
            else
                c
        )
        deck


isMatching : Card -> Card -> Bool
isMatching c1 c2 =
    c1.id == c2.id && c1.group /= c2.group


closeUnmatched : Deck -> Deck
closeUnmatched deck =
    List.map
        (\c ->
            if c.state /= Matched then
                { c | state = Closed }
            else
                c
        )
        deck


allMatched : Deck -> Bool
allMatched deck =
    List.all (\c -> c.state == Matched) deck


updateCardClick : Card -> GameState -> GameState
updateCardClick clickedCard game =
    case game of
        Choosing deck ->
            let
                updatedDeck =
                    deck
                        |> closeUnmatched
                        |> setCard Open clickedCard
            in
                Matching updatedDeck clickedCard

        Matching deck openCard ->
            let
                updatedDeck =
                    if isMatching clickedCard openCard then
                        deck
                            |> setCard Matched clickedCard
                            |> setCard Matched openCard
                    else
                        setCard Open clickedCard deck
            in
                if allMatched updatedDeck then
                    GameOver
                else
                    Choosing updatedDeck

        GameOver ->
            game


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CardClicked card ->
            ( { model | game = updateCardClick card model.game }, Cmd.none )

        DeckGenerated deck ->
            ( { game = Choosing deck }, Cmd.none )

        RestartGame ->
            ( init, Cmd.none )


init : Model
init =
    { game = Choosing DeckGenerator.static }


view : Model -> Html Msg
view model =
    case model.game of
        Choosing deck ->
            viewCards deck

        Matching deck card ->
            viewCards deck

        GameOver ->
            div [ class "victory" ]
                [ text "You won!"
                , button
                    [ class "restart"
                    , onClick RestartGame
                    ]
                    [ text "Click to restart"
                    ]
                ]


main =
    Html.program
        { init =
            ( init
            , Random.generate DeckGenerated DeckGenerator.random
            )
        , view = view
        , update = update
        , subscriptions = \s -> Sub.none
        }
