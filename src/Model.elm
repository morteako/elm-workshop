module Model exposing (..)


type CardState
    = Open
    | Closed
    | Matched


type alias Card =
    { id : String
    , state : CardState
    , group : Group
    }


type Group
    = A
    | B


type GameState
    = Choosing Deck
    | Matching Deck Card
    | GameOver


type alias Model =
    { gameState : GameState }


type Msg
    = CardClick Card


type alias Deck =
    List Card
