module Main exposing (..)

import Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import DeckGenerator exposing (..)


tupleMap : (a -> b) -> ( a, a ) -> ( b, b )
tupleMap f ( a, b ) =
    ( f a, f b )


viewCard : Card -> Html Msg
viewCard card =
    let
        path x =
            "/static/cats/" ++ x

        picPath x =
            "/static/cats/" ++ x ++ ".jpg"
    in
        case card.state of
            Closed ->
                div [ class "closed", onClick (CardClick card) ] [ img [ src (path "closed.png") ] [] ]

            Open ->
                div [ class "open" ] [ img [ src (picPath card.id) ] [] ]

            Matched ->
                div [ class "matched" ] [ img [ src (picPath card.id) ] [] ]


viewCards : List Card -> Html Msg
viewCards cards =
    div [] (List.map viewCard cards)


setCard : CardState -> Card -> Card
setCard s c =
    { c | state = s }


isCardEqual a b =
    a.id == b.id && a.group == b.group



{-
   updateFun : Msg -> Model -> Model
   updateFun (CardClick card) model =
       let
           f c =
               if isCardEqual c card then
                   { c | state = Open }
               else
                   c
       in
           case model.gameState of
               Choosing deck ->
                   { model | gameState = Choosing (List.map f deck) }

               Matching deck c ->
                   { model | gameState = Matching (List.map f deck) c }

               GameOver ->
                   model
-}


updateFun (CardClick card) model =
    { model | gameState = updateCardClick card model.gameState }


viewFun : Model -> Html Msg
viewFun mod =
    case mod.gameState of
        Choosing deck ->
            viewCards deck

        Matching deck _ ->
            viewCards deck

        GameOver ->
            viewCards []


updateCardClick : Card -> GameState -> GameState
updateCardClick card gameState =
    case gameState of
        GameOver ->
            GameOver

        Choosing deck ->
            let
                a =
                    List.map
                        (\x ->
                            if not (x.state == Matched) then
                                { x | state = Closed }
                            else
                                x
                        )
                        deck

                c =
                    { card | state = Open }

                updatedDeck =
                    List.map
                        (\x ->
                            if isCardEqual x card then
                                c
                            else
                                x
                        )
                        a
            in
                Matching updatedDeck c

        Matching deck c ->
            let
                ( cNew, cardNew ) =
                    if c.id == card.id then
                        tupleMap (setCard Matched) ( c, card )
                    else
                        ( c, setCard Open card )

                updatedDeck =
                    List.map
                        (\x ->
                            if isCardEqual x card then
                                cardNew
                            else if isCardEqual x c then
                                cNew
                            else
                                x
                        )
                        deck
            in
                if List.all (\x -> x.state == Matched) updatedDeck then
                    GameOver
                else
                    Choosing updatedDeck


main =
    Html.beginnerProgram
        { model =
            { gameState = Choosing DeckGenerator.static }
        , view = viewFun
        , update = updateFun
        }
