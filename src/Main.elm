module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Time exposing (..)
import Keyboard exposing (..)
import Random exposing (..)


type alias Model =
    { snake : Snake
    , direction : Direction
    , food : Food
    , snakeStatus : SnakeStatus
    , map : Map
    , mapSize : Int
    , hasMoved : Bool
    }


type alias Position =
    ( Int, Int )


type alias Food =
    Maybe Position


type alias Snake =
    { head : Position
    , tail : List Position
    , isGrowing : Bool
    }


type SnakeStatus
    = Dead
    | Alive


mapSize : Int
mapSize =
    15


tempInit : Model
tempInit =
    -- { snake = { head = ( 2, 2 ), tail = List.map (\x -> (2,x)) (List.range 3 14), isGrowing = False }
    { snake = { head = ( 1, 1 ), tail = [], isGrowing = False }
    , direction = Right
    , food = Nothing
    , snakeStatus = Alive
    , map = createMap mapSize
    , mapSize = mapSize
    , hasMoved = True
    }


init : Model
init =
    { tempInit | food = findNewFoodPosition tempInit }


type Direction
    = Up
    | Down
    | Left
    | Right


type Tile
    = Wall
    | Open Position


type alias Row =
    List Tile


type alias Map =
    List Row


zip : List a -> List b -> List ( a, b )
zip xs ys =
    case ( xs, ys ) of
        ( [], _ ) ->
            []

        ( _, [] ) ->
            []

        ( x :: xs, y :: ys ) ->
            ( x, y ) :: zip xs ys


createMap : Int -> Map
createMap size =
    List.map (createRow size) (List.range 1 size)


createRow : Int -> Int -> Row
createRow size y =
    List.map (\x -> generateTileAt size ( x, y )) (List.range 1 size)


generateTileAt : Int -> Position -> Tile
generateTileAt size ( x, y ) =
    if x == 1 || y == 1 || x == size || y == size then
        Wall
    else
        Open ( x, y )


tileToPic tile =
    let
        path x =
            "/static/snake/" ++ x ++ ".png"
    in
        case tile of
            Open pos ->
                path "open"

            y ->
                path (toString y)


view : Model -> Html a
view model =
    case model.snakeStatus of
        Alive ->
            let
                spanner tile =
                    span
                        []
                        [ img [ src (tileToPic tile) ] [] ]

                spannerSnake =
                    span
                        []
                        [ img [ src ("/static/snake/snake.png") ] [] ]

                spannerFood =
                    span
                        []
                        [ img [ src ("/static/snake/food.png") ] [] ]

                checkIfSnake pos =
                    List.member pos (consSnake model.snake)

                viewRow x y curRow =
                    case curRow of
                        [] ->
                            []

                        c :: cs ->
                            if checkIfSnake ( x, y ) then
                                spannerSnake :: viewRow (x + 1) y cs
                            else if checkIfFood ( x, y ) model.food then
                                spannerFood :: viewRow (x + 1) y cs
                            else
                                spanner c :: viewRow (x + 1) y cs

                viewMap y curMap =
                    List.map (\( val, row ) -> viewRow 0 val row)
                        (zip (List.range 0 model.mapSize) curMap)
            in
                div [ class "map" ] (List.map (div [ class "row" ]) (viewMap 0 model.map))

        Dead ->
            let
                score =
                    (List.length model.snake.tail - 1)
            in
                if score + 1 == (model.mapSize - 2) ^ 2 then
                    text <| "You won! Score = " ++ toString score
                else
                    text <| "U is ded. Score = " ++ toString score


checkIfFood : Position -> Maybe Position -> Bool
checkIfFood pos food =
    case food of
        Nothing ->
            False

        Just foodpos ->
            pos == foodpos


keyNrToDirection : Int -> Direction
keyNrToDirection keyNr =
    case keyNr of
        37 ->
            Left

        38 ->
            Up

        39 ->
            Right

        40 ->
            Down

        _ ->
            Left


type Msg
    = KeyMsg Int
    | TimeMsg Time.Time


isOpposite : Direction -> Direction -> Bool
isOpposite a b =
    case ( a, b ) of
        ( Up, Down ) ->
            True

        ( Down, Up ) ->
            True

        ( Left, Right ) ->
            True

        ( Right, Left ) ->
            True

        _ ->
            False


changeDirection : Int -> Model -> Model
changeDirection keyNr model =
    let
        newDir =
            keyNrToDirection keyNr
    in
        case ( isOpposite newDir model.direction, model.hasMoved ) of
            ( False, True ) ->
                { model | direction = newDir, hasMoved = False }

            _ ->
                model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Keyboard.downs KeyMsg, Time.every (700 * Time.millisecond) TimeMsg ]



-- update : msg -> model -> ( model, Cmd msg )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    -- case model.snakeStatus of
    -- Alive ->
    case msg of
        KeyMsg keyNr ->
            ( changeDirection keyNr model, Cmd.none )

        TimeMsg time ->
            ( moveSnakeInModel model, Cmd.none )


getNewPosition : Position -> Direction -> Position
getNewPosition ( x, y ) dir =
    case dir of
        Up ->
            ( x, y - 1 )

        Down ->
            ( x, y + 1 )

        Left ->
            ( x - 1, y )

        Right ->
            ( x + 1, y )


initial : List a -> List a
initial xs =
    case xs of
        a :: b :: c ->
            a :: initial (b :: c)

        _ ->
            []


legalHeadPosition : Position -> List Position -> Int -> Bool
legalHeadPosition head tail mapSize =
    let
        ( x, y ) =
            head
    in
        List.all (\v -> v >= 1 && v < mapSize - 1) [ x, y ]
            && not (List.member ( x, y ) tail)


makeTail : Bool -> Position -> Position -> List Position -> Maybe Position -> List Position
makeTail isHeadAtFood newHead oldHead tail food =
    let
        empty =
            List.isEmpty tail
    in
        newHead
            :: case ( isHeadAtFood, empty ) of
                ( True, True ) ->
                    [ oldHead ]

                ( True, False ) ->
                    tail

                ( False, True ) ->
                    []

                ( False, False ) ->
                    initial tail


randomPosGenerator : Generator Position
randomPosGenerator =
    pair (Random.int 1 (mapSize - 2)) (Random.int 1 (mapSize - 2))


consSnake : Snake -> List Position
consSnake snake =
    snake.head :: snake.tail


findNewFoodPosition : Model -> Maybe Position
findNewFoodPosition model =
    let
        generateUntil ( val, seed ) =
            case List.member val <| consSnake model.snake of
                False ->
                    Just val

                True ->
                    generateUntil <| Random.step randomPosGenerator seed
    in
        case List.length (consSnake model.snake) > (model.mapSize - 2) ^ 2 of
            True ->
                Nothing

            False ->
                generateUntil ( model.snake.head, (initialSeed 300) )


moveSnakeInModel model =
    let
        snake =
            model.snake

        newHead =
            getNewPosition snake.head model.direction

        isHeadAtFood =
            checkIfFood newHead model.food

        newTail =
            makeTail isHeadAtFood newHead snake.head snake.tail model.food

        updatedSnake =
            { snake | head = newHead, tail = newTail }

        updatedModel =
            { model | snake = updatedSnake, hasMoved = True }

        finalModel =
            case isHeadAtFood of
                True ->
                    { updatedModel | food = findNewFoodPosition updatedModel }

                False ->
                    updatedModel
    in
        case ( legalHeadPosition newHead snake.tail model.mapSize, finalModel.food ) of
            ( True, Just _ ) ->
                finalModel

            ( _, _ ) ->
                { finalModel | snakeStatus = Dead }


main =
    Html.program
        ({ init = ( init, Cmd.none )
         , update = update
         , subscriptions = subscriptions
         , view = view
         }
        )
