module Hello exposing (..)

import Html exposing (Html, button, div, program, text)
import Html.Events exposing (onClick)
import List exposing (..)
import Array exposing (..)

-- MODEL


type Turn = Left | Right

numValue : Turn -> Int
numValue turn =
    case turn of
        Left ->
            -1
        Right ->
            1


type alias Model =
    { current : List Turn
    , upcoming : List Turn
    , distances : Array Int
    }

initTurn : Turn
initTurn = Left

initModel = Model [initTurn] [] (Array.repeat 1 (numValue initTurn))

init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


-- MESSAGES


type Msg = Next | NoOp


-- VIEW

viewOne : Turn -> Int -> Html Msg
viewOne turn distance =
    case turn of
        Left ->
            div [] [text ("L  " ++ toString(distance))]
        Right ->
            div [] [text ("R  " ++ toString(distance))]


view : Model -> Html Msg
view model =
    div []
      [ button [onClick Next ] [ text "+" ]
      , div [] (List.map2 viewOne model.current (Array.toList model.distances))
      ]



-- UPDATE
reverse_single : Turn -> Turn
reverse_single turn =
    case turn of
        Left ->
            Right
        Right ->
            Left


reversed : List Turn -> List Turn
reversed turns =
        reverse (List.map reverse_single turns)


next_distance : Array Int -> Turn -> Int
next_distance distances turn =
    (Maybe.withDefault (numValue initTurn) (Array.get ((Array.length distances) - 1) distances)) + (numValue turn)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Next ->
            if List.length(model.upcoming) > 0 then
                let
                    next = Maybe.withDefault initTurn (List.head model.upcoming)
                    current =
                        model.current ++ [next]
                    upcoming = List.drop 1 model.upcoming
                    distances = Array.push (next_distance model.distances next) model.distances
                in
                    (Model current upcoming distances, Cmd.none)
            else
                let
                    upcoming = reversed model.current
                    current = model.current ++ [initTurn]
                    distances = Array.push (next_distance model.distances initTurn) model.distances
                in
                    (Model current upcoming distances, Cmd.none)
        NoOp ->
            (model, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }