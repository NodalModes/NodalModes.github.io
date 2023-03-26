module Conway exposing (..)

import Array exposing (Array)
import Colors exposing (..)
import Element exposing (column, el, explain, fill, height, none, padding, paddingXY, px, row, spacing, text, width)
import Element.Background as Background
import Html exposing (Html)
import Maybe exposing (andThen)
import Random
import Simple.Transition as Transition exposing (properties)


type alias Conway =
    { width : Int
    , height : Int
    , grid : Array (Array Bool)
    }


view : Conway -> Element.Element msg
view conway =
    column
        [ width fill, height fill, spacing 10, paddingXY 10 0 ]
    <|
        (arrArrToListList conway |> something)


arrArrToListList : Conway -> List (List Bool)
arrArrToListList conway =
    List.map Array.toList (Array.toList conway.grid)


something : List (List Bool) -> List (Element.Element msg)
something lLBool =
    List.map (List.map boolToText) lLBool
        |> List.map (row [ width fill, height fill, spacing 10 ])


boolToText : Bool -> Element.Element msg
boolToText bool =
    if bool then
        el
            [ Background.color lightGrey
            , backgroundFadeTransition
            , width fill
            , height fill
            ]
            (text "")

    else
        el
            [ Background.color white
            , backgroundFadeTransition
            , width fill
            , height fill
            ]
            (text "")


backgroundFadeTransition : Element.Attribute msg
backgroundFadeTransition =
    Transition.properties [ Transition.backgroundColor 500 [] ] |> Element.htmlAttribute


update : Conway -> Conway
update conway =
    { conway | grid = updateGrid conway }


updateGrid : Conway -> Array (Array Bool)
updateGrid conway =
    Array.indexedMap (\y -> Array.indexedMap (applyRules conway y)) conway.grid



-- applyRules needs actual conway rules


applyRules : Conway -> Int -> Int -> Bool -> Bool
applyRules conway y x alive =
    let
        h =
            conway.height

        w =
            conway.width

        g =
            conway.grid

        ym1xm1 =
            getNeighborAlive h w g (y - 1) (x - 1)

        ym1x =
            getNeighborAlive h w g (y - 1) x

        ym1xp1 =
            getNeighborAlive h w g (y - 1) (x + 1)

        yxm1 =
            getNeighborAlive h w g y (x - 1)

        yxp1 =
            getNeighborAlive h w g y (x + 1)

        yp1xm1 =
            getNeighborAlive h w g (y + 1) (x - 1)

        yp1x =
            getNeighborAlive h w g (y + 1) x

        yp1xp1 =
            getNeighborAlive h w g (y + 1) (x + 1)

        neighborAliveCount =
            ym1xm1 + ym1x + ym1xp1 + yxm1 + yxp1 + yp1xm1 + yp1x + yp1xp1
    in
    if alive then
        if neighborAliveCount == 2 || neighborAliveCount == 3 then
            True

        else
            False

    else if neighborAliveCount == 3 then
        True

    else
        False


getNeighborAlive : Int -> Int -> Array (Array Bool) -> Int -> Int -> Int
getNeighborAlive h w g y x =
    case Array.get (modBy h y) g |> andThen (Array.get (modBy w x)) of
        Just b ->
            if b then
                1

            else
                0

        Nothing ->
            0


init : () -> Conway
init _ =
    let
        w =
            32

        h =
            18
    in
    { width = w
    , height = h
    , grid = initializeGrid h w
    }


initializeGrid : Int -> Int -> Array (Array Bool)
initializeGrid width height =
    Array.initialize width (\bbb -> Array.initialize height (getBoolFromIndex bbb))


getBoolFromIndex : Int -> Int -> Bool
getBoolFromIndex bbb index =
    if index >= bbb - 3 && index <= bbb + 3 then
        True

    else
        False
