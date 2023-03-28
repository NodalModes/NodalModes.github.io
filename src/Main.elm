module Main exposing (main)

import Browser exposing (element)
import Browser.Events as Events
import Colors exposing (..)
import Conway exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as ElEvents
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (footer, header)
import Json.Decode as Decode
import Random
import Simple.Transition as Transition exposing (properties)
import Task


main : Program Flags Model Msg
main =
    element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = Resized Int Int
    | PossibleConwayTrigger
    | ImmediateConwayTrigger
    | RandomStuff (List (List Float))
    | RestartConway


type alias Model =
    { device : Device
    , conway : Conway.Conway
    , mouseMoveCount : Int
    , rands : List (List Float)
    }


type alias Flags =
    { windowWidth : Int
    , windowHeight : Int
    }


randGen : Model -> Random.Generator (List (List Float))
randGen model =
    Random.list model.conway.height (Random.list model.conway.width (Random.float 0 1))


getRand : Model -> Cmd Msg
getRand model =
    Random.generate RandomStuff (randGen model)


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { device = getDevice flags.windowWidth flags.windowHeight
      , conway = Conway.init [ [] ]
      , mouseMoveCount = 0
      , rands = [ [] ]
      }
    , run RestartConway
    )


run : msg -> Cmd msg
run m =
    Task.perform (always m) (Task.succeed ())


getDevice : Int -> Int -> Device
getDevice width height =
    classifyDevice { width = width, height = height }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RestartConway ->
            ( model, getRand model )

        RandomStuff stuff ->
            ( { model
                | conway = Conway.init stuff
              }
            , Cmd.none
            )

        Resized newWidth newHeight ->
            ( { model
                | device = getDevice newWidth newHeight
              }
            , Cmd.none
            )

        PossibleConwayTrigger ->
            if model.mouseMoveCount == 1 then
                ( { model
                    | conway = Conway.update model.conway
                    , mouseMoveCount = modBy 8 (model.mouseMoveCount + 1)
                  }
                , Cmd.none
                )

            else
                ( { model
                    | mouseMoveCount = modBy 8 (model.mouseMoveCount + 1)
                  }
                , Cmd.none
                )

        ImmediateConwayTrigger ->
            ( { model
                | conway = Conway.update model.conway
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onResize (\width height -> Resized width height)
        ]


view : Model -> Html.Html Msg
view model =
    layoutWith
        { options =
            case model.device.class of
                Phone ->
                    [ forceHover ]

                Tablet ->
                    [ forceHover ]

                Desktop ->
                    []

                BigDesktop ->
                    []
        }
        [ Font.family [ Font.monospace ]
        , Font.color textColor
        , Border.width 10
        , Border.color darkGreen
        , Background.color white

        -- , width fill
        -- , height fill
        ]
    <|
        column
            [ width fill
            , height fill
            , spacing 10
            ]
            [ header
            , middle model
            , footer model
            ]


header : Element Msg
header =
    row
        [ width fill

        -- , explain Debug.todo
        , Background.color white
        , mouseOver [ Background.color lightGreen ]
        , backgroundFadeTransition
        ]
        [ el
            [ centerX
            , centerY
            , padding 10
            , Font.size 40
            , Region.heading 1
            ]
            (text "{- NodalModes -}")
        ]


middle : Model -> Element Msg
middle model =
    row
        [ width fill
        , height fill
        , padding 10
        , Background.color white

        -- , mouseOver [ Background.color lightGrey ]
        , ElEvents.onMouseMove PossibleConwayTrigger
        , ElEvents.onClick ImmediateConwayTrigger

        -- , backgroundFadeTransition
        , inFront <| content

        -- , explain Debug.todo
        ]
        [ Conway.view model.conway ]


backgroundFadeTransition : Attribute msg
backgroundFadeTransition =
    Transition.properties [ Transition.backgroundColor 1000 [] ] |> Element.htmlAttribute


content : Element msg
content =
    el
        [ centerX
        , centerY
        , clip -- , height fill
        , Region.mainContent
        ]
    <|
        el [ centerX, centerY, Font.size 30 ] (text contentText)


contentText : String
contentText =
    """
I am Ryan Ellis

    (not the hockey player) 

        (or the racecar driver)
"""


footerContent : List (Element Msg)
footerContent =
    [ Input.button
        [ height fill
        , padding 10
        , Background.color transWhite
        , mouseOver [ Background.color green ]
        , backgroundFadeTransition
        , Region.description "Restart button for Conway's game of life. Randomizes the grid. "
        ]
      <|
        { onPress = Just RestartConway, label = text "[Restart]" }
    , el
        [ centerX
        , centerY
        , padding 10
        , Background.color transWhite
        , mouseOver [ Background.color green ]
        , backgroundFadeTransition
        ]
      <|
        text "ellisryanjames@gmail.com"
    , el linkAttributes <|
        newTabLink []
            { url = "https://github.com/NodalModes"
            , label = text "GitHub"
            }
    , el linkAttributes <|
        newTabLink []
            { url = "https://www.linkedin.com/in/ryan-ellis-589938100"
            , label = text "LinkedIn"
            }
    , el linkAttributes <|
        newTabLink []
            { url = "https://elm-lang.org/"
            , label = text "Made in Elm"
            }
    ]


footer : Model -> Element Msg
footer model =
    el
        [ Background.color white
        , mouseOver [ Background.color lightGreen ]
        , backgroundFadeTransition
        , Region.footer
        , Font.size 20

        -- , padding 10
        , width fill

        -- , explain Debug.todo
        ]
    <|
        wrappedRow [ width fill, spacing 10 ] footerContent


linkAttributes : List (Attribute msg)
linkAttributes =
    [ centerX
    , centerY
    , padding 10
    , Background.color transWhite
    , mouseOver [ Background.color green ]
    , backgroundFadeTransition
    , Font.underline
    ]
