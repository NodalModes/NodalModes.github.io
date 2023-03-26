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
import Element.Region as Region
import Html exposing (footer, header)
import Json.Decode as Decode
import Simple.Transition as Transition exposing (properties)


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


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { device = getDevice flags.windowWidth flags.windowHeight
      , conway = Conway.init ()
      , mouseMoveCount = 0
      }
    , Cmd.none
    )


getDevice : Int -> Int -> Device
getDevice width height =
    classifyDevice { width = width, height = height }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resized newWidth newHeight ->
            ( { model
                | device = getDevice newWidth newHeight
                , conway = Conway.update model.conway
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


type alias Model =
    { device : Device
    , conway : Conway.Conway
    , mouseMoveCount : Int
    }


type alias Flags =
    { windowWidth : Int
    , windowHeight : Int
    }


view : Model -> Html.Html Msg
view model =
    layoutWith
        { options =
            [-- forceHover
            ]
        }
        [ Font.family [ Font.monospace ]
        , Font.color textColor
        , Background.color bgColor
        ]
    <|
        column
            [ Border.width 10
            , Border.color black
            , width fill
            , height fill
            , spacing 10
            ]
            [ header
            , middle model
            , footer model
            ]


header : Element msg
header =
    row
        [ width fill
        , padding 10

        -- , explain Debug.todo
        , Background.color white
        , mouseOver [ Background.color lightGrey ]
        , backgroundFadeTransition
        ]
        [ el
            [ centerX
            , centerY
            , Font.size 32
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
        , behindContent <| Conway.view model.conway

        -- , explain Debug.todo
        ]
        [ content ]


backgroundFadeTransition : Attribute msg
backgroundFadeTransition =
    Transition.properties [ Transition.backgroundColor 1000 [] ] |> Element.htmlAttribute


content : Element msg
content =
    el
        [ width fill
        , Region.mainContent
        ]
    <|
        el [ centerX, centerY ] (text contentText)


contentText : String
contentText =
    """Howdy!

I am Ryan Ellis. 
(Not the hockey player.) 
(Not the racecar driver.)
"""


footerContent : List (Element msg)
footerContent =
    [ el
        [ centerX
        , centerY
        , padding 10
        , Background.color transWhite
        , mouseOver [ Background.color grey ]
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


footer : Model -> Element msg
footer model =
    el
        [ Background.color white
        , mouseOver [ Background.color lightGrey ]
        , backgroundFadeTransition
        , Region.footer

        -- , padding 10
        , width fill

        -- , explain Debug.todo
        ]
    <|
        let
            orientation =
                model.device.orientation

            class =
                model.device.class

            situation =
                ( class, orientation )
        in
        case situation of
            ( Phone, _ ) ->
                column [ width fill, spacing 10 ] footerContent

            ( _, Landscape ) ->
                row [ width fill, spacing 10 ] footerContent

            ( _, Portrait ) ->
                column [ width fill, spacing 10 ] footerContent


linkAttributes : List (Attribute msg)
linkAttributes =
    [ centerX
    , centerY
    , padding 10
    , Background.color transWhite
    , mouseOver [ Background.color grey ]
    , backgroundFadeTransition
    , Font.underline
    ]
