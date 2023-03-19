module Main exposing (main)

import Browser exposing (element)
import Browser.Events as Events
import Colors exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Html exposing (footer, header)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = Resized Int Int


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { device = getDevice flags.windowWidth flags.windowHeight }
    , Cmd.none
    )


getDevice : Int -> Int -> Device
getDevice width height =
    classifyDevice { width = width, height = height }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resized newWidth newHeight ->
            ( { model | device = getDevice newWidth newHeight }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Events.onResize (\width height -> Resized width height)


type alias Model =
    { device : Device }


type alias Flags =
    { windowWidth : Int
    , windowHeight : Int
    }


view : Model -> Html.Html msg
view model =
    layout
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
            [ header model
            , middle
            , footer model
            ]


header : Model -> Element msg
header model =
    row
        [ width fill
        , padding 10

        -- , explain Debug.todo
        , mouseOver [ Background.color lightGrey ]
        ]
        [ el
            [ centerX
            , centerY
            , Font.size 32
            , Region.heading 1
            ]
            (text "{- NodalModes -}")
        ]


middle : Element msg
middle =
    row
        [ width fill
        , height fill
        , mouseOver [ Background.color lightGrey ]

        -- , explain Debug.todo
        ]
        [ content ]


content : Element msg
content =
    el
        [ width fill
        , Region.mainContent
        , mouseOver [ Background.color lightGrey ]
        ]
    <|
        el [ centerX, centerY ] (text contentText)


contentText : String
contentText =
    """
Howdy!

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
        , mouseOver [ Background.color grey ]
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
        [ mouseOver [ Background.color lightGrey ]
        , Region.footer
        , padding 10
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
                column [ width fill ] (footerContent ++ [ text "Phone" ])

            ( _, Landscape ) ->
                row [ width fill ] (footerContent ++ [ text "_Landscape" ])

            ( _, Portrait ) ->
                column [ width fill ] (footerContent ++ [ text "_Portrait" ])


linkAttributes : List (Attribute msg)
linkAttributes =
    [ centerX
    , centerY
    , padding 10
    , mouseOver [ Background.color grey ]
    , Font.underline
    ]
