module Main exposing (main)

import Colors exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Html exposing (footer, header)


main : Html.Html msg
main =
    layout
        [ Font.family [ Font.monospace ]
        , Font.color textColor
        , Background.color bgColor

        -- , Border.width 10
        ]
        view


view : Element msg
view =
    column
        [ Border.width 10
        , Border.color black
        , width fill
        , height fill
        , spacing 10
        ]
        [ header
        , middle
        , footer
        ]


header : Element msg
header =
    row
        [ width fill
        , padding 10

        -- , Border.width 10
        -- , Border.color (rgb 0 0 0)
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
        el [ centerX, centerY ] (text "Content")


footer : Element msg
footer =
    row
        [ mouseOver [ Background.color lightGrey ]
        , Region.footer
        , padding 10

        -- , explain Debug.todo
        , width fill
        ]
    <|
        [ el linkAttributes <|
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


linkAttributes : List (Attribute msg)
linkAttributes =
    [ centerX
    , centerY
    , padding 10
    , mouseOver [ Background.color grey ]
    , Font.underline
    ]
