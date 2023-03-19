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


footer : Element msg
footer =
    row
        [ mouseOver [ Background.color lightGrey ]
        , Region.footer
        , padding 10
        , width fill

        -- , explain Debug.todo
        ]
    <|
        [ el linkAttributes <|
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


linkAttributes : List (Attribute msg)
linkAttributes =
    [ centerX
    , centerY
    , padding 10
    , mouseOver [ Background.color grey ]
    , Font.underline
    ]
