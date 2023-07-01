module UI.Styles exposing
    ( body1
    , containedButton
    , headline4
    , outlinedButtonWithIcon
    , semesterProgressRow
    )

import Css exposing (..)



{- Overlay spec and implementation:
   https://m2.material.io/design/interaction/states.html#anatomy
   https://stackoverflow.com/a/44616196/13194448
-}


baseOverlay : Style
baseOverlay =
    batch
        [ position absolute
        , padding (px 0)
        , top (px 0)
        , left (px 0)
        , width (pct 100)
        , height (pct 100)
        , overflow hidden
        , backgroundColor currentColor
        ]


overlayOnColoredSurface : Style
overlayOnColoredSurface =
    batch
        [ baseOverlay
        , opacity (num 0)
        , hover [ opacity (num 0.08) ]
        , focus [ opacity (num 0.24) ]
        , active [ opacity (num 0.32) ]
        ]


overlayOnWhiteBackground : Style
overlayOnWhiteBackground =
    batch
        [ baseOverlay
        , opacity (num 0)
        , hover [ opacity (num 0.04) ]
        , focus [ opacity (num 0.08) ]
        , active [ opacity (num 0.12) ]
        ]



{- Button spec:
   https://m2.material.io/components/buttons
   https://m2.material.io/design/typography/the-type-system.html#type-scale
-}


baseButton : Style
baseButton =
    batch
        [ position relative
        , fontSize (px 14)
        , fontWeight (int 500)
        , letterSpacing (px 1.25)
        , fontFamilies [ "Roboto", "sans-serif" ]
        , textTransform uppercase
        , cursor pointer
        , borderRadius (px 4)
        , outline none
        , property "transition-duration" "0.28s"
        ]


containedButton : Style
containedButton =
    batch
        [ baseButton
        , backgroundColor (hex "#004479")
        , color (hex "#FFFFFF")
        , paddingLeft (rem 1)
        , paddingRight (rem 1)
        , height (rem 2.25)
        , minWidth (rem 4)
        , border (px 0)
        , property "box-shadow" "0px 3px 1px -2px rgba(0, 0, 0, 0.2), 0px 2px 2px 0px rgba(0, 0, 0, 0.14), 0px 1px 5px 0px rgba(0, 0, 0, 0.12)"
        , hover [ property "box-shadow" "0px 2px 4px -1px rgba(0, 0, 0, 0.2), 0px 4px 5px 0px rgba(0, 0, 0, 0.14), 0px 1px 10px 0px rgba(0, 0, 0, 0.12)" ]
        , focus [ property "box-shadow" "0px 2px 4px -1px rgba(0, 0, 0, 0.2), 0px 4px 5px 0px rgba(0, 0, 0, 0.14), 0px 1px 10px 0px rgba(0, 0, 0, 0.12)" ]
        , after
            [ property "content" "\"\""
            , overlayOnColoredSurface
            ]
        ]


outlinedButtonWithIcon : Style
outlinedButtonWithIcon =
    batch
        [ baseButton
        , backgroundColor transparent
        , color (hex "#002E5F")
        , border3 (px 1) solid (rgba 0 0 0 0.12)
        , paddingLeft (rem 0.75)
        , paddingRight (rem 1)
        , height (rem 2.25)
        , minWidth (rem 4)
        , property "display" "grid"
        , property "place-items" "center"
        , property "grid-template-columns" "auto auto"
        , property "grid-gap" "0.5rem"
        , after
            [ property "content" "\"\""
            , overlayOnWhiteBackground
            ]
        ]


body1 : Style
body1 =
    batch
        [ letterSpacing (px 0.5)
        , fontSize (em 1)
        ]


headline4 : Style
headline4 =
    batch
        [ fontSize (em 2.125)
        , letterSpacing (px 0.25)
        , fontWeight (int 400)
        ]


headline5 : Style
headline5 =
    batch
        [ fontSize (em 1.5)
        , letterSpacing (px 0)
        , fontWeight (int 400)
        ]


semesterProgressRow : Style
semesterProgressRow =
    batch
        [ property "display" "grid"
        , property "grid-gap" "1.6rem"
        , property "grid-template-columns" "repeat(auto-fit, 12rem)"
        , property "justify-content" "center"
        , property "align-items" "start"
        ]
