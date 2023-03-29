module Main exposing (..)

import Browser
import Css exposing (..)
import File exposing (File)
import File.Select as Select
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events exposing (..)
import Json.Decode as Decode



---- MODEL ----


type alias Model =
    { hover : Bool
    , files : List File
    }


init : ( Model, Cmd Msg )
init =
    ( { hover = False
      , files = []
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Pick
    | DragEnter
    | DragLeave
    | GotFiles File (List File)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Pick ->
            ( model
            , Select.files [ "*" ] GotFiles
            )

        DragEnter ->
            ( { model | hover = True }
            , Cmd.none
            )

        DragLeave ->
            ( { model | hover = False }
            , Cmd.none
            )

        GotFiles file files ->
            ( { model
                | files = file :: files
                , hover = False
              }
            , Cmd.none
            )



---- VIEW ----


alwaysPreventDefault :
    msg
    ->
        { message : msg
        , stopPropagation : Bool
        , preventDefault : Bool
        }
alwaysPreventDefault msg =
    { message = msg
    , stopPropagation = True
    , preventDefault = True
    }


hijackOn : String -> Decode.Decoder msg -> Attribute msg
hijackOn event decoder =
    custom event (Decode.map alwaysPreventDefault decoder)


view : Model -> Html Msg
view model =
    styled div
        [ border3 (px 2) dashed (rgb 11 14 17)
        , borderRadius (rem 1)
        , width (pct 80)
        , height (rem 8)
        ]
        [ hijackOn "drop" (Decode.at [ "dataTransfer", "files" ] (Decode.oneOrMore GotFiles File.decoder))
        , hijackOn "dragover" (Decode.succeed DragEnter)
        , on "dragenter" (Decode.succeed DragEnter)
        , on "dragleave" (Decode.succeed DragLeave)
        ]
        [ button [ onClick Pick ] [ text "Upload Images" ]
        , styled span
            [ color (rgb 11 14 17) ]
            []
            [ text (Debug.toString model) ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view >> toUnstyled
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
