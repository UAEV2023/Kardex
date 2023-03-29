module Main exposing (..)

import Browser
import File exposing (File)
import File.Select as Select
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events exposing (..)



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
            , Select.files [ "image/*" ] GotFiles
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


view : Model -> Html Msg
view model =
    div []
        [ img [ Attributes.src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
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
