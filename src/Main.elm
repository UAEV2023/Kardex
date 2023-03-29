module Main exposing (..)

import Browser
import Css exposing (..)
import File exposing (File)
import File.Select as Select
import Html.Parser
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events exposing (..)
import Json.Decode as Decode
import Maybe.Extra
import Task



---- MODEL ----


type alias Model =
    { hover : Bool
    , files : List File
    , content : String
    }


init : ( Model, Cmd Msg )
init =
    ( { hover = False
      , files = []
      , content = ""
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Pick
    | DragEnter
    | DragLeave
    | GotFiles File (List File)
    | GotContent String


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
            , Task.perform GotContent (File.toString file)
            )

        GotContent content ->
            ( { model | content = content }
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
            [ text (Debug.toString (semesters (Html.Parser.runDocument model.content))) ]
        ]


semesters : Result (List a) Html.Parser.Document -> List Semester
semesters docResult =
    case docResult of
        Ok { document } ->
            document
                |> Tuple.second
                |> findByClassInNodeList "textoTablasKardex"
                |> List.map nodeToSemester

        Err _ ->
            []


type alias Semester =
    { title : Maybe String
    , subjects : List String
    }


nodeToSemester : Html.Parser.Node -> Semester
nodeToSemester node =
    case node of
        Html.Parser.Element _ _ ((Html.Parser.Element _ _ (titleNode :: nodesAfterTitle)) :: _) ->
            { title = firstText titleNode
            , subjects = []
            }

        _ ->
            Semester Nothing []


firstText : Html.Parser.Node -> Maybe String
firstText node =
    case node of
        Html.Parser.Text string ->
            Just string

        Html.Parser.Element _ _ nodes ->
            nodes
                |> List.map firstText
                |> Maybe.Extra.orList

        _ ->
            Nothing


findByClassInNodeList : String -> List Html.Parser.Node -> List Html.Parser.Node
findByClassInNodeList class nodes =
    case nodes of
        [] ->
            []

        node :: nextNodes ->
            findByClassInNode class node ++ findByClassInNodeList class nextNodes


findByClassInNode : String -> Html.Parser.Node -> List Html.Parser.Node
findByClassInNode class node =
    case node of
        Html.Parser.Element _ attributes nodes ->
            if attributes |> List.any (hasClass class) then
                [ node ]

            else
                findByClassInNodeList class nodes

        _ ->
            []


hasClass : String -> ( String, String ) -> Bool
hasClass className attribute =
    case attribute of
        ( "class", classNames ) ->
            classNames |> String.contains className

        _ ->
            False



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view >> toUnstyled
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
