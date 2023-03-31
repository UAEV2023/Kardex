module Main exposing (..)

import Browser
import Css exposing (..)
import Dict exposing (Dict)
import Dict.Extra
import File exposing (File)
import File.Select as Select
import Html.Parser
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events exposing (..)
import Json.Decode as Decode
import Maybe.Extra
import Set
import Task



---- MODEL ----


type alias Model =
    { hover : Bool
    , files : List File
    , kardex : Maybe (List PeriodoCursado)
    }


init : ( Model, Cmd Msg )
init =
    ( { hover = False
      , files = []
      , kardex = Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Pick
    | DragEnter
    | DragLeave
    | GotFiles File (List File)
    | GotKardex (Maybe (List PeriodoCursado))


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
            , Task.perform GotKardex
                (File.toString file
                    |> Task.map Html.Parser.runDocument
                    |> Task.map leerKardex
                    |> Task.map Just
                )
            )

        GotKardex content ->
            ( { model | kardex = content }
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
    case model.kardex of
        Nothing ->
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
                [ button
                    [ onClick Pick ]
                    [ text "Subir Kardex" ]
                ]

        Just kardex ->
            kardex
                |> organizarKardexPorNombre
                |> avanceDeMallaCurricular mallaCurricularIngSoftware
                |> Debug.toString
                |> text


type alias MallaCurricular =
    { obligatorias : List (List String)
    , optativas : List String
    , libres : List String
    }


mallaCurricularIngSoftware : MallaCurricular
mallaCurricularIngSoftware =
    { obligatorias =
        [ [ "Álgebra Intermedia"
          , "Geometría Analítica"
          , "Algoritmia"
          , "Fundamentos de Ingeniería de Software"
          , "Responsabilidad Social Universitaria"
          ]
        , [ "Álgebra Superior"
          , "Cálculo Diferencial"
          , "Programación Estructurada"
          , "Matemáticas Discretas"
          , "Cultura Maya"
          ]
        , [ "Álgebra Lineal"
          , "Cálculo Integral"
          , "Programación Orientada a Objetos"
          , "Teoría de la Computación"
          , "Arquitectura y Organización de Computadoras"
          ]
        , [ "Probabilidad"
          , "Diseño de Software"
          , "Estructuras de Datos"
          , "Sistemas Operativos"
          , "Teoría de Lenguajes de Programación"
          ]
        , [ "Inferencia Estadística"
          , "Arquitecturas de Software"
          , "Construcción de Software"
          , "Diseño de Bases de Datos"
          , "Desarrollo de Aplicaciones Web"
          ]
        , [ "Métricas de Software"
          , "Aseguramiento de la Calidad del Software"
          , "Requisitos de Software"
          , "Interacción Humano Computadora"
          ]
        , [ "Experimentación en Ingeniería de Software"
          , "Verificación y Validación de Software"
          , "Redes y Seguridad de Computadoras"
          , "Innovación Tecnológica"
          ]
        , [ "Administración de Proyectos I"
          , "Mantenimiento de Software"
          , "Sistemas Distribuidos"
          ]
        , [ "Administración de Proyectos II" ]
        ]
    , optativas = []
    , libres = []
    }


type alias AvanceDeMallaCurricular =
    { etiqueta : String
    , materias : List ( String, List MateriaCursada )
    }


materiasFueraDeLaMallaCurricular : MallaCurricular -> Dict String (List MateriaCursada) -> List ( String, List MateriaCursada )
materiasFueraDeLaMallaCurricular { obligatorias, optativas, libres } materias =
    materias
        |> Dict.Extra.removeMany (Set.fromList (List.concat [ optativas, libres, List.concat obligatorias ]))
        |> Dict.toList


avanceDeMallaCurricular : MallaCurricular -> Dict String (List MateriaCursada) -> List AvanceDeMallaCurricular
avanceDeMallaCurricular { obligatorias, optativas, libres } materiasCursadas =
    List.concat
        [ List.indexedMap (avanceSemestral materiasCursadas) obligatorias
        , [ { etiqueta = "Optativas"
            , materias =
                materiasCursadas
                    |> Dict.Extra.keepOnly (Set.fromList optativas)
                    |> Dict.toList
            }
          , { etiqueta = "Libres"
            , materias =
                materiasCursadas
                    |> Dict.Extra.keepOnly (Set.fromList libres)
                    |> Dict.toList
            }
          ]
        ]


avanceSemestral : Dict String (List MateriaCursada) -> Int -> List String -> AvanceDeMallaCurricular
avanceSemestral materiasCursadas index materiasDelSemestre =
    { etiqueta = indexToString (index + 1) ++ " Semestre"
    , materias =
        materiasDelSemestre
            |> List.map
                (\materia ->
                    ( materia
                    , materiasCursadas
                        |> Dict.get materia
                        |> Maybe.withDefault []
                    )
                )
    }


indexToString : Int -> String
indexToString index =
    case index of
        1 ->
            "Primer"

        2 ->
            "Segundo"

        3 ->
            "Tercer"

        4 ->
            "Cuarto"

        5 ->
            "Quinto"

        6 ->
            "Sexto"

        7 ->
            "Séptimo"

        8 ->
            "Octavo"

        9 ->
            "Noveno"

        10 ->
            "Décimo"

        _ ->
            String.fromInt index ++ "°"


organizarKardexPorNombre : List PeriodoCursado -> Dict String (List MateriaCursada)
organizarKardexPorNombre kardex =
    organizarMateriasCurzadasPorNombre
        (kardex |> List.concatMap .materias)
        Dict.empty


organizarMateriasCurzadasPorNombre : List MateriaCursada -> Dict String (List MateriaCursada) -> Dict String (List MateriaCursada)
organizarMateriasCurzadasPorNombre materias acumulador =
    case materias of
        [] ->
            acumulador

        materia :: siguientesMaterias ->
            case materia.asignatura of
                Just nombreDeAsignatura ->
                    addToDictList nombreDeAsignatura materia acumulador
                        |> organizarMateriasCurzadasPorNombre siguientesMaterias

                Nothing ->
                    acumulador
                        |> organizarMateriasCurzadasPorNombre siguientesMaterias


addToDictList : comparable -> value -> Dict comparable (List value) -> Dict comparable (List value)
addToDictList key value dict =
    case Dict.get key dict of
        Just previousKeys ->
            Dict.insert key (previousKeys ++ [ value ]) dict

        _ ->
            Dict.insert key [ value ] dict


leerKardex : Result (List a) Html.Parser.Document -> List PeriodoCursado
leerKardex docResult =
    case docResult of
        Ok { document } ->
            document
                |> Tuple.second
                |> findByClassInNodeList "textoTablasKardex"
                |> List.map leerPeriodo

        Err _ ->
            []


type alias MateriaCursada =
    { asignatura : Maybe String
    , calificacion : Maybe String
    , situacion : Maybe String
    , creditos : Maybe String
    , tipoDeExamen : Maybe String
    }


emptySubject : MateriaCursada
emptySubject =
    MateriaCursada Nothing Nothing Nothing Nothing Nothing


type alias PeriodoCursado =
    { cicloEscolar : Maybe String
    , materias : List MateriaCursada
    }


leerPeriodo : Html.Parser.Node -> PeriodoCursado
leerPeriodo node =
    case node of
        Html.Parser.Element _ _ ((Html.Parser.Element _ _ (titleNode :: nodesAfterTitle)) :: _) ->
            { cicloEscolar = firstText titleNode
            , materias = nodesToSubjects nodesAfterTitle
            }

        _ ->
            PeriodoCursado Nothing []


nodesToSubjects : List Html.Parser.Node -> List MateriaCursada
nodesToSubjects nodesAfterTitle =
    case nodesAfterTitle of
        (Html.Parser.Element _ _ ((Html.Parser.Element _ _ subjectNodes) :: _)) :: _ ->
            subjectNodes
                |> findByElementInNodeList "tbody"
                |> findByElementInNodeList "tr"
                |> List.map tableRowToSubject

        _ ->
            []


tableRowToSubject : Html.Parser.Node -> MateriaCursada
tableRowToSubject tableRow =
    case tableRow of
        Html.Parser.Element _ _ tableCells ->
            case tableCells |> List.map firstText of
                [ asignatura, calificacion, situacion, creditos, tipoDeExamen ] ->
                    { asignatura = asignatura
                    , calificacion = calificacion
                    , situacion = situacion
                    , creditos = creditos
                    , tipoDeExamen = tipoDeExamen
                    }

                _ ->
                    emptySubject

        _ ->
            emptySubject


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


findByElementInNodeList : String -> List Html.Parser.Node -> List Html.Parser.Node
findByElementInNodeList element nodes =
    case nodes of
        [] ->
            []

        node :: nextNodes ->
            findByElementInNode element node ++ findByElementInNodeList element nextNodes


findByElementInNode : String -> Html.Parser.Node -> List Html.Parser.Node
findByElementInNode elementToMatch node =
    case node of
        Html.Parser.Element element _ nodes ->
            if elementToMatch == element then
                [ node ]

            else
                findByElementInNodeList elementToMatch nodes

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
