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
import List.Extra
import Maybe.Extra
import Set
import Task



---- MODEL ----


type alias Model =
    { hover : Bool
    , files : List File
    , kardex : Maybe (List PeriodoCursado)
    , mallaCurricular : Maybe MallaCurricular
    }


init : ( Model, Cmd Msg )
init =
    ( { hover = False
      , files = []
      , kardex = Nothing
      , mallaCurricular = Nothing
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
    | SelectMallaCurricular String


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

        SelectMallaCurricular index ->
            ( { model
                | mallaCurricular =
                    mallasCurriculares
                        |> List.Extra.getAt (index |> String.toInt |> Maybe.withDefault -1)
                        |> Maybe.map Tuple.second
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


opcionMallaCurricular : Int -> ( String, a ) -> Html msg
opcionMallaCurricular index ( nombre, _ ) =
    option
        [ Attributes.value (String.fromInt index) ]
        [ text nombre ]


view : Model -> Html Msg
view model =
    styled div
        [ property "display" "grid"
        , property "grid-template-columns" "1fr"
        , property "justify-items" "center"
        , property "grid-gap" "1rem"
        , padding2 (rem 2) (rem 4)
        ]
        []
        [ styled div
            [ border3 (px 2)
                dashed
                (if model.hover then
                    rgb 23 166 226

                 else
                    rgb 11 14 17
                )
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
            , text (Debug.toString model.files)
            ]
        , styled div
            [ property "display" "grid"
            , property "grid-template-columns" "auto auto"
            , property "align-items" "center"
            , property "grid-gap" "0.5rem"
            ]
            []
            [ label [ Attributes.for "malla-curricular" ] [ text "Seleccionar malla curricular:" ]
            , styled select
                [ width (rem 20)
                , padding2 (rem 0.25) (rem 0.5)
                ]
                [ Attributes.id "malla-curricular"
                , onInput SelectMallaCurricular
                ]
                (option
                    [ Attributes.disabled True
                    , Attributes.selected True
                    , Attributes.value ""
                    ]
                    []
                    :: (mallasCurriculares |> List.indexedMap opcionMallaCurricular)
                )
            ]
        , case ( model.kardex, model.mallaCurricular ) of
            ( Just kardex, Just mallaCurricular ) ->
                let
                    kardexPorNombre =
                        organizarKardexPorNombre kardex
                in
                styled div
                    [ property "justify-self" "stretch" ]
                    []
                    (List.concat
                        [ kardexPorNombre
                            |> avanceDeMallaCurricular mallaCurricular
                            |> List.map mostrarMallaCurricular
                        , [ styled div
                                [ marginBottom (rem 2) ]
                                []
                                [ h2 [] [ text "Otros" ]
                                , p [] [ text "Si una materia se encuentra aquí es porque la malla curricular está incompleta o tiene errores de ortografía. Favor de reportarlo al desarrollador" ]
                                , styled div
                                    [ property "display" "grid"
                                    , property "grid-gap" "1.6rem"
                                    , property "grid-template-columns" "repeat(auto-fit, 12rem)"
                                    , property "justify-content" "center"
                                    , property "align-items" "start"
                                    ]
                                    []
                                    (kardexPorNombre
                                        |> materiasFueraDeLaMallaCurricular mallaCurricular
                                        |> List.map mostrarMateria
                                    )
                                ]
                          ]
                        ]
                    )

            _ ->
                text ""
        ]


mostrarMallaCurricular : AvanceDeMallaCurricular -> Html msg
mostrarMallaCurricular { etiqueta, materias } =
    styled div
        [ marginBottom (rem 2) ]
        []
        [ h2 [] [ text etiqueta ]
        , styled div
            [ property "display" "grid"
            , property "grid-gap" "1.6rem"
            , property "grid-template-columns" "repeat(auto-fit, 12rem)"
            , property "justify-content" "center"
            , property "align-items" "start"
            ]
            []
            (materias |> List.map mostrarMateria)
        ]


mostrarMateria : ( String, List MateriaCursada ) -> Html msg
mostrarMateria ( nombre, intentos ) =
    styled div
        [ property "display" "grid"
        , border3 (px 1) solid (rgb 11 14 17)
        , borderRadius (px 5)
        , padding (rem 0.5)
        ]
        []
        [ styled div
            [ borderBottom3 (px 1) solid (rgba 56 56 61 0.8)
            , paddingBottom (rem 0.4)
            , marginBottom (rem 0.4)
            ]
            []
            [ text nombre ]
        , if List.length intentos > 0 then
            styled div
                [ borderBottom3 (px 1) solid (rgba 56 56 61 0.8)
                , paddingBottom (rem 0.4)
                , marginBottom (rem 0.4)
                ]
                []
                [ (intentos
                    |> List.length
                    |> String.fromInt
                  )
                    ++ (intentos
                            |> List.length
                            |> foo " intento"
                       )
                    |> text
                ]

          else
            text ""
        , mostrarEstado intentos
        ]


foo : String -> Int -> String
foo word n =
    if n == 1 then
        word

    else
        word ++ "s"


mostrarEstado : List MateriaCursada -> Html msg
mostrarEstado intentos =
    case List.Extra.last intentos of
        Just { situacion } ->
            styled div
                [ backgroundColor (colorDeSituacion situacion intentos)
                ]
                []
                [ situacion
                    |> Maybe.withDefault "???"
                    |> text
                ]

        Nothing ->
            styled div
                [ backgroundColor (rgba 64 64 64 0.4) ]
                []
                [ text "Sin Intentos" ]


colorDeSituacion : Maybe String -> List a -> Color
colorDeSituacion situacion intentos =
    case ( situacion, List.length intentos ) of
        ( Just "No Acreditado", 1 ) ->
            rgba 231 143 12 0.6

        ( Just "No Acreditado", _ ) ->
            rgba 231 12 12 0.6

        _ ->
            rgb 255 255 255


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
    , optativas =
        [ "AWS Academy Cloud Foundations"
        , "CCNA Redes empresariales, seguridad y automatización"
        , "Clean Architecture Principles"
        , "Desarrollo Web Ágil de API y SPA"
        , "Optimización de Aplicaciones Web"
        , "Programación de robots móviles"
        , "Secure Programming"
        ]
    , libres =
        [ "Expresión, Actuación, Comunicación y Creación Escénica"
        , "Formación y Manejo de Equipos de Trabajo"
        , "Herramientas para la Comunicación Científica"
        , "Introducción al Dibujo Artístico"
        ]
    }


mallaCurricularBachilleratoEnLinea : MallaCurricular
mallaCurricularBachilleratoEnLinea =
    { obligatorias =
        [ [ "Desarrollo del lenguaje algebraico"
          , "Estructura y organización de la naturaleza"
          , "Desarrollo del pensamiento científico"
          , "Cultura Maya"
          , "Comprensión lectora y organización de la información"
          , "Beginner 1"
          , "Desarrollo del individuo"
          , "Formación ocupacional 1"
          ]
        , [ "Solución de problemas con ecuaciones"
          , "Dinámica de la naturaleza"
          , "Desarrollo del pensamiento filosófico"
          , "Desarrollo de la argumentación"
          , "Beginner 2"
          , "Responsabilidad social"
          , "Formación ocupacional 2"
          ]
        , [ "Trigonometría y geometría"
          , "Seres vivos y adaptación a su ambiente"
          , "Transformaciones del mundo contemporáneo"
          , "El derecho en la vida ciudadana"
          , "Elaboración de escritos académicos"
          , "Elementary 1"
          , "Formación ocupacional 3"
          ]
        , [ "Desigualdades y funciones algebraicas"
          , "Bioenergética"
          , "Problemas éticos y morales"
          , "Análisis e interpretación literaria 1"
          , "Elementary 2"
          , "Formación ocupacional 4"
          ]
        , [ "Funciones y series matemáticas"
          , "Sistemas biológicos y ambientales"
          , "De la independencia a la globalización en Méxito y Yucatán"
          , "Pre - intermediate 1"
          , "Mi filosofía de vida"
          , "Formación ocupacional 5"
          ]
        , [ "La estadística y la vida"
          , "Diagnóstico e intervención ambiental"
          , "Administración y liderazgo emprendedor"
          , "Trascendencia social"
          , "Formación ocupacional 6"
          ]
        ]
    , optativas =
        [ "Promoción del pensamiento lógico-deductivo"
        , "Sexualidad Humana"
        , "Temas actuales de psicologìa"
        , "Desarrollo del pensamiento lógico-deductivo"
        , "Temas de álgebra"
        , "Reinos eubacteria, archeobacteria, prozoa"
        , "Filosofía práctica"
        , "Apreciación del arte"
        , "Antropología cultural"
        , "Trigonometría y geometrìa analìtica avanzada"
        , "Reino fungi y plantae"
        , "Reino animalia"
        , "Problemas de nutrición"
        , "Introducción a las ciencias políticas"
        , "Ética aplicada"
        , "Historia prehispánica y colonial en Yucatán"
        , "Cálculo diferencial"
        , "Sistema de cuerpo humano 1"
        , "La naturaleza en movimiento"
        , "Desarrollo económico"
        , "Redacción de cuento"
        , "Redacción de novela"
        , "Análisis e interpretación literaria 2"
        , "Lengua maya 1"
        , "Investigación de mercados"
        , "Contribuciones fiscales"
        , "Estructuras de bases de datos"
        , "Algoritmos"
        , "Cálculo integral"
        , "Sistema del cuerpo humano 2"
        , "Química y metabolismo"
        , "Bioquímica de la energía"
        , "Administración financiera"
        , "Redacción de poesía contemporánea"
        , "Inglés técnico"
        , "Lengua maya 2"
        ]
    , libres = []
    }


mallasCurriculares : List ( String, MallaCurricular )
mallasCurriculares =
    [ ( "Licenciatura en Ingeniería de Software", mallaCurricularIngSoftware )
    , ( "Bachillerato en Línea", mallaCurricularBachilleratoEnLinea )
    ]


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
