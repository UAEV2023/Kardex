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
import Kardex
import List.Extra
import Set
import Task



---- MODEL ----


type alias Model =
    { hover : Bool
    , files : List File
    , kardex : Maybe (List Kardex.Period)
    , curriculum : Maybe Curriculum
    }


init : ( Model, Cmd Msg )
init =
    ( { hover = False
      , files = []
      , kardex = Nothing
      , curriculum = Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Pick
    | DragEnter
    | DragLeave
    | GotFiles File (List File)
    | GotKardex (Maybe (List Kardex.Period))
    | SelectCurriculum String


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
                    |> Task.map Kardex.readKardex
                    |> Task.map Just
                )
            )

        GotKardex content ->
            ( { model | kardex = content }
            , Cmd.none
            )

        SelectCurriculum index ->
            ( { model
                | curriculum =
                    curriculums
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


curriculumToHtmlOption : Int -> ( String, a ) -> Html msg
curriculumToHtmlOption index ( nombre, _ ) =
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
            , text
                (model.files
                    |> List.map File.name
                    |> String.join ", "
                )
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
                , onInput SelectCurriculum
                ]
                (option
                    [ Attributes.disabled True
                    , Attributes.selected True
                    , Attributes.value ""
                    ]
                    []
                    :: (curriculums |> List.indexedMap curriculumToHtmlOption)
                )
            ]
        , case ( model.kardex, model.curriculum ) of
            ( Just kardex, Just curriculum ) ->
                let
                    attemptsBySubjectName =
                        Kardex.organizarKardexPorNombre kardex
                in
                styled div
                    [ property "justify-self" "stretch" ]
                    []
                    (List.concat
                        [ attemptsBySubjectName
                            |> getCurriculumProgress curriculum
                            |> List.map showSemesterProgress
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
                                    (attemptsBySubjectName
                                        |> getUnrecognizedSubjects curriculum
                                        |> List.map showSubjectProgress
                                    )
                                ]
                          ]
                        ]
                    )

            _ ->
                text ""
        ]


showSemesterProgress : SemesterProgress -> Html msg
showSemesterProgress { semesterName, semesterProgress } =
    styled div
        [ marginBottom (rem 2) ]
        []
        [ h2 [] [ text semesterName ]
        , styled div
            [ property "display" "grid"
            , property "grid-gap" "1.6rem"
            , property "grid-template-columns" "repeat(auto-fit, 12rem)"
            , property "justify-content" "center"
            , property "align-items" "start"
            ]
            []
            (semesterProgress |> List.map showSubjectProgress)
        ]


showSubjectProgress : ( String, List Kardex.Attempt ) -> Html msg
showSubjectProgress ( nombre, attempts ) =
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
        , if List.length attempts > 0 then
            styled div
                [ borderBottom3 (px 1) solid (rgba 56 56 61 0.8)
                , paddingBottom (rem 0.4)
                , marginBottom (rem 0.4)
                ]
                []
                [ (attempts
                    |> List.length
                    |> String.fromInt
                  )
                    ++ (attempts
                            |> List.length
                            |> foo " intento"
                       )
                    |> text
                ]

          else
            text ""
        , showLastSituation attempts
        ]


foo : String -> Int -> String
foo word n =
    if n == 1 then
        word

    else
        word ++ "s"


showLastSituation : List Kardex.Attempt -> Html msg
showLastSituation attempts =
    case List.Extra.last attempts of
        Just { situation } ->
            styled div
                [ backgroundColor (getSituationColor situation attempts)
                ]
                []
                [ text situation ]

        Nothing ->
            styled div
                [ backgroundColor (rgba 64 64 64 0.4) ]
                []
                [ text "Sin Intentos" ]


getSituationColor : String -> List a -> Color
getSituationColor situation attempts =
    case ( situation, List.length attempts ) of
        ( "No Acreditado", 1 ) ->
            rgba 231 143 12 0.6

        ( "No Acreditado", _ ) ->
            rgba 231 12 12 0.6

        _ ->
            rgb 255 255 255


type alias Curriculum =
    { obligatorias : List (List String)
    , optativas : List String
    , libres : List String
    }


mallaCurricularIngSoftware : Curriculum
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


mallaCurricularBachilleratoEnLinea : Curriculum
mallaCurricularBachilleratoEnLinea =
    { obligatorias =
        [ [ "Desarrollo del Lenguaje Algebraico"
          , "Estructura y Organización de la Naturaleza"
          , "Desarrollo del Pensamiento Científico"
          , "Cultura Maya"
          , "Comprensión Lectora y Organización de la Información"
          , "Beginner 1"
          , "Desarrollo del Individuo"
          , "Formación ocupacional 1"
          ]
        , [ "Solución de Problemas con Ecuaciones"
          , "Dinámica de la Naturaleza"
          , "Desarrollo del Pensamiento Filosófico"
          , "Desarrollo de la Argumentación"
          , "Beginner 2"
          , "Responsabilidad Social"
          , "Formación ocupacional 2"
          ]
        , [ "Trigonometría y Geometría"
          , "Seres Vivos: Adaptación a su Ambiente"
          , "Transformaciones del Mundo Contemporáneo"
          , "El Derecho en la Vida Ciudadana"
          , "Elaboración de Escritos Académicos"
          , "Elementary 1"
          , "Formación ocupacional 3"
          ]
        , [ "Desigualdades y Funciones Algebraicas"
          , "Bioenergética"
          , "Problemas Éticos y Morales"
          , "Análisis e Interpretación Literaria 1"
          , "Elementary 2"
          , "Formación ocupacional 4"
          ]
        , [ "Funciones y Series Matemáticas"
          , "Sistemas Biológicos y Ambientales"
          , "De la Independencia a la Globalización en México y Yucatán"
          , "Pre-Intermediate"
          , "Mi Filosofía de Vida"
          , "Formación ocupacional 5"
          ]
        , [ "La Estadística y La Vida"
          , "Diagnóstico e Intervención Ambiental"
          , "Administración y Liderazgo Emprendedor"
          , "Trascendencia Social"
          , "Formación ocupacional 6"
          ]
        ]
    , optativas =
        -- Nivel 2
        [ "Promoción del Pensamiento Lógico Deductivo"
        , "Sexualidad Humana"
        , "Temas Actuales de Psicología"

        -- Nivel 3
        , "Desarrollo del Pensamiento Lógico Deductivo"
        , "Temas de Algebra"
        , "Reinos Eubacteria, Archeobacteria y Protozoa"
        , "Filosofía Práctica"
        , "Apreciación del Arte"
        , "Antropología Cultural"

        -- Nivel 4
        , "Trigonometría y Geometría Analítica Avanzada"
        , "Reinos Fungi y Plantae"
        , "Reino Animalia"
        , "Problemas de Nutrición"
        , "Introducción a las Ciencias Políticas"
        , "Ética Aplicada"
        , "Historia Prehispánica y Colonial en Yucatán"

        -- Nivel 5
        , "Cálculo Diferencial"
        , "Sistemas del Cuerpo Humano 1"
        , "La Naturaleza en Movimiento"
        , "Desarrollo Económico"
        , "Redacción de Cuento"
        , "Redacción de Novelas"
        , "Análisis e Interpretación Literaria 2"

        -- Nivel 6
        , "Cálculo Integral"
        , "Sistemas del Cuerpo Humano 2"
        , "Química y Metabolismo"
        , "Bioquímica de la Energía"
        , "Administración Financiera"
        , "Redacción de Poesía Contemporánea"
        , "Inglés Técnico"
        ]
    , libres = []
    }


curriculums : List ( String, Curriculum )
curriculums =
    [ ( "Licenciatura en Ingeniería de Software", mallaCurricularIngSoftware )
    , ( "Bachillerato en Línea", mallaCurricularBachilleratoEnLinea )
    , ( "Licenciatura en Literatura Latinoamericana"
      , { obligatorias =
            [ [ "Literatura Española, Siglos XIII al XVI"
              , "Literatura de los Pueblos Originarios de América, Siglos XVI al XVIII"
              , "Semiótica Cultural"
              , "Cultura y Pensamiento del Renacimiento Europeo"
              , "Redacción de Textos Académicos"
              , "Responsabilidad Social Universitaria"
              ]
            , [ "Literatura de la América Virreinal"
              , "Literatura de la Nueva España"
              , "Narratología"
              , "Cultura y Pensamiento de la América Virreinal"
              , "Corrección de Textos"
              , "Cultura Maya"
              ]
            , [ "Literatura Latinoamericana del Siglo XIX"
              , "Literatura Mexicana del Siglo XIX"
              , "Literatura de los Pueblos Originarios de América, Siglos XIX al XXI"
              , "Poéticas Clásicas Occidentales"
              , "Cultura y Pensamiento del Siglo XIX"
              , "Lengua Maya"
              ]
            , [ "Literatura Latinoamericana del Siglo XX"
              , "Literatura Mexicana del Siglo XX"
              , "Literatura Maya Contemporánea"
              , "Literatura y Sociedad"
              , "Cultura y Pensamiento del Siglo XX"
              , "Diseño y Producción de Publicaciones"
              ]
            , [ "Literatura del Siglo XXI"
              , "Literatura Caribeña"
              , "Seminario de Investigación I"
              , "Literatura y Recepción"
              , "Corrientes Actuales de Pensamiento"
              , "Didáctica de la Literatura"
              ]
            , [ "Literatura de Yucatán"
              , "Seminario de Investigación II"
              , "Literatura y Sistemas"
              , "Formación de Públicos Lectores"
              , "Mediación de Aprendizajes"
              ]
            , [ "Seminario de Titulación I"
              , "Cultura Emprendedora"
              ]
            , [ "Seminario de Titulación II"
              , "Prácticas Profesionales"
              ]
            ]
        , optativas = []
        , libres = []
        }
      )
    ]


type alias SemesterProgress =
    { semesterName : String
    , semesterProgress : List ( String, List Kardex.Attempt )
    }


getUnrecognizedSubjects : Curriculum -> Dict String (List Kardex.Attempt) -> List ( String, List Kardex.Attempt )
getUnrecognizedSubjects { obligatorias, optativas, libres } materias =
    materias
        |> Dict.Extra.removeMany (Set.fromList (List.concat [ optativas, libres, List.concat obligatorias ]))
        |> Dict.toList


getCurriculumProgress : Curriculum -> Dict String (List Kardex.Attempt) -> List SemesterProgress
getCurriculumProgress { obligatorias, optativas, libres } totalAttempts =
    List.concat
        [ List.indexedMap (getSemesterProgress totalAttempts) obligatorias
        , [ { semesterName = "Optativas"
            , semesterProgress =
                totalAttempts
                    |> Dict.Extra.keepOnly (Set.fromList optativas)
                    |> Dict.toList
            }
          , { semesterName = "Libres"
            , semesterProgress =
                totalAttempts
                    |> Dict.Extra.keepOnly (Set.fromList libres)
                    |> Dict.toList
            }
          ]
        ]


getSemesterProgress : Dict String (List Kardex.Attempt) -> Int -> List String -> SemesterProgress
getSemesterProgress attemptsPerSubjectName index subjectsInSemester =
    { semesterName = indexToString (index + 1) ++ " Semestre"
    , semesterProgress =
        subjectsInSemester
            |> List.map
                (\materia ->
                    ( materia
                    , attemptsPerSubjectName
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



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view >> toUnstyled
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
