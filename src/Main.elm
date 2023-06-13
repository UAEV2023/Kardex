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
      , curriculum = Nothing
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
    | GotKardex Kardex.ParsedHtmlKardex
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
                )
            )

        GotKardex kardex ->
            ( { model | kardex = Just kardex }
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
                    attemptsPerSubject =
                        kardex
                            |> List.concatMap .attempts
                            |> Kardex.groupAttemptsBySubject aliases Dict.empty
                in
                styled div
                    [ property "justify-self" "stretch" ]
                    []
                    (List.concat
                        [ attemptsPerSubject
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
                                    (attemptsPerSubject
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
    { compulsorySubjects : List (List String)
    , optionalSubjects : List String
    , occupationalSubjects : List String
    }


mallaCurricularIngSoftware : Curriculum
mallaCurricularIngSoftware =
    { compulsorySubjects =
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
    , optionalSubjects =
        [ "AWS Academy Cloud Foundations"
        , "CCNA Redes empresariales, seguridad y automatización"
        , "Clean Architecture Principles"
        , "Desarrollo Web Ágil de API y SPA"
        , "Optimización de Aplicaciones Web"
        , "Programación de robots móviles"
        , "Secure Programming"
        ]
    , occupationalSubjects =
        [ "Expresión, Actuación, Comunicación y Creación Escénica"
        , "Formación y Manejo de Equipos de Trabajo"
        , "Herramientas para la Comunicación Científica"
        , "Introducción al Dibujo Artístico"
        ]
    }


mallaCurricularBachilleratoEnLinea : Curriculum
mallaCurricularBachilleratoEnLinea =
    { compulsorySubjects =
        [ [ "Desarrollo del Lenguaje Algebraico"
          , "Estructura y Organización de la Naturaleza"
          , "Desarrollo del Pensamiento Científico"
          , "Cultura Maya"
          , "Comprensión Lectora y Organización de la Información"
          , "Beginner 1"
          , "Desarrollo del Individuo"
          ]
        , [ "Solución de Problemas con Ecuaciones"
          , "Dinámica de la Naturaleza"
          , "Desarrollo del Pensamiento Filosófico"
          , "Desarrollo de la Argumentación"
          , "Beginner 2"
          , "Responsabilidad Social"
          ]
        , [ "Trigonometría y Geometría"
          , "Seres Vivos: Adaptación a su Ambiente"
          , "Transformaciones del Mundo Contemporáneo"
          , "El Derecho en la Vida Ciudadana"
          , "Elaboración de Escritos Académicos"
          , "Elementary 1"
          ]
        , [ "Desigualdades y Funciones Algebraicas"
          , "Bioenergética"
          , "Problemas Éticos y Morales"
          , "Análisis e Interpretación Literaria 1"
          , "Elementary 2"
          ]
        , [ "Funciones y Series Matemáticas"
          , "Sistemas Biológicos y Ambientales"
          , "De la Independencia a la Globalización en México y Yucatán"
          , "Pre-Intermediate"
          , "Mi Filosofía de Vida"
          ]
        , [ "La Estadística y La Vida"
          , "Diagnóstico e Intervención Ambiental"
          , "Administración y Liderazgo Emprendedor"
          , "Trascendencia Social"
          ]
        ]
    , optionalSubjects =
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
    , occupationalSubjects =
        -- Nivel 1
        [ "Formación Ocupacional 1: Diseño Gráfico"
        , "Formación Ocupacional 1: Introducción a la Administración"

        -- Nivel 2
        , "Formación Ocupacional 2: Formación"
        , "Formación Ocupacional 2: Planeación Estratégica"
        , "Formación Ocupacional 2: Fundamentos de Mercadotecnia"

        -- Nivel 3
        , "Formación Ocupacional 3: Diseño WEB"
        , "Formación Ocupacional 3: Fundamentos de Contabilidad"
        , "Formación Ocupacional 3: Administrador de Medios Sociales 1"

        -- Nivel 4
        , "Formación Ocupacional 4: Redes y Comunicaciones"
        , "Formación Ocupacional 4: Administración de Recursos Humanos"
        , "Formación Ocupacional 4: Administrador de Medios Sociales 2"

        -- Nivel 5
        , "Formación Ocupacional 5: Bases de Datos"
        , "Formación Ocupacional 5: Fundamentos de Mercadotecnia"
        , "Formación Ocupacional 5: Administrador de Comunidades Virtuales 1"

        -- Nivel 6
        , "Formación Ocupacional 6: Programación WEB"
        , "Formación Ocupacional 6: Análisis e Interpretación de Estados Financieros"
        ]
    }


aliases : Dict String (List String)
aliases =
    Dict.fromList
        [ ( "Diseño Gráfico", [ "Formación Ocupacional 1: Diseño Gráfico" ] )
        , ( "Introducción a la Administración", [ "Formación Ocupacional 1: Introducción a la Administración" ] )
        , ( "Formación", [ "Formación Ocupacional 2: Formación" ] )
        , ( "Planeación Estratégica", [ "Formación Ocupacional 2: Planeación Estratégica" ] )
        , ( "Fundamentos de Mercadotecnia", [ "Formación Ocupacional 2: Fundamentos de Mercadotecnia" ] )
        , ( "Diseño WEB", [ "Formación Ocupacional 3: Diseño WEB" ] )
        , ( "Fundamentos de Contabilidad", [ "Formación Ocupacional 3: Fundamentos de Contabilidad" ] )
        , ( "Administrador de Medios Sociales 1", [ "Formación Ocupacional 3: Administrador de Medios Sociales 1" ] )
        , ( "Redes y Comunicaciones", [ "Formación Ocupacional 4: Redes y Comunicaciones" ] )
        , ( "Administración de Recursos Humanos", [ "Formación Ocupacional 4: Administración de Recursos Humanos" ] )
        , ( "Administrador de Medios Sociales 2", [ "Formación Ocupacional 4: Administrador de Medios Sociales 2" ] )
        , ( "Bases de Datos", [ "Formación Ocupacional 5: Bases de Datos" ] )
        , ( "Fundamentos de Mercadotecnia", [ "Formación Ocupacional 5: Fundamentos de Mercadotecnia" ] )
        , ( "Administrador de Comunidades Virtuales 1", [ "Formación Ocupacional 5: Administrador de Comunidades Virtuales 1" ] )
        , ( "Programación WEB", [ "Formación Ocupacional 6: Programación WEB" ] )
        , ( "Análisis e Interpretación de Estados Financieros", [ "Formación Ocupacional 6: Análisis e Interpretación de Estados Financieros" ] )
        ]


curriculums : List ( String, Curriculum )
curriculums =
    [ ( "Licenciatura en Ingeniería de Software", mallaCurricularIngSoftware )
    , ( "Bachillerato en Línea", mallaCurricularBachilleratoEnLinea )
    , ( "Licenciatura en Literatura Latinoamericana"
      , { compulsorySubjects =
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
        , optionalSubjects = []
        , occupationalSubjects = []
        }
      )
    ]


type alias SemesterProgress =
    { semesterName : String
    , semesterProgress : List ( String, List Kardex.Attempt )
    }


getUnrecognizedSubjects : Curriculum -> Dict String (List Kardex.Attempt) -> List ( String, List Kardex.Attempt )
getUnrecognizedSubjects { compulsorySubjects, optionalSubjects, occupationalSubjects } materias =
    materias
        |> Dict.Extra.removeMany (Set.fromList (List.concat [ optionalSubjects, occupationalSubjects, List.concat compulsorySubjects ]))
        |> Dict.toList


getCurriculumProgress : Curriculum -> Dict String (List Kardex.Attempt) -> List SemesterProgress
getCurriculumProgress { compulsorySubjects, optionalSubjects, occupationalSubjects } totalAttempts =
    List.concat
        [ List.indexedMap (getSemesterProgress totalAttempts) compulsorySubjects
        , [ { semesterName = "Optativas"
            , semesterProgress =
                totalAttempts
                    |> Dict.Extra.keepOnly (Set.fromList optionalSubjects)
                    |> Dict.toList
            }
          , { semesterName = "Formación ocupacional"
            , semesterProgress =
                totalAttempts
                    |> Dict.Extra.keepOnly (Set.fromList occupationalSubjects)
                    |> Dict.toList
            }
          ]
        ]


getSemesterProgress : Dict String (List Kardex.Attempt) -> Int -> List String -> SemesterProgress
getSemesterProgress attemptsPerSubjectName index subjectsInSemester =
    { semesterName = "Nivel " ++ String.fromInt (index + 1)
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



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view >> toUnstyled
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
