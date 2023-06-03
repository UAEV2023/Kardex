module Kardex exposing
    ( Attempt
    , Period
    , organizarKardexPorNombre
    , readKardex
    )

import Dict exposing (Dict)
import Html.Parser
import HtmlNodes


type alias Attempt =
    { subjectName : Maybe String
    , grade : Maybe String
    , situation : Maybe String
    , credits : Maybe String
    , examType : Maybe String
    }


type alias Period =
    { periodName : Maybe String
    , attempts : List Attempt
    }


emptySubject : Attempt
emptySubject =
    Attempt Nothing Nothing Nothing Nothing Nothing


readKardex : Result (List a) Html.Parser.Document -> List Period
readKardex docResult =
    case docResult of
        Ok { document } ->
            document
                |> Tuple.second
                |> HtmlNodes.findByClassInNodeList "textoTablasKardex"
                |> List.map readPeriod

        Err _ ->
            []


tableRowToAttempt : Html.Parser.Node -> Attempt
tableRowToAttempt tableRow =
    case tableRow of
        Html.Parser.Element _ _ tableCells ->
            case tableCells |> List.map HtmlNodes.firstText of
                [ subjectName, grade, situation, credits, examType ] ->
                    { subjectName = subjectName
                    , grade = grade
                    , situation = situation
                    , credits = credits
                    , examType = examType
                    }

                _ ->
                    emptySubject

        _ ->
            emptySubject


readPeriod : Html.Parser.Node -> Period
readPeriod node =
    case node of
        Html.Parser.Element _ _ ((Html.Parser.Element _ _ (titleNode :: nodesAfterTitle)) :: _) ->
            { periodName = HtmlNodes.firstText titleNode
            , attempts = nodesToAttempts nodesAfterTitle
            }

        _ ->
            Period Nothing []


nodesToAttempts : List Html.Parser.Node -> List Attempt
nodesToAttempts nodesAfterTitle =
    case nodesAfterTitle of
        (Html.Parser.Element _ _ ((Html.Parser.Element _ _ attemptNodes) :: _)) :: _ ->
            attemptNodes
                |> HtmlNodes.findByElementInNodeList "tbody"
                |> HtmlNodes.findByElementInNodeList "tr"
                |> List.map tableRowToAttempt

        _ ->
            []


organizarKardexPorNombre : List Period -> Dict String (List Attempt)
organizarKardexPorNombre kardex =
    getAttemptsPerSubjectName
        (kardex |> List.concatMap .attempts)
        Dict.empty


getAttemptsPerSubjectName : List Attempt -> Dict String (List Attempt) -> Dict String (List Attempt)
getAttemptsPerSubjectName remainingAttempts attemptsBySubjectName =
    case remainingAttempts of
        [] ->
            attemptsBySubjectName

        attempt :: nextAttempts ->
            case attempt.subjectName of
                Just subjectName ->
                    addToDictList subjectName attempt attemptsBySubjectName
                        |> getAttemptsPerSubjectName nextAttempts

                Nothing ->
                    attemptsBySubjectName
                        |> getAttemptsPerSubjectName nextAttempts


addToDictList : comparable -> value -> Dict comparable (List value) -> Dict comparable (List value)
addToDictList key value dict =
    case Dict.get key dict of
        Just previousKeys ->
            Dict.insert key (previousKeys ++ [ value ]) dict

        _ ->
            Dict.insert key [ value ] dict
