module Kardex exposing
    ( Attempt
    , Period
    , groupAttemptsBySubject
    , readKardex
    )

import Dict exposing (Dict)
import Html.Parser
import HtmlNodes
import Maybe.Extra


type alias Attempt =
    { subjectName : String
    , grade : String
    , situation : String
    , credits : String
    , examType : String
    }


type alias Period =
    { periodName : Maybe String
    , attempts : List Attempt
    }


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


tableRowToAttempt : Html.Parser.Node -> Maybe Attempt
tableRowToAttempt tableRow =
    case tableRow of
        Html.Parser.Element _ _ tableCells ->
            case tableCells |> List.map (HtmlNodes.firstText >> Maybe.withDefault "") of
                [ subjectName, grade, situation, credits, examType ] ->
                    Just
                        { subjectName = subjectName
                        , grade = grade
                        , situation = situation
                        , credits = credits
                        , examType = examType
                        }

                _ ->
                    Nothing

        _ ->
            Nothing


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
                |> Maybe.Extra.values

        _ ->
            []


groupAttemptsBySubject : Dict String (List Attempt) -> List Attempt -> Dict String (List Attempt)
groupAttemptsBySubject attemptsBySubjectName remainingAttempts =
    case remainingAttempts of
        [] ->
            attemptsBySubjectName

        attempt :: nextAttempts ->
            nextAttempts
                |> groupAttemptsBySubject (addToDictList attempt.subjectName attempt attemptsBySubjectName)


addToDictList : comparable -> value -> Dict comparable (List value) -> Dict comparable (List value)
addToDictList key value dict =
    case Dict.get key dict of
        Just previousKeys ->
            Dict.insert key (previousKeys ++ [ value ]) dict

        _ ->
            Dict.insert key [ value ] dict
