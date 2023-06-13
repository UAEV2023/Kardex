module Kardex exposing
    ( Attempt
    , ParsedHtmlKardex
    , Period
    , groupAttemptsBySubject
    , readKardex
    )

import Dict exposing (Dict)
import Dict.Extra
import Html.Parser
import HtmlNodes
import List.Extra
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


type alias ParsedHtmlKardex =
    { studentName : Maybe String
    , tutorName : Maybe String
    , kardex : List Period
    }


readKardex : Result (List a) Html.Parser.Document -> ParsedHtmlKardex
readKardex docResult =
    case docResult of
        Ok { document } ->
            let
                studentAndTutorNodes =
                    document
                        |> Tuple.second
                        |> HtmlNodes.findByClassInNodeList "icePnlTbSetCnt"
                        |> HtmlNodes.findByClassInNodeList "icePnlGrp"
            in
            { kardex =
                document
                    |> Tuple.second
                    |> HtmlNodes.findByClassInNodeList "textoTablasKardex"
                    |> List.map readPeriod
            , studentName =
                studentAndTutorNodes
                    |> List.Extra.getAt 0
                    |> Maybe.map (HtmlNodes.firstText >> Maybe.withDefault "")
            , tutorName =
                studentAndTutorNodes
                    |> List.Extra.getAt 1
                    |> Maybe.map (HtmlNodes.firstText >> Maybe.withDefault "")
            }

        Err _ ->
            { studentName = Nothing, tutorName = Nothing, kardex = [] }


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


groupAttemptsBySubject : Dict String (List String) -> Dict String (List Attempt) -> List Attempt -> Dict String (List Attempt)
groupAttemptsBySubject aliases attemptsBySubjectName remainingAttempts =
    case remainingAttempts of
        [] ->
            attemptsBySubjectName

        attempt :: nextAttempts ->
            let
                correctAttempt =
                    updateAttemptsWithAlias aliases attempt
            in
            groupAttemptsBySubject
                aliases
                (addToDictList correctAttempt.subjectName correctAttempt attemptsBySubjectName)
                nextAttempts


updateAttemptsWithAlias : Dict String (List String) -> Attempt -> Attempt
updateAttemptsWithAlias aliases attempt =
    case Dict.Extra.find (\_ -> List.member attempt.subjectName) aliases of
        Just ( correctSubjectName, _ ) ->
            { attempt | subjectName = correctSubjectName }

        Nothing ->
            attempt


addToDictList : comparable -> value -> Dict comparable (List value) -> Dict comparable (List value)
addToDictList key value dict =
    case Dict.get key dict of
        Just previousKeys ->
            Dict.insert key (previousKeys ++ [ value ]) dict

        _ ->
            Dict.insert key [ value ] dict
