module HtmlNodes exposing
    ( findByClassInNodeList
    , findByElementInNodeList
    , firstText
    )

import Html.Parser
import Maybe.Extra


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
