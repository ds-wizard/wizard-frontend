module Yaml.Json exposing (decode, encode)

{-| YAML read into JSON and JSON written as YAML: the subset of YAML that is
JSON (mappings, sequences and scalars), with anchors and aliases resolved.

`decode` keeps the order of a mapping's keys, and `encode` writes them in the
order of the JSON object, so a document read and written again keeps its
layout; comments and formatting are not kept. `encode` writes block
collections, a multi-line string as a literal block (`|`), and quotes a
string only where it would otherwise be read as something else.

-}

import Json.Decode as D
import Json.Encode as E
import Regex exposing (Regex)
import Yaml.Parser as Parser
import Yaml.Parser.Ast as Ast


{-| A YAML document as JSON, or what is wrong with it.
-}
decode : String -> Result String E.Value
decode source =
    Parser.fromString source
        |> Result.map toJson


toJson : Ast.Value -> E.Value
toJson value =
    case value of
        Ast.String_ string ->
            E.string string

        Ast.Float_ float ->
            E.float float

        Ast.Int_ int ->
            E.int int

        Ast.List_ list ->
            E.list toJson list

        Ast.Record_ properties ->
            E.object (List.map (Tuple.mapSecond toJson) properties)

        Ast.Bool_ bool ->
            E.bool bool

        Ast.Null_ ->
            E.null

        Ast.Anchor_ _ anchored ->
            toJson anchored

        Ast.Alias_ _ ->
            -- The parser has resolved every alias it has an anchor for.
            E.null



-- ENCODE


type Node
    = NString String
    | NInt Int
    | NFloat Float
    | NBool Bool
    | NNull
    | NList (List Node)
    | NObject (List ( String, Node ))


nodeDecoder : D.Decoder Node
nodeDecoder =
    D.oneOf
        [ D.map NString D.string
        , D.map NInt D.int
        , D.map NFloat D.float
        , D.map NBool D.bool
        , D.null NNull
        , D.map NList (D.list (D.lazy (\_ -> nodeDecoder)))
        , D.map NObject (D.keyValuePairs (D.lazy (\_ -> nodeDecoder)))
        ]


{-| JSON as a YAML document.
-}
encode : E.Value -> String
encode value =
    case D.decodeValue nodeDecoder value of
        Ok node ->
            String.join "\n" (topLines node) ++ "\n"

        Err _ ->
            "null\n"


topLines : Node -> List String
topLines node =
    case node of
        NObject ((_ :: _) as pairs) ->
            List.concatMap (entryLines 0) pairs

        NList ((_ :: _) as items) ->
            List.concatMap (itemLines 0) items

        NString string ->
            if isMultiline string then
                blockLines "" 0 string

            else
                [ scalar node ]

        _ ->
            [ scalar node ]


entryLines : Int -> ( String, Node ) -> List String
entryLines indent ( key, node ) =
    let
        prefix =
            String.repeat indent " " ++ stringScalar key ++ ":"
    in
    case node of
        NObject ((_ :: _) as pairs) ->
            prefix :: List.concatMap (entryLines (indent + 2)) pairs

        NList ((_ :: _) as items) ->
            prefix :: List.concatMap (itemLines (indent + 2)) items

        NString string ->
            if isMultiline string then
                blockLines (prefix ++ " ") (indent + 2) string

            else
                [ prefix ++ " " ++ scalar node ]

        _ ->
            [ prefix ++ " " ++ scalar node ]


itemLines : Int -> Node -> List String
itemLines indent node =
    let
        dash =
            String.repeat indent " " ++ "- "

        -- The first line of a nested collection, written two further in,
        -- starts after the dash instead.
        afterDash lines =
            case lines of
                first :: rest ->
                    (dash ++ String.dropLeft (indent + 2) first) :: rest

                [] ->
                    []
    in
    case node of
        NObject ((_ :: _) as pairs) ->
            afterDash (List.concatMap (entryLines (indent + 2)) pairs)

        NList ((_ :: _) as items) ->
            afterDash (List.concatMap (itemLines (indent + 2)) items)

        NString string ->
            if isMultiline string then
                blockLines dash (indent + 2) string

            else
                [ dash ++ scalar node ]

        _ ->
            [ dash ++ scalar node ]


scalar : Node -> String
scalar node =
    case node of
        NString string ->
            stringScalar string

        NInt int ->
            String.fromInt int

        NFloat float ->
            String.fromFloat float

        NBool True ->
            "true"

        NBool False ->
            "false"

        NNull ->
            "null"

        NList _ ->
            "[]"

        NObject _ ->
            "{}"



-- STRINGS


{-| A string that is written as a literal block: one with a line break that a
block can hold as it is. One that ends in more than one line break is quoted
instead.
-}
isMultiline : String -> Bool
isMultiline string =
    String.contains "\n" (String.trimRight string)
        && not (String.endsWith "\n\n" string)
        && not (String.contains "\u{000D}" string)


blockLines : String -> Int -> String -> List String
blockLines prefix indent string =
    let
        -- A first line that starts with spaces would set the block's
        -- indentation itself; the indicator says it is two, as everything here
        -- is indented by two.
        indicator =
            if String.startsWith " " string then
                "|2"

            else
                "|"

        ( header, body ) =
            if String.endsWith "\n" string then
                ( indicator, String.dropRight 1 string )

            else
                ( indicator ++ "-", string )

        line content =
            if String.isEmpty content then
                ""

            else
                String.repeat indent " " ++ content
    in
    (prefix ++ header) :: List.map line (String.lines body)


stringScalar : String -> String
stringScalar string =
    if isPlain string then
        string

    else if String.any (\char -> char == '\n' || char == '\t' || char == '\u{000D}') string then
        E.encode 0 (E.string string)

    else
        "'" ++ String.replace "'" "''" string ++ "'"


{-| A string that reads back as itself without quotes.
-}
isPlain : String -> Bool
isPlain string =
    case String.uncons string of
        Just ( first, _ ) ->
            not (String.contains (String.fromChar first) "-?:,[]{}#&*!|>'\"%@` ")
                && not (String.endsWith " " string)
                && not (String.endsWith ":" string)
                && not (String.contains ": " string)
                && not (String.contains " #" string)
                && not (String.any (\char -> char == '\n' || char == '\t' || char == '\u{000D}') string)
                && not (Regex.contains reserved string)

        Nothing ->
            False


{-| What YAML reads as a null, a boolean or a number.
-}
reserved : Regex
reserved =
    Regex.fromStringWith { caseInsensitive = True, multiline = False }
        "^(~|null|true|false|yes|no|on|off|y|n|[-+]?(\\d[\\d_]*)?\\.?\\d*([eE][-+]?\\d+)?|[-+]?\\.(inf|nan)|0x[0-9a-f]+|0o[0-7]+)$"
        |> Maybe.withDefault Regex.never
