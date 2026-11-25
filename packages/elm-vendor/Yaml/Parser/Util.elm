module Yaml.Parser.Util exposing
    ( blockScalar
    , doubleQuotes
    , indented
    , isColon
    , isComma
    , isListEnd
    , isListStart
    , isNewLine
    , isRecordEnd
    , isRecordStart
    , isSpace
    , multiline
    , neither
    , neither3
    , postProcessFoldedString
    , postProcessString
    , remaining
    , singleQuotes
    , spaces
    , threeDashes
    , threeDots
    , whitespace
    )

import Parser as P exposing ((|.), (|=))
import Regex exposing (Regex)



-- QUESTIONS


{-| -}
isColon : Char -> Bool
isColon =
    is ':'


{-| -}
isComma : Char -> Bool
isComma =
    is ','


{-| -}
isSpace : Char -> Bool
isSpace =
    is ' '


{-| -}
isNewLine : Char -> Bool
isNewLine =
    is '\n'


{-| -}
isListStart : Char -> Bool
isListStart =
    is '['


{-| -}
isListEnd : Char -> Bool
isListEnd =
    is ']'


{-| -}
isRecordStart : Char -> Bool
isRecordStart =
    is '{'


{-| -}
isRecordEnd : Char -> Bool
isRecordEnd =
    is '}'


{-| -}
isSingleQuote : Char -> Bool
isSingleQuote =
    is '\''


{-| -}
isDoubleQuote : Char -> Bool
isDoubleQuote =
    is '"'


{-| -}
neither : (Char -> Bool) -> (Char -> Bool) -> Char -> Bool
neither f1 f2 char =
    not (f1 char) && not (f2 char)


{-| -}
neither3 : (Char -> Bool) -> (Char -> Bool) -> (Char -> Bool) -> Char -> Bool
neither3 f1 f2 f3 char =
    not (f1 char) && not (f2 char) && not (f3 char)


{-| -}
is : Char -> Char -> Bool
is searched char =
    char == searched



--


{-| -}
threeDashes : P.Parser ()
threeDashes =
    P.symbol "---"


{-| -}
threeDots : P.Parser ()
threeDots =
    P.symbol "..."


{-| -}
spaces : P.Parser ()
spaces =
    P.chompWhile isSpace


{-| -}
whitespace : P.Parser ()
whitespace =
    let
        step : a -> P.Parser (P.Step () ())
        step _ =
            P.oneOf
                [ P.succeed (P.Loop ())
                    |. comment
                , P.succeed (P.Loop ())
                    |. P.symbol " "
                , P.succeed (P.Loop ())
                    |. P.symbol "\n"
                , P.succeed (P.Done ())
                ]
    in
    P.loop () step


{-| -}
comment : P.Parser ()
comment =
    -- `P.lineComment` leaves the offset and the row out of step
    -- (elm/parser#53); this stops before the line's end, as it should.
    P.symbol "#"
        |. P.chompWhile (not << isNewLine)



-- STRINGS


{-| -}
multiline : Int -> P.Parser String
multiline indent =
    P.loop [] (multilineStep indent)


multilineStep : Int -> List String -> P.Parser (P.Step (List String) String)
multilineStep indent lines =
    let
        multilineString : List String -> String
        multilineString lines_ =
            String.join "\n" (List.reverse lines_)

        conclusion : String -> Maybe ( Int, Int ) -> P.Step (List String) String
        conclusion line next =
            case next of
                Just ( emptyLineCount, indent_ ) ->
                    if indent_ > indent then
                        P.Loop
                            ((line ++ String.repeat emptyLineCount "\n")
                                :: lines
                            )

                    else
                        P.Done (multilineString (line :: lines))

                Nothing ->
                    P.Done (multilineString (line :: lines))
    in
    P.oneOf
        [ P.succeed conclusion
            |= characters (not << isNewLine)
            |= P.oneOf
                [ P.succeed (\e i -> Just ( e, i ))
                    |. P.chompIf isNewLine
                    |. spaces
                    |= emptyLines
                    |= P.getCol
                , P.succeed Nothing
                    |. P.end
                ]
        , P.succeed (P.Done <| multilineString lines)
        ]


emptyLines : P.Parser Int
emptyLines =
    P.loop 0 emptyLinesStep


emptyLinesStep : Int -> P.Parser (P.Step Int Int)
emptyLinesStep count =
    P.oneOf
        [ P.succeed (P.Loop (count + 1))
            |. P.chompIf isNewLine
            |. spaces
        , P.succeed (P.Done count)
        ]


{-| -}
characters : (Char -> Bool) -> P.Parser String
characters isOk =
    let
        done : List String -> P.Step state String
        done chars =
            chars
                |> List.reverse
                |> String.concat
                |> P.Done

        more : List b -> b -> P.Step (List b) a
        more chars char =
            char
                :: chars
                |> P.Loop

        step : List String -> P.Parser (P.Step (List String) String)
        step chars =
            P.oneOf
                [ -- A comment starts after a space; a `#` within a word is
                  -- part of it.
                  P.succeed (done chars)
                    |. P.symbol " #"
                    |. P.chompWhile (not << isNewLine)
                , P.succeed ()
                    |. P.chompIf isOk
                    |> P.getChompedString
                    |> P.map (more chars)
                , P.succeed (done chars)
                ]
    in
    P.loop [] step


{-| -}
characters_ : (Char -> Bool) -> P.Parser String
characters_ isOk =
    P.succeed ()
        |. P.chompWhile isOk
        |> P.getChompedString


{-| A single-quoted string, in which `''` is a quote.
-}
singleQuotes : P.Parser String
singleQuotes =
    let
        step : List String -> P.Parser (P.Step (List String) String)
        step parts =
            P.oneOf
                [ P.succeed (P.Loop ("'" :: parts))
                    |. P.symbol "''"
                , P.succeed (P.Done (String.concat (List.reverse parts)))
                    |. P.symbol "'"
                , P.succeed (\part -> P.Loop (part :: parts))
                    |= (P.getChompedString (P.chompIf (not << isSingleQuote)) |> P.andThen (\part -> P.succeed part))
                ]
    in
    P.succeed identity
        |. P.symbol "'"
        |= P.loop [] step
        |. spaces


{-| A double-quoted string, with its escapes.
-}
doubleQuotes : P.Parser String
doubleQuotes =
    let
        escaped : P.Parser String
        escaped =
            P.oneOf
                [ P.map (\_ -> "\n") (P.symbol "n")
                , P.map (\_ -> "\t") (P.symbol "t")
                , P.map (\_ -> "\u{000D}") (P.symbol "r")
                , P.map (\_ -> "\"") (P.symbol "\"")
                , P.map (\_ -> "\\") (P.symbol "\\")
                , P.map (\_ -> "/") (P.symbol "/")
                , P.map (\_ -> " ") (P.symbol " ")
                , P.map (\_ -> "\u{0008}") (P.symbol "b")
                , P.map (\_ -> "\u{000C}") (P.symbol "f")
                , P.map (\_ -> "\u{0000}") (P.symbol "0")
                , P.succeed identity
                    |. P.symbol "u"
                    |= unicode
                ]

        unicode : P.Parser String
        unicode =
            P.getChompedString (P.chompIf Char.isHexDigit |. P.chompIf Char.isHexDigit |. P.chompIf Char.isHexDigit |. P.chompIf Char.isHexDigit)
                |> P.map (\hex -> String.fromChar (Char.fromCode (hexToInt hex)))

        step : List String -> P.Parser (P.Step (List String) String)
        step parts =
            P.oneOf
                [ P.succeed (\part -> P.Loop (part :: parts))
                    |. P.symbol "\\"
                    |= escaped
                , P.succeed (P.Done (String.concat (List.reverse parts)))
                    |. P.symbol "\""
                , P.succeed (\part -> P.Loop (part :: parts))
                    |= P.getChompedString (P.chompIf (\char -> not (isDoubleQuote char) && char /= '\\'))
                ]
    in
    P.succeed identity
        |. P.symbol "\""
        |= P.loop [] step
        |. spaces


hexToInt : String -> Int
hexToInt hex =
    String.foldl
        (\char total ->
            total
                * 16
                + (if Char.isDigit char then
                    Char.toCode char - Char.toCode '0'

                   else
                    Char.toCode (Char.toLower char) - Char.toCode 'a' + 10
                  )
        )
        0
        hex


{-| A literal (`|`) or folded (`>`) block scalar, from its indicator: its lines
are those indented more than the key or the dash it belongs to (`indent` is
that column). It chomps with `-` and keeps with `+`. An indentation indicator
(`|2`, `|2-`, `|-2`) says how much further than the key its lines are
indented, for text whose first line starts with spaces; without one, the first
line that is not empty says.
-}
blockScalar : Int -> P.Parser String
blockScalar indent =
    P.succeed (\literal ( chomping, explicit ) -> ( literal, chomping, explicit ))
        |= P.oneOf
            [ P.map (\_ -> True) (P.symbol "|")
            , P.map (\_ -> False) (P.symbol ">")
            ]
        |= P.oneOf
            [ P.succeed (\digit chomping -> ( chomping, Just digit ))
                |= indentationIndicator
                |= chompingIndicator
            , P.succeed (\chomping digit -> ( chomping, digit ))
                |= chompingIndicator
                |= P.oneOf [ P.map Just indentationIndicator, P.succeed Nothing ]
            ]
        |. spaces
        |. P.oneOf [ comment, P.succeed () ]
        |. P.oneOf [ P.symbol "\n", P.end ]
        |> P.backtrackable
        |> P.andThen
            (\( literal, chomping, explicit ) ->
                -- The indicator counts from the indentation of the key, one
                -- less than its column.
                blockScalarBody indent (Maybe.map (\digit -> indent - 1 + digit) explicit) literal chomping
            )


chompingIndicator : P.Parser String
chompingIndicator =
    P.oneOf
        [ P.map (\_ -> "-") (P.symbol "-")
        , P.map (\_ -> "+") (P.symbol "+")
        , P.succeed ""
        ]


indentationIndicator : P.Parser Int
indentationIndicator =
    P.getChompedString (P.chompIf (\char -> char >= '1' && char <= '9'))
        |> P.map (Maybe.withDefault 1 << String.toInt)


blockScalarBody : Int -> Maybe Int -> Bool -> String -> P.Parser String
blockScalarBody indent explicitIndent literal chomping =
    P.succeed Tuple.pair
        |= P.getOffset
        |= P.getSource
        |> P.andThen
            (\( offset, source ) ->
                let
                    lines =
                        String.split "\n" (String.dropLeft offset source)

                    leading line =
                        String.length line - String.length (String.trimLeft line)

                    isBlank line =
                        String.isEmpty (String.trim line)

                    contentIndent =
                        case explicitIndent of
                            Just explicit ->
                                explicit

                            Nothing ->
                                lines
                                    |> List.filter (not << isBlank)
                                    |> List.head
                                    |> Maybe.map leading
                                    |> Maybe.withDefault 0

                    belongs line =
                        isBlank line || (contentIndent >= indent && leading line >= contentIndent)

                    taken =
                        takeWhile belongs lines

                    content =
                        List.map
                            (\line ->
                                if isBlank line then
                                    ""

                                else
                                    String.dropLeft contentIndent line
                            )
                            taken

                    body =
                        dropTrailingBlank content

                    trailing =
                        List.length content - List.length body

                    text =
                        if literal then
                            String.join "\n" body

                        else
                            fold body

                    chomped =
                        if List.isEmpty body then
                            ""

                        else
                            case chomping of
                                "-" ->
                                    text

                                "+" ->
                                    text ++ String.repeat trailing "\n"

                                _ ->
                                    text ++ "\n"

                    target =
                        offset + String.length (String.join "\n" taken)
                in
                chompTo target
                    |> P.map (\_ -> chomped)
            )


takeWhile : (a -> Bool) -> List a -> List a
takeWhile predicate list =
    case list of
        first :: rest ->
            if predicate first then
                first :: takeWhile predicate rest

            else
                []

        [] ->
            []


dropTrailingBlank : List String -> List String
dropTrailingBlank lines =
    List.reverse (dropBlank (List.reverse lines))


dropBlank : List String -> List String
dropBlank lines =
    case lines of
        "" :: rest ->
            dropBlank rest

        _ ->
            lines


{-| Folded lines: those next to each other joined by a space, an empty line a
line break.
-}
fold : List String -> String
fold lines =
    List.foldl
        (\line folded ->
            if String.isEmpty line then
                folded ++ "\n"

            else if String.isEmpty folded || String.endsWith "\n" folded then
                folded ++ line

            else
                folded ++ " " ++ line
        )
        ""
        lines


chompTo : Int -> P.Parser ()
chompTo target =
    let
        step _ =
            P.getOffset
                |> P.andThen
                    (\current ->
                        if current >= target then
                            P.succeed (P.Done ())

                        else
                            P.map P.Loop (P.chompIf (always True))
                    )
    in
    P.loop () step


{-| -}
remaining : P.Parser String
remaining =
    P.getChompedString (chompUntilEndOrBefore "\n...\n")


{-| Chomps up to the first occurrence of a string, or to the end; unlike
`P.chompUntilEndOr`, it keeps the offset and the row in step (elm/parser#53).
-}
chompUntilEndOrBefore : String -> P.Parser ()
chompUntilEndOrBefore str =
    P.succeed Tuple.pair
        |= P.getOffset
        |= P.getSource
        |> P.andThen
            (\( offset, source ) ->
                let
                    rest =
                        String.dropLeft offset source
                in
                chompTo
                    (offset
                        + (case String.indexes str rest of
                            index :: _ ->
                                index

                            [] ->
                                String.length rest
                          )
                    )
            )


postProcessString : String -> String
postProcessString str =
    if isLiteralString str then
        postProcessLiteralString str

    else
        str
            |> String.replace "\n" " "
            |> postProcessFoldedString


postProcessFoldedString : String -> String
postProcessFoldedString str =
    let
        regexFromString : String -> Regex
        regexFromString =
            Regex.fromString >> Maybe.withDefault Regex.never
    in
    str
        |> Regex.replace (regexFromString "\\s\\s+")
            (\match ->
                if String.contains "\n\n" match.match then
                    "\n"

                else
                    " "
            )


isLiteralString : String -> Bool
isLiteralString str =
    str
        |> String.split "\n"
        |> List.head
        |> (==) (Just "|")


postProcessLiteralString : String -> String
postProcessLiteralString str =
    case String.left 2 str of
        "|\n" ->
            let
                content : String
                content =
                    String.dropLeft 2 str
            in
            content
                |> countLeadingSpacesInMultiline
                |> removeLeadingSpaces content

        _ ->
            str


removeLeadingSpaces : String -> Int -> String
removeLeadingSpaces str count =
    str
        |> String.split "\n"
        |> List.map (String.dropLeft count)
        |> String.join "\n"


countLeadingSpacesInMultiline : String -> Int
countLeadingSpacesInMultiline str =
    str
        |> String.split "\n"
        |> List.head
        |> Maybe.withDefault ""
        |> countLeadingSpacesInString


countLeadingSpacesInString : String -> Int
countLeadingSpacesInString str =
    let
        countHelper : String -> Int -> Int
        countHelper s count =
            case String.uncons s of
                Just ( ' ', rest ) ->
                    countHelper rest (count + 1)

                _ ->
                    count
    in
    countHelper str 0



-- INDENT


{-| -}
indented : Int -> { smaller : P.Parser a, exactly : P.Parser a, larger : Int -> P.Parser a, ending : P.Parser a } -> P.Parser a
indented indent next =
    let
        check : Int -> P.Parser a
        check actual =
            P.oneOf
                [ P.andThen (\_ -> next.ending) P.end
                , P.andThen (\_ -> next.ending) (P.symbol "\n...\n")
                , if actual == indent then
                    next.exactly

                  else if actual > indent then
                    next.larger actual

                  else
                    next.smaller
                ]
    in
    P.succeed identity
        |. whitespace
        |= P.getCol
        |> P.andThen check
