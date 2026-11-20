module Common.Data.DateRange exposing
    ( Direction(..)
    , Preset(..)
    , Range
    , Token(..)
    , Unit(..)
    , Value
    , directionFromString
    , directionToString
    , directions
    , empty
    , filterKeys
    , fromKey
    , isEmpty
    , presets
    , read
    , resolve
    , resolveToken
    , toKey
    , tokenFromString
    , tokenKey
    , tokenToString
    , unitFromString
    , unitToString
    , units
    , write
    )

{-| The value side of a date range filter, wherever its filters are kept: a
listing stores it under three keys named after the filter.

A range is two independent bounds, either of which may be missing, and it is
stored in one of two ways. An absolute range keeps the two instants it stands
for. A relative one - a preset such as "this month", or a rolling window such as
"last 7 days" - keeps only a token, and is expanded into instants right before
each request, so that a saved view keeps meaning what it said instead of
freezing into the day it was saved on.

Bounds are half open, `[from, to)`, so that adjacent ranges tile without
counting anything twice.

-}

import Iso8601
import List.Extra as List
import Time
import Time.Extra as TimeExtra


{-| Bounds of a resolved range. Both are optional: an absolute range may leave
either end open, which is how "after X" and "before Y" are expressed.
-}
type alias Range =
    { from : Maybe Time.Posix
    , to : Maybe Time.Posix
    }


{-| What a relative range is stored as.
-}
type Token
    = TokenPreset Preset
    | TokenRelative Direction Int Unit


{-| Named ranges anchored to the calendar in the browser's time zone.
-}
type Preset
    = Today
    | Last7Days
    | Last30Days
    | ThisMonth
    | LastMonth
    | ThisYear


{-| Which side of now a rolling window falls on.
-}
type Direction
    = Last
    | Next


type Unit
    = Seconds
    | Minutes
    | Hours
    | Days
    | Weeks
    | Months



-- FILTER KEYS


{-| Token of a relative range. Named after the filter, as every other filter
parameter is.
-}
tokenKey : String -> String
tokenKey filterId =
    filterId ++ "Preset"


fromKey : String -> String
fromKey filterId =
    filterId ++ "From"


toKey : String -> String
toKey filterId =
    filterId ++ "To"


{-| Everything a date range filter writes, so that clearing it and asking
whether it is active need no special case.
-}
filterKeys : String -> List String
filterKeys filterId =
    [ tokenKey filterId, fromKey filterId, toKey filterId ]



-- THE STORED VALUE


{-| What a filter stores: a relative range as its token, or an absolute one as
its bounds (ISO 8601), never both. All empty, the filter is off.
-}
type alias Value =
    { token : Maybe Token
    , from : Maybe String
    , to : Maybe String
    }


empty : Value
empty =
    { token = Nothing, from = Nothing, to = Nothing }


isEmpty : Value -> Bool
isEmpty value =
    value == empty


{-| The value a filter has, given how its keys are read.
-}
read : (String -> Maybe String) -> String -> Value
read getValue filterId =
    case Maybe.andThen tokenFromString (getValue (tokenKey filterId)) of
        Just token ->
            { empty | token = Just token }

        Nothing ->
            { token = Nothing
            , from = nonEmpty (getValue (fromKey filterId))
            , to = nonEmpty (getValue (toKey filterId))
            }


{-| The keys a value is stored under, `Nothing` for a key it leaves out (an
open end of a range, or the token of an absolute one).
-}
write : String -> Value -> List ( String, Maybe String )
write filterId value =
    [ ( tokenKey filterId, Maybe.map tokenToString value.token )
    , ( fromKey filterId, nonEmpty value.from )
    , ( toKey filterId, nonEmpty value.to )
    ]


nonEmpty : Maybe String -> Maybe String
nonEmpty =
    Maybe.andThen
        (\value ->
            if String.isEmpty value then
                Nothing

            else
                Just value
        )


{-| The range a value stands for at this moment.
-}
resolve : Time.Zone -> Time.Posix -> Value -> Range
resolve zone now value =
    case value.token of
        Just token ->
            resolveToken zone now token

        Nothing ->
            { from = Maybe.andThen (Result.toMaybe << Iso8601.toTime) value.from
            , to = Maybe.andThen (Result.toMaybe << Iso8601.toTime) value.to
            }



-- RESOLUTION


resolveToken : Time.Zone -> Time.Posix -> Token -> Range
resolveToken zone now token =
    let
        range from to =
            { from = Just from, to = Just to }
    in
    case token of
        TokenPreset Today ->
            let
                startOfToday =
                    TimeExtra.floor TimeExtra.Day zone now
            in
            range startOfToday (TimeExtra.add TimeExtra.Day 1 zone startOfToday)

        TokenPreset Last7Days ->
            rollingDays zone now 7

        TokenPreset Last30Days ->
            rollingDays zone now 30

        TokenPreset ThisMonth ->
            let
                startOfMonth =
                    TimeExtra.floor TimeExtra.Month zone now
            in
            range startOfMonth (TimeExtra.add TimeExtra.Month 1 zone startOfMonth)

        TokenPreset LastMonth ->
            let
                startOfMonth =
                    TimeExtra.floor TimeExtra.Month zone now
            in
            range (TimeExtra.add TimeExtra.Month -1 zone startOfMonth) startOfMonth

        TokenPreset ThisYear ->
            let
                startOfYear =
                    TimeExtra.floor TimeExtra.Year zone now
            in
            range startOfYear (TimeExtra.add TimeExtra.Year 1 zone startOfYear)

        TokenRelative Last count unit ->
            range (TimeExtra.add (unitToInterval unit) -count zone now) now

        TokenRelative Next count unit ->
            range now (TimeExtra.add (unitToInterval unit) count zone now)


{-| The last `days` whole days, the current one included, so that "last 7 days"
covers a full week rather than six days and a fraction.
-}
rollingDays : Time.Zone -> Time.Posix -> Int -> Range
rollingDays zone now days =
    let
        startOfToday =
            TimeExtra.floor TimeExtra.Day zone now
    in
    { from = Just (TimeExtra.add TimeExtra.Day (1 - days) zone startOfToday)
    , to = Just (TimeExtra.add TimeExtra.Day 1 zone startOfToday)
    }


unitToInterval : Unit -> TimeExtra.Interval
unitToInterval unit =
    case unit of
        Seconds ->
            TimeExtra.Second

        Minutes ->
            TimeExtra.Minute

        Hours ->
            TimeExtra.Hour

        Days ->
            TimeExtra.Day

        Weeks ->
            TimeExtra.Week

        Months ->
            TimeExtra.Month



-- TOKENS


tokenToString : Token -> String
tokenToString token =
    case token of
        TokenPreset preset ->
            presetToString preset

        TokenRelative direction count unit ->
            String.join ":"
                [ directionToString direction
                , String.fromInt count
                , unitToString unit
                ]


tokenFromString : String -> Maybe Token
tokenFromString string =
    case String.split ":" string of
        [ direction, count, unit ] ->
            Maybe.map3 TokenRelative
                (directionFromString direction)
                (Maybe.andThen positive (String.toInt count))
                (unitFromString unit)

        _ ->
            Maybe.map TokenPreset (presetFromString string)


positive : Int -> Maybe Int
positive count =
    if count > 0 then
        Just count

    else
        Nothing


presets : List Preset
presets =
    [ Today, Last7Days, Last30Days, ThisMonth, LastMonth, ThisYear ]


presetToString : Preset -> String
presetToString preset =
    case preset of
        Today ->
            "today"

        Last7Days ->
            "last7Days"

        Last30Days ->
            "last30Days"

        ThisMonth ->
            "thisMonth"

        LastMonth ->
            "lastMonth"

        ThisYear ->
            "thisYear"


presetFromString : String -> Maybe Preset
presetFromString string =
    List.find (\preset -> presetToString preset == string) presets


directions : List Direction
directions =
    [ Last, Next ]


directionToString : Direction -> String
directionToString direction =
    case direction of
        Last ->
            "last"

        Next ->
            "next"


directionFromString : String -> Maybe Direction
directionFromString string =
    List.find (\direction -> directionToString direction == string) directions


units : List Unit
units =
    [ Seconds, Minutes, Hours, Days, Weeks, Months ]


unitToString : Unit -> String
unitToString unit =
    case unit of
        Seconds ->
            "seconds"

        Minutes ->
            "minutes"

        Hours ->
            "hours"

        Days ->
            "days"

        Weeks ->
            "weeks"

        Months ->
            "months"


unitFromString : String -> Maybe Unit
unitFromString string =
    List.find (\unit -> unitToString unit == string) units
