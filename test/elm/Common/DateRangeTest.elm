module Common.DateRangeTest exposing
    ( resolveTests
    , tokenTests
    )

import Common.Data.DateRange as DateRange exposing (Direction(..), Preset(..), Token(..), Unit(..))
import Expect
import Iso8601
import Test exposing (Test, describe, test)
import Time


{-| A Wednesday afternoon, so that the flooring the presets do is visible.
-}
now : Time.Posix
now =
    at "2026-03-11T14:37:12.000Z"


at : String -> Time.Posix
at string =
    Result.withDefault (Time.millisToPosix 0) (Iso8601.toTime string)


{-| Two hours ahead of UTC, so that a day does not start where the instant does.
-}
prague : Time.Zone
prague =
    Time.customZone 120 []


tokenTests : Test
tokenTests =
    describe "tokens"
        [ test "a preset survives a save and reload" <|
            \_ ->
                Expect.equal (DateRange.tokenFromString (DateRange.tokenToString (TokenPreset ThisMonth)))
                    (Just (TokenPreset ThisMonth))
        , test "a rolling window survives a save and reload" <|
            \_ ->
                Expect.equal (DateRange.tokenFromString (DateRange.tokenToString (TokenRelative Next 3 Months)))
                    (Just (TokenRelative Next 3 Months))
        , test "an unknown token is not read back" <|
            \_ ->
                Expect.equal (DateRange.tokenFromString "lastFortnight") Nothing
        , test "a window without a positive count is not read back" <|
            \_ ->
                Expect.equal (DateRange.tokenFromString "last:0:days") Nothing
        , test "a window of an unknown unit is not read back" <|
            \_ ->
                Expect.equal (DateRange.tokenFromString "last:2:fortnights") Nothing
        ]


resolveTests : Test
resolveTests =
    describe "resolve"
        [ test "today is the day the user is in, not the day in UTC" <|
            \_ ->
                Expect.equal (DateRange.resolveToken prague now (TokenPreset Today))
                    { from = Just (at "2026-03-10T22:00:00.000Z")
                    , to = Just (at "2026-03-11T22:00:00.000Z")
                    }
        , test "the last 7 days include today in full" <|
            \_ ->
                Expect.equal (DateRange.resolveToken prague now (TokenPreset Last7Days))
                    { from = Just (at "2026-03-04T22:00:00.000Z")
                    , to = Just (at "2026-03-11T22:00:00.000Z")
                    }
        , test "this month runs to the start of the next one" <|
            \_ ->
                Expect.equal (DateRange.resolveToken prague now (TokenPreset ThisMonth))
                    { from = Just (at "2026-02-28T22:00:00.000Z")
                    , to = Just (at "2026-03-31T22:00:00.000Z")
                    }
        , test "last month ends where this month starts" <|
            \_ ->
                Expect.equal (DateRange.resolveToken prague now (TokenPreset LastMonth)).to
                    (DateRange.resolveToken prague now (TokenPreset ThisMonth)).from
        , test "a rolling window ends now rather than at a day boundary" <|
            \_ ->
                Expect.equal (DateRange.resolveToken prague now (TokenRelative Last 90 Minutes))
                    { from = Just (at "2026-03-11T13:07:12.000Z")
                    , to = Just now
                    }
        , test "a window into the future starts now" <|
            \_ ->
                Expect.equal (DateRange.resolveToken prague now (TokenRelative Next 2 Weeks))
                    { from = Just now
                    , to = Just (at "2026-03-25T14:37:12.000Z")
                    }
        ]
