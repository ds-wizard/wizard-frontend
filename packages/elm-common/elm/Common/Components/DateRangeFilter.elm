module Common.Components.DateRangeFilter exposing
    ( Dialog
    , Mode
    , apply
    , dialog
    , menuItems
    , summary
    , viewDialogs
    )

{-| A date range filter as a menu and two dialogs, for any filter that keeps a
`Common.Data.DateRange.Value`: a table column's, a log's.

The presets are menu items and apply at once. A rolling window and two
instants are fields, and a menu is the wrong place for fields, so each is a
dialog the menu opens. The owner keeps the open dialog (`Maybe Dialog`) and
renders `viewDialogs` with its page, closed ones included, so opening one is a
change to it rather than a new element.

-}

import Common.Data.Context exposing (WithTime)
import Common.Data.DateRange as DateRange
import Common.Utils.TimeUtils as TimeUtils
import Gettext exposing (gettext)
import Html exposing (Html)
import Iso8601
import Maybe.Extra as Maybe
import Time
import Ui.Field as Field
import Ui.Layout as Layout
import Ui.Menu as Menu
import Ui.Modal as Modal


type alias Dialog =
    { mode : Mode
    , direction : String
    , count : String
    , unit : String
    , from : String
    , to : String
    , submitted : Bool
    }


type Mode
    = RelativeMode
    | CustomMode


{-| The presets, then the two dialogs; the one that stands for the value is
checked.
-}
menuItems :
    WithTime a
    ->
        { value : DateRange.Value
        , onSet : DateRange.Value -> msg
        , onOpen : Mode -> msg
        }
    -> List (Menu.Item msg)
menuItems appState cfg =
    let
        preset value =
            Menu.radioItem (cfg.value.token == Just (DateRange.TokenPreset value))
                (presetLabel appState value)
                (cfg.onSet { token = Just (DateRange.TokenPreset value), from = Nothing, to = Nothing })

        isRelative =
            case cfg.value.token of
                Just (DateRange.TokenRelative _ _ _) ->
                    True

                _ ->
                    False

        isCustom =
            Maybe.isNothing cfg.value.token && not (DateRange.isEmpty cfg.value)
    in
    List.map preset DateRange.presets
        ++ [ Menu.separator
           , Menu.radioItem isRelative (gettext "Relative" appState.locale ++ "…") (cfg.onOpen RelativeMode)
           , Menu.radioItem isCustom (gettext "Custom range" appState.locale ++ "…") (cfg.onOpen CustomMode)
           ]


{-| A dialog opened on the value the filter has.
-}
dialog : Mode -> DateRange.Value -> Dialog
dialog mode value =
    let
        base =
            { mode = mode
            , direction = DateRange.directionToString DateRange.Last
            , count = "7"
            , unit = DateRange.unitToString DateRange.Days
            , from = Maybe.withDefault "" value.from
            , to = Maybe.withDefault "" value.to
            , submitted = False
            }
    in
    case value.token of
        Just (DateRange.TokenRelative direction count unit) ->
            { base
                | direction = DateRange.directionToString direction
                , count = String.fromInt count
                , unit = DateRange.unitToString unit
            }

        _ ->
            base


{-| The value a dialog stands for, or `Nothing` while it is not valid (the
dialog then shows why, once submitted).
-}
apply : Dialog -> Maybe DateRange.Value
apply current =
    case current.mode of
        RelativeMode ->
            relativeToken current
                |> Maybe.map (\token -> { token = Just token, from = Nothing, to = Nothing })

        CustomMode ->
            if Maybe.isNothing (customError current) then
                Just { token = Nothing, from = nonEmpty current.from, to = nonEmpty current.to }

            else
                Nothing


nonEmpty : String -> Maybe String
nonEmpty value =
    if String.isEmpty value then
        Nothing

    else
        Just value


relativeToken : Dialog -> Maybe DateRange.Token
relativeToken current =
    Maybe.map3 DateRange.TokenRelative
        (DateRange.directionFromString current.direction)
        (String.toInt (String.trim current.count)
            |> Maybe.andThen
                (\count ->
                    if count > 0 then
                        Just count

                    else
                        Nothing
                )
        )
        (DateRange.unitFromString current.unit)


type CustomError
    = NoBound
    | Reversed


customError : Dialog -> Maybe CustomError
customError current =
    if String.isEmpty current.from && String.isEmpty current.to then
        Just NoBound

    else
        let
            toPosix =
                Result.toMaybe << Iso8601.toTime
        in
        case ( toPosix current.from, toPosix current.to ) of
            ( Just from, Just to ) ->
                if Time.posixToMillis from >= Time.posixToMillis to then
                    Just Reversed

                else
                    Nothing

            _ ->
                Nothing


{-| What the filter's button says while it is on.
-}
summary : WithTime a -> DateRange.Value -> Maybe String
summary appState value =
    case value.token of
        Just (DateRange.TokenPreset preset) ->
            Just (presetLabel appState preset)

        Just (DateRange.TokenRelative direction count unit) ->
            Just (directionLabel appState direction ++ " " ++ String.fromInt count ++ " " ++ unitLabel appState unit)

        Nothing ->
            let
                bound =
                    Maybe.andThen (Result.toMaybe << Iso8601.toTime)
                        >> Maybe.map (TimeUtils.toReadableDateTime appState.timeZone)
            in
            case ( bound value.from, bound value.to ) of
                ( Nothing, Nothing ) ->
                    Nothing

                ( from, to ) ->
                    Just (Maybe.withDefault "" from ++ " – " ++ Maybe.withDefault "" to)


{-| The two dialogs, the open one (if any) shown.
-}
viewDialogs :
    WithTime a
    ->
        { idPrefix : String
        , title : String
        , dialog : Maybe Dialog
        , onChange : Dialog -> msg
        , onApply : msg
        , onClose : msg
        }
    -> Html msg
viewDialogs appState cfg =
    let
        current =
            Maybe.withDefault (dialog RelativeMode DateRange.empty) cfg.dialog

        isOpen mode =
            Maybe.map .mode cfg.dialog == Just mode

        change =
            cfg.onChange

        relativeError =
            if current.submitted && Maybe.isNothing (relativeToken current) then
                Just (gettext "Enter a whole number greater than zero." appState.locale)

            else
                Nothing

        customMessage =
            if current.submitted || customError current == Just Reversed then
                case customError current of
                    Just Reversed ->
                        Just (gettext "The start of the range must be before its end." appState.locale)

                    Just NoBound ->
                        Just (gettext "Enter at least one of the two." appState.locale)

                    Nothing ->
                        Nothing

            else
                Nothing

        modal mode modalTitle body =
            Modal.plain
                { title = modalTitle
                , open = isOpen mode
                , onClose = cfg.onClose
                , body = body
                }
                |> Modal.withForm { onSubmit = cfg.onApply, submitLabel = gettext "Apply" appState.locale }
                |> Modal.withCancelLabel (gettext "Cancel" appState.locale)
                |> Modal.withId (cfg.idPrefix ++ "-" ++ modeId mode)
                |> Modal.toHtml

        modeId mode =
            case mode of
                RelativeMode ->
                    "relative"

                CustomMode ->
                    "custom"
    in
    Html.div []
        [ modal RelativeMode
            (cfg.title ++ ": " ++ gettext "Relative" appState.locale)
            [ Layout.inline
                [ Field.select
                    { id = cfg.idPrefix ++ "-direction"
                    , label = gettext "Direction" appState.locale
                    , value = current.direction
                    , options = List.map (\direction -> ( DateRange.directionToString direction, directionLabel appState direction )) DateRange.directions
                    , onInput = \value -> change { current | direction = value }
                    }
                    |> Field.toHtml
                , Field.input
                    { id = cfg.idPrefix ++ "-count"
                    , label = gettext "Count" appState.locale
                    , value = current.count
                    , onInput = \value -> change { current | count = value }
                    }
                    |> Field.withInputMode "numeric"
                    |> Field.withError relativeError
                    |> Field.toHtml
                , Field.select
                    { id = cfg.idPrefix ++ "-unit"
                    , label = gettext "Unit" appState.locale
                    , value = current.unit
                    , options = List.map (\unit -> ( DateRange.unitToString unit, unitLabel appState unit )) DateRange.units
                    , onInput = \value -> change { current | unit = value }
                    }
                    |> Field.toHtml
                ]
            ]
        , modal CustomMode
            (cfg.title ++ ": " ++ gettext "Custom range" appState.locale)
            [ Field.dateTime
                { id = cfg.idPrefix ++ "-from"
                , label = gettext "From" appState.locale
                , value = current.from
                , onChange = \value -> change { current | from = value }
                }
                |> Field.toHtml
            , Field.dateTime
                { id = cfg.idPrefix ++ "-to"
                , label = gettext "To" appState.locale
                , value = current.to
                , onChange = \value -> change { current | to = value }
                }
                |> Field.withError customMessage
                |> Field.toHtml
            ]
        ]


presetLabel : WithTime a -> DateRange.Preset -> String
presetLabel appState preset =
    case preset of
        DateRange.Today ->
            gettext "Today" appState.locale

        DateRange.Last7Days ->
            gettext "Last 7 days" appState.locale

        DateRange.Last30Days ->
            gettext "Last 30 days" appState.locale

        DateRange.ThisMonth ->
            gettext "This month" appState.locale

        DateRange.LastMonth ->
            gettext "Last month" appState.locale

        DateRange.ThisYear ->
            gettext "This year" appState.locale


directionLabel : WithTime a -> DateRange.Direction -> String
directionLabel appState direction =
    case direction of
        DateRange.Last ->
            gettext "Last" appState.locale

        DateRange.Next ->
            gettext "Next" appState.locale


unitLabel : WithTime a -> DateRange.Unit -> String
unitLabel appState unit =
    case unit of
        DateRange.Seconds ->
            gettext "seconds" appState.locale

        DateRange.Minutes ->
            gettext "minutes" appState.locale

        DateRange.Hours ->
            gettext "hours" appState.locale

        DateRange.Days ->
            gettext "days" appState.locale

        DateRange.Weeks ->
            gettext "weeks" appState.locale

        DateRange.Months ->
            gettext "months" appState.locale
