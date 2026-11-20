module Common.Components.SavingIndicator exposing
    ( Model
    , Msg
    , init
    , isSaving
    , saveNowButton
    , saved
    , saving
    , subscriptions
    , toHtml
    , update
    )

{-| Where the user's own changes are: on their way to the server, arrived a
moment ago, or all arrived.

Written inline on the project page first; the knowledge model editor needs the
same three states and the same three-second countdown, so it lives here rather
than twice.

**A status, not a control.** It is `role="status"`, so a change is said politely
rather than interrupting, and it is never something to press.

**"Saved" fades, "all saved" stays.** For a few seconds after the last change
lands the indicator says so in words; after that it is a tick with its meaning
in a tooltip and a visually hidden phrase, because a permanent "Saved" beside a
heading reads as a button that failed.

**Trouble outranks the rest.** What the page's outbox says (`Outbox.Status`)
comes first: a change that is late and being sent again ("Not saved,
retrying..."), one that is not getting through ("Changes not saved"), or a
socket that is down with nothing waiting ("Reconnecting..."). Only when there
is none does the indicator say where the changes are. "Changes not saved" is
the one state with something to do about it, and that is a button of its own
beside the indicator (`saveNowButton`), not the indicator itself.

-}

import Common.Api.WebSocket.Outbox as Outbox
import Gettext exposing (gettext)
import Html exposing (Html, span, text)
import Html.Attributes exposing (attribute, class)
import Time
import Ui.Button as Button
import Ui.Icon as Icon
import Ui.TestId as TestId
import Ui.Text as Text
import Ui.Tooltip as Tooltip


type Model
    = Saving
    | SavedRecently Int
    | AllSaved


init : Model
init =
    AllSaved


{-| Something is on its way. A page is in this state from the moment a change is
made — typing still waiting for the debounce is a change not yet sent, and so
not yet saved.
-}
saving : Model
saving =
    Saving


{-| The last change has landed.
-}
saved : Model
saved =
    SavedRecently secondsToSayItFor


{-| True while anything is unsaved, which is what a page's unsaved-changes guard
asks.
-}
isSaving : Model -> Bool
isSaving model =
    model == Saving


secondsToSayItFor : Int
secondsToSayItFor =
    3


type Msg
    = Tick


update : Msg -> Model -> Model
update Tick model =
    case model of
        SavedRecently seconds ->
            if seconds <= 1 then
                AllSaved

            else
                SavedRecently (seconds - 1)

        _ ->
            model


{-| A tick a second, and only while there is a countdown to run down.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        SavedRecently _ ->
            Time.every 1000 (always Tick)

        _ ->
            Sub.none


toHtml : { a | locale : Gettext.Locale } -> Outbox.Status -> Model -> Html msg
toHtml appState outboxStatus model =
    let
        indicator state problem content =
            span
                [ class "c-project-saving"
                , Html.Attributes.classList [ ( "c-project-saving--problem", problem ) ]
                , attribute "role" "status"
                , TestId.attribute "saving-indicator"

                -- What the indicator shows, for a test to wait on without its
                -- words.
                , attribute "data-state" state
                ]
                content
    in
    case outboxStatus of
        Outbox.Stalled ->
            indicator "stalled"
                True
                [ Icon.toHtml (Icon.solid "fa-triangle-exclamation"), text (gettext "Changes not saved" appState.locale) ]

        Outbox.Retrying ->
            indicator "retrying"
                False
                [ Icon.toHtml Icon.spinner, text (gettext "Not saved, retrying..." appState.locale) ]

        Outbox.Reconnecting ->
            indicator "reconnecting"
                False
                [ Icon.toHtml Icon.spinner, text (gettext "Reconnecting..." appState.locale) ]

        _ ->
            case model of
                Saving ->
                    indicator "saving" False [ Icon.toHtml Icon.spinner, text (gettext "Saving..." appState.locale) ]

                SavedRecently _ ->
                    indicator "saved" False [ Icon.toHtml (Icon.solid "fa-check"), text (gettext "Saved" appState.locale) ]

                AllSaved ->
                    indicator "all-saved"
                        False
                        [ Tooltip.view { label = gettext "All changes have been saved" appState.locale, placement = Tooltip.Below }
                            (span [] [ Icon.toHtml (Icon.solid "fa-check") ])
                        , Text.visuallyHiddenInline (gettext "All changes have been saved" appState.locale)
                        ]


{-| Save now, beside the indicator once saving has stalled — and only then:
before that, waiting is what fixes it, and a button offered for every slow
moment teaches nobody anything.
-}
saveNowButton : { a | locale : Gettext.Locale } -> msg -> Outbox.Status -> Maybe (Html msg)
saveNowButton appState msg outboxStatus =
    if outboxStatus == Outbox.Stalled then
        Just
            (Button.secondary (gettext "Save now" appState.locale) msg
                |> Button.withIcon (Icon.solid "fa-rotate")
                |> Button.small
                |> Button.withTestId "saving-indicator_save-now"
                |> Button.toHtml
            )

    else
        Nothing
