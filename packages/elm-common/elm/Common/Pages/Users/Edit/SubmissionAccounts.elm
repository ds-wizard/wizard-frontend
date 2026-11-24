module Common.Pages.Users.Edit.SubmissionAccounts exposing
    ( Model
    , Msg
    , Params
    , fetchData
    , initModel
    , initialModel
    , update
    , view
    )

{-| The connections of the current user's accounts to the services the
submissions send to with OAuth: the Zenodo repositories the administrator
configured and the HTTP services with OAuth.

Connecting sends the user to the service to authorize the instance; it sends
them back here with a `code` and the `state`, and opening the page with them
completes the connection (they are used once, so the address is then replaced
with the page's own, without them). A connection started by a submission
creates that submission once connected and continues where it was started.

-}

import ActionResult exposing (ActionResult(..))
import Browser.Navigation as Navigation
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.OAuthConnection exposing (OAuthAuthorization, OAuthCallbackResult, OAuthConnection)
import Common.Api.OAuthConnections as OAuthConnectionsApi
import Common.Components.ConfirmModal as ConfirmModal
import Common.Data.AppState exposing (AppState)
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Data.UuidOrCurrent as UuidOrCurrent
import Common.Pages.Users.Components.Section as Section
import Common.Pages.Users.Edit.Routes as UserEditRoute
import Common.Pages.Users.Routes as UsersRoutes
import Common.Routes as Routes
import Common.Utils.RequestHelpers as RequestHelpers
import Dict exposing (Dict)
import Gettext exposing (gettext)
import Html exposing (Html)
import Maybe.Extra as Maybe
import String.Format as String
import Ui.Alert as Alert
import Ui.Badge as Badge
import Ui.Button as Button
import Ui.Icon as Icon
import Ui.Layout as Layout
import Ui.Link as Link
import Ui.Table as Table
import Ui.Text as Text



-- MODEL


type alias Model =
    { connections : ActionResult (List OAuthConnection)
    , completing : ActionResult String
    , continueUrl : Maybe String
    , connecting : Dict String (ActionResult ())
    , connectionToDisconnect : Maybe OAuthConnection
    , disconnecting : ActionResult String
    }


initialModel : Model
initialModel =
    { connections = Loading
    , completing = Unset
    , continueUrl = Nothing
    , connecting = Dict.empty
    , connectionToDisconnect = Nothing
    , disconnecting = Unset
    }


type alias Params =
    { code : Maybe String
    , state : Maybe String
    , error : Maybe String
    }


{-| With a `code` and the `state` from the service, the connection is completed
first; with an `error` from the service (the user denied the access), it is said.
-}
initModel : AppState -> Params -> Model
initModel appState params =
    case ( params.code, params.state, params.error ) of
        ( Just _, Just _, _ ) ->
            { initialModel | completing = Loading }

        ( _, _, Just error ) ->
            { initialModel | completing = Error (String.format (gettext "The service did not authorize the connection (%s)." appState.locale) [ error ]) }

        _ ->
            initialModel


fetchData : AppState -> Params -> Cmd Msg
fetchData appState params =
    case ( params.code, params.state ) of
        ( Just code, Just state ) ->
            OAuthConnectionsApi.completeAuthorization appState { code = code, state = state } CompleteAuthorizationCompleted

        _ ->
            OAuthConnectionsApi.getConnections appState GetConnectionsCompleted



-- UPDATE


type Msg
    = GetConnectionsCompleted (Result ApiError (List OAuthConnection))
    | CompleteAuthorizationCompleted (Result ApiError OAuthCallbackResult)
    | Connect String
    | ConnectCompleted String (Result ApiError OAuthAuthorization)
    | ShowHideDisconnect (Maybe OAuthConnection)
    | Disconnect
    | DisconnectCompleted (Result ApiError ())


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GetConnectionsCompleted result ->
            RequestHelpers.applyResult
                { setResult = \value record -> { record | connections = value }
                , defaultError = gettext "Unable to get the connected accounts." appState.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , locale = appState.locale
                }

        CompleteAuthorizationCompleted result ->
            let
                -- The code and the state are used once: the address no longer
                -- carries them, so that reloading the page does not use them again.
                cleanUrl =
                    Navigation.replaceUrl appState.key
                        (Routes.toUrl appState (Routes.UsersRoute (UsersRoutes.EditRoute UuidOrCurrent.current (UserEditRoute.SubmissionAccounts Nothing Nothing Nothing))))

                reload =
                    OAuthConnectionsApi.getConnections appState (cfg.wrapMsg << GetConnectionsCompleted)
            in
            case result of
                Ok callbackResult ->
                    case ( callbackResult.returnUrl, callbackResult.submissionError ) of
                        ( Just returnUrl, Nothing ) ->
                            -- Started by a submission that is now on its way:
                            -- back to where it was started. The callback address
                            -- is replaced, so going back does not use the code again.
                            ( model, Navigation.replaceUrl appState.key returnUrl )

                        ( mReturnUrl, Just submissionError ) ->
                            ( { model
                                | completing = Error (String.format (gettext "Your account of %s was connected, but the submission could not be created: %s" appState.locale) [ callbackResult.connection.name, submissionError ])
                                , continueUrl = mReturnUrl
                              }
                            , Cmd.batch [ reload, cleanUrl ]
                            )

                        ( Nothing, Nothing ) ->
                            ( { model | completing = Success (String.format (gettext "Your account of %s was connected." appState.locale) [ callbackResult.connection.name ]) }
                            , Cmd.batch [ reload, cleanUrl ]
                            )

                Err error ->
                    ( { model | completing = ApiError.toActionResult appState (gettext "Connecting the account failed." appState.locale) error }
                    , Cmd.batch [ reload, RequestHelpers.getResultCmd cfg.logoutMsg result ]
                    )

        Connect provider ->
            case Dict.get provider model.connecting of
                Just Loading ->
                    ( model, Cmd.none )

                _ ->
                    ( { model | connecting = Dict.insert provider Loading model.connecting }
                    , OAuthConnectionsApi.authorize appState
                        { provider = provider, returnUrl = Nothing, pendingSubmission = Nothing }
                        (cfg.wrapMsg << ConnectCompleted provider)
                    )

        ConnectCompleted provider result ->
            case result of
                Ok authorization ->
                    ( model, Navigation.load authorization.url )

                Err error ->
                    ( { model | connecting = Dict.insert provider (ApiError.toActionResult appState (gettext "Connecting the account failed." appState.locale) error) model.connecting }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        ShowHideDisconnect mbConnection ->
            ( { model | connectionToDisconnect = mbConnection, disconnecting = Unset }, Cmd.none )

        Disconnect ->
            case model.connectionToDisconnect of
                Just connection ->
                    ( { model | disconnecting = Loading }
                    , OAuthConnectionsApi.disconnect appState connection.provider (cfg.wrapMsg << DisconnectCompleted)
                    )

                Nothing ->
                    ( model, Cmd.none )

        DisconnectCompleted result ->
            case result of
                Ok _ ->
                    ( { model | connectionToDisconnect = Nothing, disconnecting = Success (gettext "The account was disconnected." appState.locale) }
                    , OAuthConnectionsApi.getConnections appState (cfg.wrapMsg << GetConnectionsCompleted)
                    )

                Err error ->
                    ( { model | disconnecting = ApiError.toActionResult appState (gettext "Disconnecting the account failed." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )



-- VIEW


view : AppState -> Model -> List (Html Msg)
view appState model =
    Section.view appState
        { title = gettext "Submission Accounts" appState.locale
        , guideLink = Nothing
        , content =
            [ Text.body (gettext "Connect your accounts of the services the projects are submitted to. Submitting with a service you have not connected yet connects it first." appState.locale)
            , resultAlert model.completing
            , continueLink appState model.continueUrl
            , resultAlert model.disconnecting
            ]
                ++ Section.actionResult appState (viewConnections appState model) model.connections
                ++ [ viewDisconnectModal appState model ]
        }


resultAlert : ActionResult String -> Html msg
resultAlert result =
    case result of
        Success message ->
            Alert.toHtml (Alert.success message)

        Error message ->
            Alert.toHtml (Alert.danger message)

        _ ->
            Html.text ""


continueLink : AppState -> Maybe String -> Html msg
continueLink appState mUrl =
    case mUrl of
        Just url ->
            Link.link { label = gettext "Back to the project" appState.locale, url = url }
                |> Link.toHtml

        Nothing ->
            Html.text ""


viewConnections : AppState -> Model -> List OAuthConnection -> List (Html Msg)
viewConnections appState model connections =
    if List.isEmpty connections then
        [ Alert.toHtml (Alert.info (gettext "There are no services to connect an account of." appState.locale)) ]

    else
        let
            status connection =
                if not connection.connected then
                    Badge.toHtml (Badge.neutral (gettext "Not connected" appState.locale))

                else if List.isEmpty connection.missingScopes then
                    Badge.toHtml (Badge.success (gettext "Connected" appState.locale))

                else
                    Badge.toHtml (Badge.warning (gettext "Reconnect needed" appState.locale))

            connectButton connection =
                let
                    label =
                        if connection.connected then
                            gettext "Reconnect" appState.locale

                        else
                            gettext "Connect" appState.locale

                    state =
                        Maybe.withDefault Unset (Dict.get connection.provider model.connecting)
                in
                Button.secondary label (Connect connection.provider)
                    |> Button.withIcon (Icon.solid "fa-link")
                    |> Button.withLoading (ActionResult.isLoading state)
                    |> Button.withAccessibleName (String.format "%s: %s" [ label, connection.name ])

            disconnectButton connection =
                Button.dangerIconOnly (Icon.solid "fa-link-slash") (gettext "Disconnect" appState.locale) (ShowHideDisconnect (Just connection))
                    |> Button.withAccessibleName (String.format "%s: %s" [ gettext "Disconnect" appState.locale, connection.name ])

            actions connection =
                connectButton connection
                    :: (if connection.connected then
                            [ disconnectButton connection ]

                        else
                            []
                       )

            connectErrors =
                Dict.values model.connecting
                    |> List.filterMap
                        (\state ->
                            case state of
                                Error message ->
                                    Just (Alert.toHtml (Alert.danger message))

                                _ ->
                                    Nothing
                        )
        in
        connectErrors
            ++ [ Table.table
                    [ Table.column (gettext "Service" appState.locale) (Html.text << .name)
                    , Table.column (gettext "Status" appState.locale) status
                    , Table.actions (gettext "Actions" appState.locale) actions
                    ]
                    connections
                    |> Table.withRowTestId (\row -> "submission-accounts_row_" ++ row.provider)
                    |> Table.toHtml
               ]


viewDisconnectModal : AppState -> Model -> Html Msg
viewDisconnectModal appState model =
    ConfirmModal.dangerConfirm appState
        { title = gettext "Disconnect Account" appState.locale
        , open = Maybe.isJust model.connectionToDisconnect
        , body =
            [ Text.body (String.format (gettext "Are you sure you want to disconnect your account of %s?" appState.locale) [ Maybe.unwrap "" .name model.connectionToDisconnect ])
            , Text.body (gettext "Submissions to it will ask you to connect it again." appState.locale)
            ]
        , confirmLabel = gettext "Disconnect" appState.locale
        , onConfirm = Disconnect
        , onClose = ShowHideDisconnect Nothing
        , result = model.disconnecting
        , testId = Just "disconnect-submission-account"
        }
