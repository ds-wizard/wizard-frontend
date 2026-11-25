module Common.Components.SubmitProjectModal exposing
    ( Model
    , Msg
    , UpdateConfig
    , initialModel
    , open
    , subscriptions
    , update
    , view
    )

{-| Submit a project with one of the submissions the administrator defined:
choose the submission, send it, and follow it until it is done.

The server generates the documents of the submission that are missing and sends
them once they are all done, so after sending the dialog asks for the
submission every two seconds and says what it is doing — generating the
documents, then submitting them — until it is done or failed. Closing the dialog
does not stop the submission; it goes on in the background and the project's
submissions show how it ended.

A form dialog (`Ui.Modal.withForm`): Cmd/Ctrl+Enter sends it, and the submit
button is busy while the request is out. A submission that could not be created
(e.g., the Zenodo account is not connected) is said inside the dialog above the
choice, so it can be sent again.

-}

import ActionResult exposing (ActionResult(..))
import Browser.Navigation as Navigation
import Common.Api.ApiError as ApiError exposing (ApiError)
import Common.Api.Models.AvailableSubmission as AvailableSubmission exposing (AvailableSubmission)
import Common.Api.Models.Document.DocumentState exposing (DocumentState(..))
import Common.Api.Models.OAuthConnection exposing (OAuthAuthorization)
import Common.Api.Models.Submission as Submission exposing (Submission)
import Common.Api.Models.Submission.SubmissionState as SubmissionState
import Common.Api.OAuthConnections as OAuthConnectionsApi
import Common.Api.ProjectSubmissions as ProjectSubmissionsApi
import Common.Components.GuideLink as GuideLink
import Common.Data.AppState as AppState exposing (AppState)
import Common.Data.GuideLinks as GuideLinks
import Common.Data.UuidOrCurrent as UuidOrCurrent
import Common.Routes as Routes
import Common.Utils.RequestHelpers as RequestHelpers
import Gettext exposing (gettext)
import Html exposing (Html)
import Maybe.Extra as Maybe
import String.Format as String
import Task.Extra as Task
import Time
import Ui.Alert as Alert
import Ui.AsyncBlock as AsyncBlock
import Ui.Button as Button
import Ui.Field as Field
import Ui.Layout as Layout
import Ui.Link as Link
import Ui.Modal as Modal
import Ui.Text as Text
import Uuid exposing (Uuid)


type alias Model =
    { projectUuid : Maybe Uuid
    , availableSubmissions : ActionResult (List AvailableSubmission)
    , selectedId : Maybe String
    , submission : ActionResult Submission
    }


initialModel : Model
initialModel =
    { projectUuid = Nothing
    , availableSubmissions = Unset
    , selectedId = Nothing
    , submission = Unset
    }


type Msg
    = Open Uuid
    | Close
    | GetAvailableSubmissionsCompleted (Result ApiError (List AvailableSubmission))
    | Select String
    | Submit
    | SubmitCompleted (Result ApiError Submission)
    | ConnectAndSubmitCompleted (Result ApiError OAuthAuthorization)
    | Poll
    | PollCompleted (Result ApiError Submission)


open : Uuid -> Msg
open =
    Open


type alias UpdateConfig msg =
    { wrapMsg : Msg -> msg
    , logoutMsg : msg
    , submittedMsg : Submission -> msg
    , closeCmd : Cmd msg
    }


update : UpdateConfig msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        Open projectUuid ->
            ( { initialModel | projectUuid = Just projectUuid, availableSubmissions = Loading }
            , ProjectSubmissionsApi.getAvailableSubmissions appState projectUuid (cfg.wrapMsg << GetAvailableSubmissionsCompleted)
            )

        Close ->
            ( initialModel
            , if Maybe.isJust model.projectUuid then
                cfg.closeCmd

              else
                Cmd.none
            )

        GetAvailableSubmissionsCompleted result ->
            RequestHelpers.applyResult
                { setResult =
                    \value record ->
                        { record
                            | availableSubmissions = value
                            , selectedId =
                                value
                                    |> ActionResult.map (Maybe.map .id << List.head)
                                    |> ActionResult.withDefault Nothing
                        }
                , defaultError = gettext "Unable to get the submissions for the project." appState.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , locale = appState.locale
                }

        Select id ->
            ( { model | selectedId = Just id }, Cmd.none )

        Submit ->
            case ( model.projectUuid, model.selectedId, model.submission ) of
                ( _, _, Loading ) ->
                    ( model, Cmd.none )

                ( _, _, Success _ ) ->
                    ( model, Cmd.none )

                ( Just projectUuid, Just definitionId, _ ) ->
                    if mustConnectInProfile model then
                        -- Submitting cannot go on before the account is connected
                        -- in the profile, which the dialog tells.
                        ( model, Cmd.none )

                    else
                        case connectFirst model of
                            Just provider ->
                                -- The service needs an account the user has not
                                -- connected: connect it first, the server submits once
                                -- connected and the user comes back to the documents.
                                ( { model | submission = Loading }
                                , OAuthConnectionsApi.authorize appState
                                    { provider = provider
                                    , returnUrl = Just (Routes.toUrl appState (Routes.projectsDetailDocuments projectUuid))
                                    , pendingSubmission = Just { projectUuid = projectUuid, definitionId = definitionId }
                                    }
                                    (cfg.wrapMsg << ConnectAndSubmitCompleted)
                                )

                            Nothing ->
                                ( { model | submission = Loading }
                                , ProjectSubmissionsApi.postSubmission appState projectUuid definitionId (cfg.wrapMsg << SubmitCompleted)
                                )

                _ ->
                    ( model, Cmd.none )

        SubmitCompleted result ->
            let
                ( newModel, cmd ) =
                    RequestHelpers.applyResult
                        { setResult = \value record -> { record | submission = value }
                        , defaultError = gettext "Unable to submit the project." appState.locale
                        , model = model
                        , result = result
                        , logoutMsg = cfg.logoutMsg
                        , locale = appState.locale
                        }
            in
            case result of
                Ok submission ->
                    ( newModel, Cmd.batch [ cmd, Task.dispatch (cfg.submittedMsg submission) ] )

                Err _ ->
                    ( newModel, cmd )

        ConnectAndSubmitCompleted result ->
            case ( result, model.projectUuid ) of
                ( Ok authorization, Just _ ) ->
                    ( model, Navigation.load authorization.url )

                ( Ok _, Nothing ) ->
                    -- The dialog was closed meanwhile: the submission is cancelled.
                    ( model, Cmd.none )

                ( Err error, _ ) ->
                    ( { model | submission = ApiError.toActionResult appState (gettext "Unable to connect the account." appState.locale) error }
                    , RequestHelpers.getResultCmd cfg.logoutMsg result
                    )

        Poll ->
            case ( model.projectUuid, model.submission ) of
                ( Just projectUuid, Success submission ) ->
                    ( model, ProjectSubmissionsApi.getSubmission appState projectUuid submission.uuid (cfg.wrapMsg << PollCompleted) )

                _ ->
                    ( model, Cmd.none )

        PollCompleted result ->
            case ( result, model.projectUuid, model.submission ) of
                ( Ok submission, Just _, Success followed ) ->
                    if submission.uuid == followed.uuid then
                        ( { model | submission = Success submission }
                        , if submission.state /= SubmissionState.InProgress then
                            Task.dispatch (cfg.submittedMsg submission)

                          else
                            Cmd.none
                        )

                    else
                        -- A late answer for a submission no longer followed.
                        ( model, Cmd.none )

                ( Err _, _, _ ) ->
                    -- A failed poll is retried by the next one.
                    ( model, RequestHelpers.getResultCmd cfg.logoutMsg result )

                _ ->
                    ( model, Cmd.none )


selectedSubmission : Model -> Maybe AvailableSubmission
selectedSubmission model =
    case ( model.availableSubmissions, model.selectedId ) of
        ( Success availableSubmissions, Just id ) ->
            List.head (List.filter ((==) id << .id) availableSubmissions)

        _ ->
            Nothing


{-| The service to connect first: the selected submission needs an account the
user has not connected and connects it when submitting.
-}
connectFirst : Model -> Maybe String
connectFirst model =
    selectedSubmission model
        |> Maybe.andThen
            (\submission ->
                if submission.requiresConnection && submission.connectOnSubmit then
                    submission.oauthProvider

                else
                    Nothing
            )


{-| The selected submission needs an account the user has not connected, and it
is not connected when submitting: the user has to connect it in their profile.
-}
mustConnectInProfile : Model -> Bool
mustConnectInProfile model =
    selectedSubmission model
        |> Maybe.map (\submission -> submission.requiresConnection && not submission.connectOnSubmit)
        |> Maybe.withDefault False


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.submission of
        Success submission ->
            if Maybe.isJust model.projectUuid && submission.state == SubmissionState.InProgress then
                Time.every 2000 (always Poll)

            else
                Sub.none

        _ ->
            Sub.none


view : AppState -> Model -> Html Msg
view appState model =
    let
        buttons =
            case ( model.submission, model.availableSubmissions ) of
                ( Success submission, _ ) ->
                    if submission.state == SubmissionState.InProgress then
                        [ Button.primary (gettext "Close" appState.locale) Close ]

                    else
                        [ Button.primary (gettext "Done" appState.locale) Close ]

                ( _, Success (_ :: _) ) ->
                    let
                        submitting =
                            ActionResult.isLoading model.submission
                    in
                    -- Cancel first and the action last, as every other dialog's
                    -- footer has them.
                    [ Button.ghost (gettext "Cancel" appState.locale) Close
                        |> Button.withDisabled submitting
                    , Button.submit
                        (if Maybe.isJust (connectFirst model) then
                            gettext "Connect and submit" appState.locale

                         else
                            gettext "Submit" appState.locale
                        )
                        |> Button.withLoading submitting
                        |> Button.withDisabled (mustConnectInProfile model)
                    ]

                _ ->
                    -- Nothing to submit with: closing is all there is to do.
                    [ Button.primary (gettext "Close" appState.locale) Close ]

        content =
            case model.submission of
                Success submission ->
                    viewProgress appState submission

                _ ->
                    viewAvailableSubmissions appState model
    in
    Modal.plain
        { title = gettext "Submit project" appState.locale
        , open = Maybe.isJust model.projectUuid
        , onClose = Close
        , body = [ content ]
        }
        |> Modal.withForm { onSubmit = Submit, submitLabel = gettext "Submit" appState.locale }
        |> Modal.withActions buttons
        |> Modal.withCancelLabel (gettext "Cancel" appState.locale)
        |> Modal.withHeaderAction (GuideLink.button appState (AppState.toServerInfo appState) GuideLinks.projectsDocumentSubmission)
        |> Modal.withTestId "project-submit"
        |> Modal.toHtml


viewAvailableSubmissions : AppState -> Model -> Html Msg
viewAvailableSubmissions appState model =
    case model.availableSubmissions of
        Success [] ->
            Alert.toHtml (Alert.info (gettext "There are no submissions configured for this project." appState.locale))

        Success availableSubmissions ->
            let
                documentsDescription availableSubmission =
                    availableSubmission.exports
                        |> List.map (\export -> export.documentTemplateName ++ " (" ++ export.formatName ++ ")")
                        |> String.join ", "

                targetDescription availableSubmission =
                    if AvailableSubmission.isZenodo availableSubmission then
                        gettext "Uploaded to Zenodo as a draft you publish there." appState.locale

                    else
                        gettext "Sent to an external service." appState.locale

                choiceDescription availableSubmission =
                    String.join " "
                        (List.filter (not << String.isEmpty)
                            [ availableSubmission.description
                            , String.format (gettext "Documents: %s." appState.locale) [ documentsDescription availableSubmission ]
                            , targetDescription availableSubmission
                            ]
                        )

                submissionChoice availableSubmission =
                    Field.choice { value = availableSubmission.id, label = availableSubmission.name }
                        |> Field.withChoiceDescription (choiceDescription availableSubmission)

                errorAlert =
                    case model.submission of
                        Error message ->
                            Alert.toHtml (Alert.danger message)

                        _ ->
                            Html.text ""

                accountsLink =
                    Link.link
                        { label = gettext "Submission accounts" appState.locale
                        , url = Routes.toUrl appState (Routes.usersEditSubmissionAccounts UuidOrCurrent.current)
                        }
                        |> Link.toHtml

                connectionNotice =
                    case selectedSubmission model of
                        Just submission ->
                            if not submission.requiresConnection then
                                if Maybe.isJust submission.oauthProvider then
                                    Layout.inline
                                        [ Text.body (gettext "The documents are sent with your connected account." appState.locale)
                                        , accountsLink
                                        ]

                                else
                                    Html.text ""

                            else if submission.connectOnSubmit then
                                Alert.toHtml (Alert.info (gettext "You have not connected your account of this service yet. You will be asked to connect it first, and the submission continues afterwards." appState.locale))

                            else
                                Layout.stack
                                    [ Alert.toHtml (Alert.warning (gettext "Connect your account of this service in your profile first." appState.locale))
                                    , accountsLink
                                    ]

                        Nothing ->
                            Html.text ""
            in
            Layout.stack
                [ errorAlert
                , Text.body (gettext "The documents of the submission are generated from the current state of the project, unless they already are, and then sent together." appState.locale)
                , Field.radioGroup
                    { id = "project-submission"
                    , label = gettext "Submission" appState.locale
                    , value = Maybe.withDefault "" model.selectedId
                    , options = List.map submissionChoice availableSubmissions
                    , onChange = Select
                    }
                    |> Field.withDisabled (ActionResult.isLoading model.submission)
                    |> Field.toHtml
                , connectionNotice
                ]

        Error message ->
            Alert.toHtml (Alert.danger message)

        _ ->
            loading appState


viewProgress : AppState -> Submission -> Html Msg
viewProgress appState submission =
    case submission.state of
        SubmissionState.InProgress ->
            let
                total =
                    List.length submission.documents

                done =
                    List.length (List.filter ((==) DoneDocumentState << .state) submission.documents)

                message =
                    if done < total then
                        String.format (gettext "Generating documents (%s of %s done)..." appState.locale) [ String.fromInt done, String.fromInt total ]

                    else
                        gettext "Submitting..." appState.locale
            in
            Layout.stack
                [ AsyncBlock.view
                    { loading = True
                    , error = Nothing
                    , loadingLabel = message
                    , content = []
                    }
                , Text.body (gettext "You can close this dialog, the submission continues in the background." appState.locale)
                ]

        SubmissionState.Done ->
            Layout.stack
                [ Alert.toHtml
                    (Alert.success
                        (if Submission.isZenodo submission then
                            gettext "The documents were uploaded to a Zenodo draft. Review and publish it in Zenodo." appState.locale

                         else
                            gettext "The project was successfully submitted." appState.locale
                        )
                    )
                , case submission.location of
                    Just location ->
                        Layout.inline
                            [ Text.body (gettext "You can find it here: " appState.locale)
                            , Link.link { label = location, url = location }
                                |> Link.external
                                |> Link.withExternalNotice (gettext "(opens in a new tab)" appState.locale)
                                |> Link.toHtml
                            ]

                    Nothing ->
                        Html.text ""
                , case submission.returnedData of
                    Just warning ->
                        Alert.toHtml (Alert.warning warning)

                    Nothing ->
                        Html.text ""
                ]

        SubmissionState.Error ->
            Alert.toHtml (Alert.danger (Maybe.withDefault (gettext "The submission failed." appState.locale) submission.returnedData))


loading : AppState -> Html Msg
loading appState =
    AsyncBlock.view
        { loading = True
        , error = Nothing
        , loadingLabel = gettext "Loading..." appState.locale
        , content = []
        }
