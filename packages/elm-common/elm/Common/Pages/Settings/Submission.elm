module Common.Pages.Settings.Submission exposing
    ( Model
    , Msg
    , init
    , initialModel
    , isGuarded
    , subscriptions
    , update
    , view
    )

import ActionResult exposing (ActionResult(..))
import Common.Api.ApiError exposing (ApiError)
import Common.Api.DocumentTemplates as DocumentTemplatesApi
import Common.Api.Models.DocumentTemplateAllSuggestion as DocumentTemplateAllSuggestion exposing (DocumentTemplateAllSuggestion)
import Common.Api.Models.Settings.SubmissionSettings as SubmissionSettings exposing (SubmissionSettings)
import Common.Components.Form.Field as Field
import Common.Components.SettingsPage as SettingsPage
import Common.Data.AppState as AppState exposing (AppState)
import Common.Data.GuideLinks as GuideLinks
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.Settings.Components.Section as Section
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.HttpMethod as HttpMethod
import Common.Utils.RequestHelpers as RequestHelpers
import Form exposing (Form)
import Form.Field as FormField
import Gettext exposing (gettext)
import Html exposing (Html)
import List.Extra as List
import String.Format as String
import Ui.Form as UiForm
import Ui.Layout as Layout
import Version


type alias Model =
    { templates : ActionResult (List DocumentTemplateAllSuggestion)
    , genericModel : SettingsPage.Model SubmissionSettings SubmissionSettings
    }


type Msg
    = GenericMsg (SettingsPage.Msg SubmissionSettings)
    | GetTemplatesCompleted (Result ApiError (List DocumentTemplateAllSuggestion))


initialModel : Model
initialModel =
    { templates = Loading
    , genericModel = SettingsPage.initialModel SubmissionSettings.initEmptyForm
    }


init : AppState -> ( Model, Cmd Msg )
init appState =
    let
        ( genericModel, genericCmd ) =
            SettingsPage.init appState SubmissionSettings.section SubmissionSettings.initEmptyForm
    in
    ( { initialModel | genericModel = genericModel }
    , Cmd.batch
        [ DocumentTemplatesApi.getTemplatesAll appState GetTemplatesCompleted
        , Cmd.map GenericMsg genericCmd
        ]
    )


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState msg model =
    case msg of
        GenericMsg genericMsg ->
            case SettingsPage.toFormMsg genericMsg of
                Just (Form.Append "definitions") ->
                    -- A new submission connects the account when submitting by
                    -- default, as a configuration without the field does.
                    let
                        index =
                            List.length (Form.getListIndexes "definitions" model.genericModel.form)

                        connectOnSubmit =
                            Form.Input ("definitions." ++ String.fromInt index ++ ".connectOnSubmit") Form.Checkbox (FormField.Bool True)

                        ( appendedModel, appendCmd ) =
                            updateGeneric cfg appState genericMsg model

                        ( newModel, inputCmd ) =
                            updateGeneric cfg appState (SettingsPage.formMsg connectOnSubmit) appendedModel
                    in
                    ( newModel, Cmd.batch [ appendCmd, inputCmd ] )

                _ ->
                    updateGeneric cfg appState genericMsg model

        GetTemplatesCompleted result ->
            RequestHelpers.applyResult
                { setResult = \templates m -> { m | templates = templates }
                , defaultError = gettext "Unable to get document templates." appState.locale
                , model = model
                , result = result
                , logoutMsg = cfg.logoutMsg
                , locale = appState.locale
                }


updateGeneric : UpdateConfig Msg msg -> AppState -> SettingsPage.Msg SubmissionSettings -> Model -> ( Model, Cmd msg )
updateGeneric cfg appState genericMsg model =
    let
        ( genericModel, cmd ) =
            SettingsPage.update appState
                { wrapMsg = cfg.wrapMsg << GenericMsg
                , logoutMsg = cfg.logoutMsg
                , section = SubmissionSettings.section
                , initForm = SubmissionSettings.initForm
                , formToValue = \form _ -> form
                , formValidation = SubmissionSettings.validation
                }
                genericMsg
                model.genericModel
    in
    ( { model | genericModel = genericModel }, cmd )


subscriptions : AppState -> Model -> Sub Msg
subscriptions appState model =
    Sub.map GenericMsg (SettingsPage.subscriptions appState model.genericModel)


view : AppState -> Model -> { body : List (Html Msg), footer : Maybe (Html Msg) }
view appState model =
    let
        -- The formats are chosen from the document templates, so the form
        -- waits for them as well as for the configuration.
        genericModel_ =
            model.genericModel

        ( templates, genericModel ) =
            case ( model.templates, model.genericModel.config ) of
                ( Success loadedTemplates, _ ) ->
                    ( loadedTemplates, model.genericModel )

                ( Error error, _ ) ->
                    ( [], { genericModel_ | config = Error error } )

                ( _, Error _ ) ->
                    ( [], model.genericModel )

                _ ->
                    ( [], { genericModel_ | config = Loading } )
    in
    SettingsPage.view appState
        { title = gettext "Submissions" appState.locale
        , description = Nothing
        , guideLink = Just (Section.guideLink appState GuideLinks.settingsDocumentSubmission)
        , formView = formView appState templates
        , wrapMsg = GenericMsg
        }
        genericModel


formView : AppState -> List DocumentTemplateAllSuggestion -> Form FormError SubmissionSettings -> Html (SettingsPage.Msg SubmissionSettings)
formView appState templates form =
    let
        enabled =
            Maybe.withDefault False (Form.getFieldAsBool "enabled" form).value

        definitionsInput =
            if enabled then
                [ UiForm.nested
                    [ Field.listWithHint (gettext "Each submission generates the selected documents from a project and sends them together to its target. Project editors choose a submission when they submit a project." appState.locale)
                        appState
                        form
                        { field = "definitions"
                        , label = gettext "Submissions" appState.locale
                        , addLabel = gettext "Add submission" appState.locale
                        , rowLabel = \index -> stringValue form (Field.listItemPath "definitions" index "name")
                        , row = definitionFormView appState templates form
                        }
                    ]
                ]

            else
                []
    in
    Html.map SettingsPage.formMsg <|
        Layout.stack
            ((Field.toggle appState form "enabled" (gettext "Enabled" appState.locale)
                |> Field.withHint (gettext "If enabled, projects can be submitted to external services such as Zenodo or a repository with an HTTP API." appState.locale)
                |> Field.toHtml
             )
                :: definitionsInput
            )


definitionFormView : AppState -> List DocumentTemplateAllSuggestion -> Form FormError SubmissionSettings -> Int -> List (Html Form.Msg)
definitionFormView appState templates form index =
    let
        field =
            Field.listItemPath "definitions" index

        exportsField =
            field "exports"

        targetType =
            stringValue form (field "target.type")

        isZenodo =
            targetType == SubmissionSettings.zenodoTargetType

        targetTypeOptions =
            [ ( "", "--" )
            , ( SubmissionSettings.httpTargetType, gettext "HTTP service" appState.locale )
            , ( SubmissionSettings.zenodoTargetType, "Zenodo" )
            ]

        targetInput =
            if isZenodo then
                [ zenodoTargetFormView appState form (field "target") ]

            else if targetType == SubmissionSettings.httpTargetType then
                [ httpTargetFormView appState form (field "target") ]

            else
                []
    in
    [ Field.input appState form (field "id") (gettext "ID" appState.locale)
        |> Field.toHtml
    , Field.input appState form (field "name") (gettext "Name" appState.locale)
        |> Field.toHtml
    , Field.markdown appState form (field "description") (gettext "Description" appState.locale)
        |> Field.toHtml
    , Field.listWithHint (gettext "The documents generated from the project and submitted together. Documents already generated from the current state of the project are reused." appState.locale)
        appState
        form
        { field = exportsField
        , label = gettext "Documents" appState.locale
        , addLabel = gettext "Add document" appState.locale
        , rowLabel = \exportIndex -> stringValue form (Field.listItemPath exportsField exportIndex "formatName")
        , row = exportFormView appState templates form isZenodo exportsField
        }
    , Field.select appState form (field "target.type") (gettext "Target" appState.locale) targetTypeOptions
        |> Field.toHtml
    ]
        ++ targetInput
        ++ [ Field.checkbox appState form (field "connectOnSubmit") (gettext "Connect the account when submitting" appState.locale)
                |> Field.withHint (gettext "When the target needs an account the user has not connected yet (Zenodo, an HTTP service with OAuth), submitting first asks the user to connect it and then submits. Otherwise, the user is told to connect it in their profile." appState.locale)
                |> Field.toHtml
           ]


httpTargetFormView : AppState -> Form FormError SubmissionSettings -> String -> Html Form.Msg
httpTargetFormView appState form prefix =
    let
        propsField =
            prefix ++ ".props"
    in
    Layout.stack
        [ Field.listWithHintMarkdown (gettext "You can create properties that can be set by each user in their profile. Then, you can use these properties in the request settings. For example, if you want each user to use their own authorization token, you can create a property called `Token` and use it in request headers as `${Token}`. Users will be able to set the token in their profile settings." appState.locale)
            appState
            form
            { field = propsField
            , label = gettext "User Properties" appState.locale
            , addLabel = gettext "Add property" appState.locale
            , rowLabel = \propIndex -> stringValue form (Field.listItemPath propsField propIndex "")
            , row =
                \propIndex ->
                    [ Field.input appState form (Field.listItemPath propsField propIndex "") (gettext "Name" appState.locale)
                        |> Field.toHtml
                    ]
            }
        , requestFormView appState form (prefix ++ ".request")
        , oauthFormView appState form (prefix ++ ".oauth")
        ]


{-| The optional OAuth 2.0 authorization of an HTTP service: each user connects
their account (in their profile, or when submitting) and the request can use
the access token as `${accessToken}`.
-}
oauthFormView : AppState -> Form FormError SubmissionSettings -> String -> Html Form.Msg
oauthFormView appState form prefix =
    let
        field name =
            prefix ++ "." ++ name

        enabled =
            Maybe.withDefault False (Form.getFieldAsBool (field "enabled") form).value

        oauthInputs =
            if enabled then
                [ UiForm.nested
                    [ Field.input appState form (field "authorizeUrl") (gettext "Authorization URL" appState.locale)
                        |> Field.toHtml
                    , Field.input appState form (field "tokenUrl") (gettext "Token URL" appState.locale)
                        |> Field.toHtml
                    , Field.input appState form (field "clientId") (gettext "Client ID" appState.locale)
                        |> Field.toHtml
                    , Field.secret appState form (field "clientSecret") (gettext "Client Secret" appState.locale)
                        |> Field.withHint (gettext "A secret that is set shows as ********; leave it to keep the secret." appState.locale)
                        |> Field.toHtml
                    , Field.input appState form (field "scopes") (gettext "Scopes" appState.locale)
                        |> Field.withHint (gettext "Separated by spaces." appState.locale)
                        |> Field.toHtml
                    ]
                ]

            else
                []
    in
    UiForm.section
        { id = prefix ++ "-oauth"
        , legend = "OAuth"
        , content =
            (Field.toggle appState form (field "enabled") (gettext "Users connect their account with OAuth" appState.locale)
                |> Field.withHintMarkdown (String.format (gettext "The access token of the user is `${accessToken}` in the request, e.g., the header `Authorization` with the value `Bearer ${accessToken}`. Register the application of the service with the redirect URI `%s`." appState.locale) [ AppState.getClientUrlRoot appState ++ "/users/edit/current/submission-accounts" ])
                |> Field.toHtml
            )
                :: oauthInputs
        }


zenodoTargetFormView : AppState -> Form FormError SubmissionSettings -> String -> Html Form.Msg
zenodoTargetFormView appState form prefix =
    let
        field name =
            prefix ++ "." ++ name

        currentRepository =
            stringValue form (field "repository")

        repositories =
            appState.config.submission.zenodoRepositories
                ++ (if String.isEmpty currentRepository || List.member currentRepository appState.config.submission.zenodoRepositories then
                        []

                    else
                        [ currentRepository ]
                   )

        repositoryOptions =
            ( "", "--" ) :: List.map (\repository -> ( repository, SubmissionSettings.zenodoRepositoryName repository )) repositories

        repositoryHint =
            if List.isEmpty appState.config.submission.zenodoRepositories then
                Field.withHint (gettext "No Zenodo repository is configured yet. Configure the OAuth application of Zenodo in the Zenodo settings first." appState.locale)

            else
                Field.withHint (gettext "Users connect their Zenodo account in their profile; the documents are uploaded as an unpublished draft they publish in Zenodo." appState.locale)
    in
    UiForm.section
        { id = prefix ++ "-zenodo"
        , legend = "Zenodo"
        , content =
            [ Field.select appState form (field "repository") (gettext "Repository" appState.locale) repositoryOptions
                |> repositoryHint
                |> Field.toHtml
            , Field.select appState form (field "resourceType") (gettext "Resource Type" appState.locale) SubmissionSettings.resourceTypeOptions
                |> Field.toHtml
            , Layout.grid
                [ Field.input appState form (field "licenseId") (gettext "License" appState.locale)
                    |> Field.withHint (gettext "The identifier of the license in Zenodo, e.g., cc-by-4.0." appState.locale)
                    |> Field.toHtml
                , Field.select appState form (field "accessLevel") (gettext "Access" appState.locale) SubmissionSettings.accessLevelOptions
                    |> Field.toHtml
                ]
            , Field.input appState form (field "community") (gettext "Community" appState.locale)
                |> Field.withHint (gettext "Optional identifier of a Zenodo community; the draft is added to it as a community submission (not submitted for review)." appState.locale)
                |> Field.toHtml
            ]
        }


{-| A document of a submission: a document template, one of its versions, and
one of that version's formats by name. Each is chosen from what the one before
it leaves.
-}
exportFormView : AppState -> List DocumentTemplateAllSuggestion -> Form FormError SubmissionSettings -> Bool -> String -> Int -> List (Html Form.Msg)
exportFormView appState templates form isZenodo prefix index =
    supportedFormatFormView appState templates form prefix index
        ++ (if isZenodo then
                [ Field.checkbox appState form (Field.listItemPath prefix index "madmpMetadata") (gettext "Use as maDMP metadata source" appState.locale)
                    |> Field.withHint (gettext "For an RDA maDMP JSON document: its title, creators, funding and related identifiers are used as the metadata of the Zenodo record." appState.locale)
                    |> Field.toHtml
                ]

            else
                []
           )


supportedFormatFormView : AppState -> List DocumentTemplateAllSuggestion -> Form FormError SubmissionSettings -> String -> Int -> List (Html Form.Msg)
supportedFormatFormView appState templates form prefix index =
    let
        field =
            Field.listItemPath prefix index

        value name =
            Maybe.withDefault "" (Form.getFieldAsString (field name) form).value

        defaultOption =
            ( "", "--" )

        sorted =
            List.sortWith DocumentTemplateAllSuggestion.compare templates

        templateOptions =
            sorted
                |> List.uniqueBy .id
                |> List.map (\template -> ( template.id, template.name ++ " (" ++ template.id ++ ")" ))
                |> (::) defaultOption

        versions =
            List.filter ((==) (value "id") << .id) sorted

        versionOptions =
            versions
                |> List.map (\template -> ( Version.toString template.version, Version.toString template.version ))
                |> List.reverse
                |> (::) defaultOption

        formatOptions =
            versions
                |> List.find ((==) (value "version") << Version.toString << .version)
                |> Maybe.map (.formats >> List.map (\f -> ( f.name, f.name )) >> (::) defaultOption)
                |> Maybe.withDefault [ defaultOption ]
    in
    [ Field.select appState form (field "id") (gettext "Document Template" appState.locale) templateOptions
        |> Field.toHtml
    , Layout.grid
        [ Field.select appState form (field "version") (gettext "Version" appState.locale) versionOptions
            |> Field.toHtml
        , Field.select appState form (field "formatName") (gettext "Format" appState.locale) formatOptions
            |> Field.toHtml
        ]
    ]


requestFormView : AppState -> Form FormError SubmissionSettings -> String -> Html Form.Msg
requestFormView appState form prefix =
    let
        field name =
            prefix ++ "." ++ name

        headersField =
            field "headers"

        multipartEnabled =
            Maybe.withDefault False (Form.getFieldAsBool (field "multipart.enabled") form).value

        multipartFileNameInput =
            if multipartEnabled then
                [ UiForm.nested
                    [ Field.input appState form (field "multipart.fileName") (gettext "Multipart File Name" appState.locale)
                        |> Field.toHtml
                    ]
                ]

            else
                []
    in
    UiForm.section
        { id = prefix ++ "-request"
        , legend = gettext "Request" appState.locale
        , content =
            [ Field.select appState form (field "method") (gettext "Method" appState.locale) HttpMethod.options
                |> Field.toHtml
            , Field.input appState form (field "url") (gettext "URL" appState.locale)
                |> Field.toHtml
            , Field.list appState
                form
                { field = headersField
                , label = gettext "Headers" appState.locale
                , addLabel = gettext "Add header" appState.locale
                , rowLabel = \headerIndex -> stringValue form (Field.listItemPath headersField headerIndex "key")
                , row =
                    \headerIndex ->
                        [ Layout.grid
                            [ Field.input appState form (Field.listItemPath headersField headerIndex "key") (gettext "Header" appState.locale)
                                |> Field.toHtml
                            , Field.input appState form (Field.listItemPath headersField headerIndex "value") (gettext "Value" appState.locale)
                                |> Field.toHtml
                            ]
                        ]
                }
            , Field.toggle appState form (field "multipart.enabled") (gettext "Multipart" appState.locale)
                |> Field.withHint (gettext "If enabled, file will be sent using multipart request. Otherwise, it will be directly in the request body." appState.locale)
                |> Field.toHtml
            ]
                ++ multipartFileNameInput
        }


stringValue : Form FormError SubmissionSettings -> String -> String
stringValue form path =
    Maybe.withDefault "" (Form.getFieldAsString path form).value


isGuarded : AppState -> Model -> Maybe String
isGuarded appState model =
    Section.guard appState (SettingsPage.isDirty model.genericModel)
