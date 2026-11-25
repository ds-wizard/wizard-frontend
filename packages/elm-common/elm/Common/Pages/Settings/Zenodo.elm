module Common.Pages.Settings.Zenodo exposing
    ( Model
    , Msg
    , init
    , initialModel
    , isGuarded
    , subscriptions
    , update
    , view
    )

{-| The OAuth applications of Zenodo (sandbox and production). Each one is
registered in the developer applications of the user who administers it in
Zenodo, with the redirect URI shown here; its client ID and secret go here.
Users then connect their Zenodo accounts in their profile, and submissions with
a Zenodo target upload to the accounts of the users who submit.
-}

import Common.Api.Models.Settings.ZenodoSettings as ZenodoSettings exposing (ZenodoSettings)
import Common.Components.Form.Field as Field
import Common.Components.SettingsPage as SettingsPage
import Common.Data.AppState as AppState exposing (AppState)
import Common.Data.UpdateConfig exposing (UpdateConfig)
import Common.Pages.Settings.Components.Section as Section
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html)
import String.Format as String
import Ui.Field as UiField
import Ui.Form as UiForm
import Ui.Layout as Layout


type alias Model =
    SettingsPage.Model ZenodoSettings ZenodoSettings


type alias Msg =
    SettingsPage.Msg ZenodoSettings


initialModel : Model
initialModel =
    SettingsPage.initialModel ZenodoSettings.initEmptyForm


init : AppState -> ( Model, Cmd Msg )
init appState =
    SettingsPage.init appState ZenodoSettings.section ZenodoSettings.initEmptyForm


update : UpdateConfig Msg msg -> AppState -> Msg -> Model -> ( Model, Cmd msg )
update cfg appState =
    SettingsPage.update appState
        { wrapMsg = cfg.wrapMsg
        , logoutMsg = cfg.logoutMsg
        , section = ZenodoSettings.section
        , initForm = ZenodoSettings.initForm
        , formToValue = \form _ -> form
        , formValidation = ZenodoSettings.validation
        }


subscriptions : AppState -> Model -> Sub Msg
subscriptions =
    SettingsPage.subscriptions


view : AppState -> Model -> { body : List (Html Msg), footer : Maybe (Html Msg) }
view appState =
    SettingsPage.view appState
        { title = "Zenodo"
        , description = Just (gettext "Connect the instance to Zenodo, so that projects can be submitted to Zenodo with the Zenodo accounts of their users." appState.locale)
        , guideLink = Nothing
        , formView = formView appState
        , wrapMsg = identity
        }


formView : AppState -> Form FormError ZenodoSettings -> Html Msg
formView appState form =
    let
        redirectUri =
            AppState.getClientUrlRoot appState ++ "/users/edit/current/submission-accounts"
    in
    Html.map SettingsPage.formMsg <|
        Layout.stack
            [ UiField.readOnly
                { id = "zenodo-redirect-uri"
                , label = gettext "Redirect URI" appState.locale
                , value = redirectUri
                }
                |> UiField.withHint (gettext "Register an application in the developer applications of the Zenodo account settings (Applications) with this redirect URI, the client type Confidential and the scopes deposit:write and deposit:actions." appState.locale)
                |> UiField.toHtml
            , repositoryFormView appState form "sandbox" "Zenodo Sandbox" "https://sandbox.zenodo.org"
            , repositoryFormView appState form "production" "Zenodo" "https://zenodo.org"
            ]


repositoryFormView : AppState -> Form FormError ZenodoSettings -> String -> String -> String -> Html Form.Msg
repositoryFormView appState form prefix name url =
    let
        field fieldName =
            prefix ++ "." ++ fieldName

        enabled =
            Maybe.withDefault False (Form.getFieldAsBool (field "enabled") form).value

        clientInputs =
            if enabled then
                [ UiForm.nested
                    [ Field.input appState form (field "clientId") (gettext "Client ID" appState.locale)
                        |> Field.toHtml
                    , Field.secret appState form (field "clientSecret") (gettext "Client Secret" appState.locale)
                        |> Field.withHint (gettext "A secret that is set shows as ********; leave it to keep the secret." appState.locale)
                        |> Field.toHtml
                    ]
                ]

            else
                []
    in
    UiForm.section
        { id = "zenodo-" ++ prefix
        , legend = name
        , content =
            (Field.toggle appState form (field "enabled") (gettext "Enabled" appState.locale)
                |> Field.withHint (String.format (gettext "Allow submissions to %s." appState.locale) [ url ])
                |> Field.toHtml
            )
                :: clientInputs
        }


isGuarded : AppState -> Model -> Maybe String
isGuarded appState model =
    Section.guard appState (SettingsPage.isDirty model)
