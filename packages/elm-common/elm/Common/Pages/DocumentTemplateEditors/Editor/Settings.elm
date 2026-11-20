module Common.Pages.DocumentTemplateEditors.Editor.Settings exposing
    ( Config
    , formats
    , general
    , knowledgeModels
    , newFormat
    )

{-| The Settings tab: what the template is called, which knowledge models it
works with, and the formats it can be rendered in.

**The three sections are one form** (`DocumentTemplateDraftEditForm`), saved by
the page's single Save together with the files: one button, one state, one
guard. The form is what says a field is wrong — the ID before anything is sent,
and whatever the server refuses after — and the error is shown on the field.

Formats and their steps are lists that can be added to and taken from, in the
order they were made. They do not reorder: `Ui.ListField` has no move, and the
order of formats is not something the server reads.

The fields keep the ids they had before they were in a form, rather than the
ones the form's path would give them, so anything that finds them by id still
does.

-}

import Common.Api.Models.DocumentTemplateDraft.DocumentTemplateFormatDraft exposing (DocumentTemplateFormatDraft)
import Common.Components.Form.Field as FormField
import Common.Data.AppState exposing (AppState)
import Common.Data.Forms.DocumentTemplateDraftEditForm exposing (DocumentTemplateDraftEditForm)
import Common.Utils.Form.FormError exposing (FormError)
import Form exposing (Form)
import Gettext exposing (gettext)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import LanguageCodes
import Ui.Field as Field
import Ui.Icon as Icon
import Ui.Layout as Layout
import Ui.ListField as ListField
import Uuid exposing (Uuid)


type alias Config msg =
    { appState : AppState
    , form : Form FormError DocumentTemplateDraftEditForm
    , toMsg : Form.Msg -> msg

    -- A new format needs a uuid, and only the shell can make one.
    , onAddFormat : msg
    }


general : Config msg -> Html msg
general config =
    let
        locale =
            config.appState.locale
    in
    Layout.stack
        [ input config "dt-settings-name" "name" (gettext "Name" locale)
        , input config "dt-settings-description" "description" (gettext "Description" locale)
        , FormField.input config.appState config.form "id" (gettext "Document Template ID" locale)
            |> FormField.withId "dt-settings-templateId"
            |> FormField.withRequired True
            |> FormField.withHint (gettext "Document template ID can only contain alphanumeric characters, hyphens, underscores, and dots." locale)
            |> FormField.toHtml
            |> Html.map config.toMsg
        , Field.groupToHtml
            (Field.group
                { id = "dt-settings-version"
                , legend = gettext "Version" locale
                , fields =
                    [ versionPart config "major" "versionMajor" (gettext "Major" locale)
                    , versionPart config "minor" "versionMinor" (gettext "Minor" locale)
                    , versionPart config "patch" "versionPatch" (gettext "Patch" locale)
                    ]
                }
                |> Field.withGroupHint (FormField.versionHint config.appState)
            )

        -- The language the template is written in, as a code. The names
        -- are what anybody knows it by, so the list says both.
        , FormField.select config.appState config.form "language" (gettext "Language" locale) LanguageCodes.options
            |> FormField.withId "dt-settings-language"
            |> FormField.toHtml
            |> Html.map config.toMsg
        , input config "dt-settings-license" "license" (gettext "License" locale)
        , FormField.markdown config.appState config.form "readme" (gettext "Readme" locale)
            |> FormField.withId "dt-settings-readme"
            |> FormField.toHtml
            |> Html.map config.toMsg
        ]


{-| One of the three numbers a version is made of.
-}
versionPart : Config msg -> String -> String -> String -> Html msg
versionPart config name path label =
    FormField.number config.appState config.form path label
        |> FormField.withId ("dt-settings-version-" ++ name)
        |> FormField.withRequired True
        |> FormField.withPlaceholder "0"
        |> FormField.toHtml
        |> Html.map config.toMsg



-- WHICH KNOWLEDGE MODELS IT WORKS WITH


knowledgeModels : Config msg -> Html msg
knowledgeModels config =
    let
        locale =
            config.appState.locale

        listPath =
            "allowedPackages"

        row index =
            let
                path =
                    FormField.listItemPath listPath index
            in
            ListField.row
                { label = packageLabel config index (value config (path "id"))
                , content =
                    [ input config ("dt-settings-allowed-km-" ++ String.fromInt index) (path "id") (gettext "Knowledge Model ID" locale)
                    , input config ("dt-settings-allowed-min-" ++ String.fromInt index) (path "minVersion") (gettext "Min Version" locale)
                    , input config ("dt-settings-allowed-max-" ++ String.fromInt index) (path "maxVersion") (gettext "Max Version" locale)
                    ]
                }
                -- Positional, as its fields' ids are: the rows have no key of their own.
                |> ListField.withRowTestId ("dt-settings-allowed-packages_row_" ++ String.fromInt index)
    in
    Layout.stack
        [ ListField.toHtml
            { id = "dt-settings-allowed-packages"
            , label = gettext "Allowed Knowledge Models" locale
            , rows = List.map row (Form.getListIndexes listPath config.form)
            , addLabel = gettext "Add knowledge model" locale
            , onAdd = config.toMsg (Form.Append listPath)
            , onRemove = Just (config.toMsg << Form.RemoveItem listPath)
            , hint = Nothing
            , error = Nothing
            }
        ]


packageLabel : Config msg -> Int -> String -> String
packageLabel config index id =
    if String.isEmpty id then
        String.replace "%s" (String.fromInt (index + 1)) (gettext "Knowledge model %s" config.appState.locale)

    else
        id



-- THE FORMATS IT RENDERS IN


formats : Config msg -> Html msg
formats config =
    let
        locale =
            config.appState.locale

        listPath =
            "formats"

        row index =
            let
                path =
                    FormField.listItemPath listPath index

                icon =
                    value config (path "icon")
            in
            ListField.row
                { label = rowLabel (gettext "Format %s" locale) index (value config (path "name"))
                , content =
                    -- The icon and the name on one line where there is room for
                    -- two, the icon first: it is the narrower of the pair and it
                    -- is what the format is recognized by in a menu. The icon is
                    -- drawn beside its own name as it is typed, as the custom
                    -- menu links are; an empty or unknown one shows the question
                    -- mark `Icon.fromClass` falls back to.
                    [ Layout.grid
                        [ FormField.input config.appState config.form (path "icon") (gettext "Icon" locale)
                            |> FormField.withId ("dt-settings-format-icon-" ++ String.fromInt index)
                            |> FormField.withSuffixIcon (Icon.fromClass icon)
                            |> FormField.toHtml
                            |> Html.map config.toMsg
                        , input config ("dt-settings-format-name-" ++ String.fromInt index) (path "name") (gettext "Name" locale)
                        ]
                    , steps config index
                    ]
                }
                -- Positional, as its fields' ids are: the rows have no key of their own.
                |> ListField.withRowTestId ("dt-settings-format-list_row_" ++ String.fromInt index)
    in
    Layout.stack
        [ ListField.toHtml
            { id = "dt-settings-format-list"
            , label = gettext "Formats" locale
            , rows = List.map row (Form.getListIndexes listPath config.form)
            , addLabel = gettext "Add format" locale
            , onAdd = config.onAddFormat
            , onRemove = Just (config.toMsg << Form.RemoveItem listPath)
            , hint = Nothing
            , error = Nothing
            }
        ]


{-| A new format starts from the first prefab where there is one, because a
format nobody has filled in renders nothing. The uuid is the shell's to make.
-}
newFormat : List DocumentTemplateFormatDraft -> Uuid -> DocumentTemplateFormatDraft
newFormat prefabs uuid =
    case List.head prefabs of
        Just prefab ->
            { prefab | uuid = uuid }

        Nothing ->
            { uuid = uuid, name = "", icon = "", steps = [] }


steps : Config msg -> Int -> Html msg
steps config formatIndex =
    let
        locale =
            config.appState.locale

        listId =
            "dt-settings-steps-" ++ String.fromInt formatIndex

        listPath =
            FormField.listItemPath "formats" formatIndex "steps"

        row index =
            let
                path =
                    FormField.listItemPath listPath index
            in
            ListField.row
                { label = rowLabel (gettext "Step %s" locale) index (value config (path "name"))
                , content =
                    [ input config ("dt-settings-step-" ++ String.fromInt formatIndex ++ "-" ++ String.fromInt index) (path "name") (gettext "Name" locale)
                    , options config formatIndex index
                    ]
                }
                -- Positional, as its fields' ids are: the rows have no key of their own.
                |> ListField.withRowTestId (listId ++ "_row_" ++ String.fromInt index)
    in
    ListField.toHtml
        { id = listId
        , label = gettext "Steps" locale
        , rows = List.map row (Form.getListIndexes listPath config.form)
        , addLabel = gettext "Add step" locale
        , onAdd = config.toMsg (Form.Append listPath)
        , onRemove = Just (config.toMsg << Form.RemoveItem listPath)
        , hint = Nothing
        , error = Nothing
        }


{-| What a step is given to work with: named values, a pair to a row.

A step without them does nothing useful — the jinja step is told which template
to render — so they are edited here rather than left to whatever filled the
format in.

-}
options : Config msg -> Int -> Int -> Html msg
options config formatIndex stepIndex =
    let
        locale =
            config.appState.locale

        listId =
            "dt-settings-step-options-" ++ String.fromInt formatIndex ++ "-" ++ String.fromInt stepIndex

        listPath =
            FormField.listItemPath (FormField.listItemPath "formats" formatIndex "steps") stepIndex "options"

        id part index =
            String.join "-"
                [ "dt-settings-step-option"
                , String.fromInt formatIndex
                , String.fromInt stepIndex
                , String.fromInt index
                , part
                ]

        row index =
            let
                path =
                    FormField.listItemPath listPath index
            in
            ListField.row
                { label = rowLabel (gettext "Option %s" locale) index (value config (path "key"))
                , content =
                    -- A name and its value are one fact, so they sit on one
                    -- line where the row is wide enough for two and stack where
                    -- it is not (`Ui.Layout.grid`).
                    [ div [ class "c-dt-settings-option" ]
                        [ Layout.grid
                            [ input config (id "name" index) (path "key") (gettext "Name" locale)
                            , input config (id "value" index) (path "value") (gettext "Value" locale)
                            ]
                        ]
                    ]
                }
                -- Positional, as its fields' ids are: the rows have no key of their own.
                |> ListField.withRowTestId (listId ++ "_row_" ++ String.fromInt index)
    in
    ListField.toHtml
        { id = listId
        , label = gettext "Options" locale
        , rows = List.map row (Form.getListIndexes listPath config.form)
        , addLabel = gettext "Add option" locale
        , onAdd = config.toMsg (Form.Append listPath)
        , onRemove = Just (config.toMsg << Form.RemoveItem listPath)
        , hint = Nothing
        , error = Nothing
        }



-- ODDS AND ENDS


{-| A text field of the form at `path`, with the id it is known by.
-}
input : Config msg -> String -> String -> String -> Html msg
input config id path label =
    FormField.input config.appState config.form path label
        |> FormField.withId id
        |> FormField.toHtml
        |> Html.map config.toMsg


value : Config msg -> String -> String
value config path =
    Maybe.withDefault "" (Form.getFieldAsString path config.form).value


{-| A row is called by its name, and by its position until it has one.
-}
rowLabel : String -> Int -> String -> String
rowLabel template index name =
    if String.isEmpty name then
        String.replace "%s" (String.fromInt (index + 1)) template

    else
        name
