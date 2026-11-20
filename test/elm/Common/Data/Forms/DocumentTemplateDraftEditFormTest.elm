module Common.Data.Forms.DocumentTemplateDraftEditFormTest exposing (suite)

{-| The document template editor's settings as a form: what it sends is what the
draft holds, only the ID is held to a format, and a field the server refuses is
marked with the server's reason.
-}

import Common.Api.ApiError as ApiError
import Common.Api.Models.DocumentTemplateDraftDetail exposing (DocumentTemplateDraftDetail)
import Common.Data.Forms.DocumentTemplateDraftEditForm as DocumentTemplateDraftEditForm exposing (DocumentTemplateDraftEditForm)
import Common.Utils.Form as FormUtils
import Common.Utils.Form.FormError exposing (FormError)
import Expect
import Form exposing (Form)
import Form.Field as Field
import Gettext
import Json.Encode as E
import Test exposing (Test, describe, test)
import Time
import Uuid
import Version


appState : { locale : Gettext.Locale }
appState =
    { locale = Gettext.defaultLocale }


suite : Test
suite =
    describe "DocumentTemplateDraftEditForm"
        [ test "the draft goes through the form unchanged, the order of options included" <|
            \_ ->
                Form.getOutput (DocumentTemplateDraftEditForm.init appState draft)
                    |> Maybe.map (encode >> E.encode 0)
                    |> Expect.equal (Just expectedJson)
        , test "a publish or a rename sends the draft as the form would" <|
            \_ ->
                DocumentTemplateDraftEditForm.fromDraft draft
                    |> encode
                    |> E.encode 0
                    |> Expect.equal expectedJson
        , test "an ID with characters an ID cannot have is an error on the ID" <|
            \_ ->
                let
                    form =
                        DocumentTemplateDraftEditForm.init appState draft
                            |> set "id" "bad id!"
                in
                Expect.all
                    [ Form.getOutput >> Expect.equal Nothing
                    , Form.getErrors >> List.map Tuple.first >> Expect.equal [ "id" ]
                    ]
                    form
        , test "a valid ID is sent" <|
            \_ ->
                DocumentTemplateDraftEditForm.init appState draft
                    |> set "id" "my_template-2.0"
                    |> Form.getOutput
                    |> Maybe.map .id
                    |> Expect.equal (Just "my_template-2.0")
        , test "empty names and icons of formats, steps and options are accepted, as the server accepts them" <|
            \_ ->
                DocumentTemplateDraftEditForm.init appState
                    { draft
                        | name = ""
                        , description = ""
                        , license = ""
                        , readme = ""
                        , formats = [ { uuid = formatUuid, name = "", icon = "", steps = [ { name = "", options = [ ( "", "" ) ] } ] } ]
                    }
                    |> Form.update (DocumentTemplateDraftEditForm.validation appState) (Form.Append "allowedPackages")
                    |> Form.update (DocumentTemplateDraftEditForm.validation appState) (Form.Append "formats.0.steps")
                    -- elm-form validates on every message but an append.
                    |> Form.update (DocumentTemplateDraftEditForm.validation appState) Form.Validate
                    |> Form.getOutput
                    |> Maybe.map (\output -> ( List.length output.allowedPackages, List.map (.steps >> List.length) output.formats ))
                    |> Expect.equal (Just ( 2, [ 2 ] ))
        , test "a format added from a prefab comes with its uuid, steps and options" <|
            \_ ->
                let
                    newUuid =
                        Uuid.fromUuidString "33333333-3333-4333-8333-333333333333"

                    prefab =
                        { uuid = newUuid
                        , name = "PDF"
                        , icon = "far fa-file-pdf"
                        , steps =
                            [ { name = "jinja", options = [ ( "template", "index.html.j2" ), ( "content-type", "text/html" ) ] }
                            , { name = "wkhtmltopdf", options = [] }
                            ]
                        }
                in
                DocumentTemplateDraftEditForm.init appState draft
                    |> DocumentTemplateDraftEditForm.appendFormat appState prefab
                    |> Form.getOutput
                    |> Maybe.map (.formats >> List.drop 1)
                    |> Expect.equal (Just [ prefab ])
        , test "an ID the server refuses is marked with the server's reason" <|
            \_ ->
                DocumentTemplateDraftEditForm.init appState draft
                    |> FormUtils.setFormErrors appState (ApiError.BadStatus 400 serverError)
                    |> Form.getFieldAsString "id"
                    |> .error
                    |> Maybe.map (FormUtils.errorToString appState.locale "ID")
                    |> Expect.equal (Just "'test!' is not in a valid format. Use only letters, numbers, hyphens, underscores, and dots.")
        ]


encode : DocumentTemplateDraftEditForm -> E.Value
encode =
    DocumentTemplateDraftEditForm.encode "DraftDocumentTemplatePhase"


set : String -> String -> Form FormError DocumentTemplateDraftEditForm -> Form FormError DocumentTemplateDraftEditForm
set path value =
    Form.update (DocumentTemplateDraftEditForm.validation appState) (Form.Input path Form.Text (Field.String value))


formatUuid : Uuid.Uuid
formatUuid =
    Uuid.fromUuidString "22222222-2222-4222-8222-222222222222"


draft : DocumentTemplateDraftDetail
draft =
    { allowedPackages = [ { id = Just "org:km", minVersion = Just "1.0.0", maxVersion = Nothing } ]
    , createdAt = Time.millisToPosix 0
    , description = "A template"
    , formats =
        [ { uuid = formatUuid
          , name = "HTML"
          , icon = "far fa-file-code"
          , steps = [ { name = "jinja", options = [ ( "template", "index.html.j2" ), ( "content-type", "text/html" ) ] } ]
          }
        ]
    , uuid = Uuid.fromUuidString "11111111-1111-4111-8111-111111111111"
    , language = "en"
    , license = "MIT"
    , name = "My template"
    , readme = "# Readme"
    , id = "my-template"
    , version = Version.create 1 2 3
    , formatUuid = Nothing
    , projectUuid = Nothing
    , project = Nothing
    , knowledgeModelEditorUuid = Nothing
    , knowledgeModelEditor = Nothing
    , workspaceUuid = Nothing
    }


expectedJson : String
expectedJson =
    String.concat
        [ "{\"name\":\"My template\",\"description\":\"A template\",\"language\":\"en\",\"license\":\"MIT\",\"readme\":\"# Readme\""
        , ",\"allowedPackages\":[{\"id\":\"org:km\",\"minVersion\":\"1.0.0\",\"maxVersion\":null}]"
        , ",\"formats\":[{\"uuid\":\"22222222-2222-4222-8222-222222222222\",\"name\":\"HTML\",\"icon\":\"far fa-file-code\""
        , ",\"steps\":[{\"name\":\"jinja\",\"options\":{\"template\":\"index.html.j2\",\"content-type\":\"text/html\"}}]}]"
        , ",\"id\":\"my-template\",\"version\":\"1.2.3\",\"phase\":\"DraftDocumentTemplatePhase\"}"
        ]


serverError : String
serverError =
    """{"fieldErrors":{"id":[{"code":"error.validation.invalid_coordinate_part_format","defaultMessage":"%s '%s' is not in the valid format","params":["id","test!"]}]},"formErrors":[],"status":400,"type":"UserFormError"}"""
