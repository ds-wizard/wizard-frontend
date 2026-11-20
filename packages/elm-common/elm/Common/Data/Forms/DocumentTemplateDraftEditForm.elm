module Common.Data.Forms.DocumentTemplateDraftEditForm exposing
    ( DocumentTemplateDraftEditForm
    , appendFormat
    , encode
    , fromDraft
    , init
    , initEmpty
    , validation
    )

{-| The settings of a document template draft — what it is called, which
knowledge models it works with and the formats it renders in — as one form.

The fields are named as the `PUT` names them, so an error the server sends for
a field (`id`) lands on the field of the same name.

Only the ID is held to a format here. Everything else is accepted as the server
accepts it, empty names and icons of formats, steps and options included.

-}

import Common.Api.Models.DocumentTemplate.DocumentTemplateAllowedPackage as DocumentTemplateAllowedPackage exposing (DocumentTemplateAllowedPackage)
import Common.Api.Models.DocumentTemplate.DocumentTemplateFormatStep exposing (DocumentTemplateFormatStep)
import Common.Api.Models.DocumentTemplateDraft.DocumentTemplateFormatDraft as DocumentTemplateFormatDraft exposing (DocumentTemplateFormatDraft)
import Common.Api.Models.DocumentTemplateDraftDetail exposing (DocumentTemplateDraftDetail)
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.Form.Validate as V
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V exposing (Validation)
import Gettext
import Json.Encode as E
import Uuid
import Version


type alias DocumentTemplateDraftEditForm =
    { name : String
    , description : String
    , id : String
    , versionMajor : Int
    , versionMinor : Int
    , versionPatch : Int
    , language : String
    , license : String
    , readme : String
    , allowedPackages : List DocumentTemplateAllowedPackage
    , formats : List DocumentTemplateFormatDraft
    }


init : { a | locale : Gettext.Locale } -> DocumentTemplateDraftDetail -> Form FormError DocumentTemplateDraftEditForm
init appState draft =
    let
        initials =
            [ ( "name", Field.string draft.name )
            , ( "description", Field.string draft.description )
            , ( "id", Field.string draft.id )
            , ( "versionMajor", Field.string (String.fromInt (Version.getMajor draft.version)) )
            , ( "versionMinor", Field.string (String.fromInt (Version.getMinor draft.version)) )
            , ( "versionPatch", Field.string (String.fromInt (Version.getPatch draft.version)) )
            , ( "language", Field.string draft.language )
            , ( "license", Field.string draft.license )
            , ( "readme", Field.string draft.readme )
            , ( "allowedPackages", Field.list (List.map DocumentTemplateAllowedPackage.init draft.allowedPackages) )
            , ( "formats", Field.list (List.map DocumentTemplateFormatDraft.field draft.formats) )
            ]
    in
    Form.initial initials (validation appState)


initEmpty : { a | locale : Gettext.Locale } -> Form FormError DocumentTemplateDraftEditForm
initEmpty appState =
    Form.initial [] (validation appState)


validation : { a | locale : Gettext.Locale } -> Validation FormError DocumentTemplateDraftEditForm
validation appState =
    V.succeed DocumentTemplateDraftEditForm
        |> V.andMap (V.field "name" V.optionalString)
        |> V.andMap (V.field "description" V.optionalString)
        |> V.andMap (V.field "id" (V.documentTemplateId appState))
        |> V.andMap (V.field "versionMajor" V.versionNumber)
        |> V.andMap (V.field "versionMinor" V.versionNumber)
        |> V.andMap (V.field "versionPatch" V.versionNumber)
        |> V.andMap (V.field "language" V.optionalString)
        |> V.andMap (V.field "license" V.optionalString)
        |> V.andMap (V.field "readme" V.optionalString)
        |> V.andMap (V.field "allowedPackages" (V.list DocumentTemplateAllowedPackage.validation))
        |> V.andMap (V.field "formats" (V.list DocumentTemplateFormatDraft.validation))


{-| The draft's settings as the server holds them, for a request that changes
none of them but the phase or the name — a publish, a rename from the title.
-}
fromDraft : DocumentTemplateDraftDetail -> DocumentTemplateDraftEditForm
fromDraft draft =
    { name = draft.name
    , description = draft.description
    , id = draft.id
    , versionMajor = Version.getMajor draft.version
    , versionMinor = Version.getMinor draft.version
    , versionPatch = Version.getPatch draft.version
    , language = draft.language
    , license = draft.license
    , readme = draft.readme
    , allowedPackages = draft.allowedPackages
    , formats = draft.formats
    }


{-| The settings, as the `PUT` wants them. The phase is what tells a save from
a publish: the same endpoint with `Released` is how a template is published.
-}
encode : String -> DocumentTemplateDraftEditForm -> E.Value
encode phase form =
    E.object
        [ ( "name", E.string form.name )
        , ( "description", E.string form.description )
        , ( "language", E.string form.language )
        , ( "license", E.string form.license )
        , ( "readme", E.string form.readme )
        , ( "allowedPackages", E.list DocumentTemplateAllowedPackage.encode form.allowedPackages )
        , ( "formats", E.list DocumentTemplateFormatDraft.encode form.formats )
        , ( "id", E.string form.id )
        , ( "version", E.string (String.join "." (List.map String.fromInt [ form.versionMajor, form.versionMinor, form.versionPatch ])) )
        , ( "phase", E.string phase )
        ]


{-| A new format at the end of the list, filled in with `format` — a prefab,
usually, with a uuid of its own.

It goes in through the form's own messages rather than by building the form
again, so what is already typed and the changes the form knows of stay.

-}
appendFormat : { a | locale : Gettext.Locale } -> DocumentTemplateFormatDraft -> Form FormError DocumentTemplateDraftEditForm -> Form FormError DocumentTemplateDraftEditForm
appendFormat appState format form =
    let
        formatPath =
            "formats." ++ String.fromInt (List.length (Form.getListIndexes "formats" form))

        messages =
            Form.Append "formats"
                :: input (formatPath ++ ".uuid") (Uuid.toString format.uuid)
                :: input (formatPath ++ ".name") format.name
                :: input (formatPath ++ ".icon") format.icon
                :: List.concat (List.indexedMap (stepMessages formatPath) format.steps)
    in
    List.foldl (Form.update (validation appState)) form messages


stepMessages : String -> Int -> DocumentTemplateFormatStep -> List Form.Msg
stepMessages formatPath index step =
    let
        stepPath =
            formatPath ++ ".steps." ++ String.fromInt index

        optionMessages optionIndex ( key, value ) =
            let
                optionPath =
                    stepPath ++ ".options." ++ String.fromInt optionIndex
            in
            [ Form.Append (stepPath ++ ".options")
            , input (optionPath ++ ".key") key
            , input (optionPath ++ ".value") value
            ]
    in
    Form.Append (formatPath ++ ".steps")
        :: input (stepPath ++ ".name") step.name
        :: List.concat (List.indexedMap optionMessages step.options)


input : String -> String -> Form.Msg
input path value =
    Form.Input path Form.Text (Field.String value)
