module Common.Api.Models.Settings.SubmissionSettings exposing
    ( Definition
    , Export
    , Multipart
    , Request
    , SubmissionSettings
    , Target(..)
    , ZenodoTarget
    , accessLevelOptions
    , decoder
    , encode
    , httpTargetType
    , initEmptyForm
    , initForm
    , resourceTypeOptions
    , section
    , validation
    , zenodoRepositoryName
    , zenodoTargetType
    )

import Common.Utils.Form.Field as Field
import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.Form.Validate as V
import Dict exposing (Dict)
import Form exposing (Form)
import Form.Field as Field exposing (Field)
import Form.Validate as V exposing (Validation)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E


type alias SubmissionSettings =
    { enabled : Bool
    , definitions : List Definition
    }


{-| A named submission: the documents (exports) it generates from a project and
where they are sent.
-}
type alias Definition =
    { id : String
    , name : String
    , description : String
    , exports : List Export
    , target : Target
    }


{-| A document template version and one of its formats (by name).
`madmpMetadata` marks an RDA maDMP JSON export Zenodo reads the metadata from.
-}
type alias Export =
    { id : String
    , version : String
    , formatName : String
    , madmpMetadata : Bool
    }


type Target
    = HttpTarget { props : List String, request : Request }
    | ZenodoTarget ZenodoTarget


type alias ZenodoTarget =
    { repository : String
    , resourceType : String
    , licenseId : String
    , accessLevel : String
    , community : Maybe String
    }


type alias Request =
    { url : String
    , method : String
    , multipart : Multipart
    , headers : Dict String String
    }


type alias Multipart =
    { enabled : Bool
    , fileName : String
    }


section : { name : String, decoder : Decoder SubmissionSettings, encode : SubmissionSettings -> E.Value }
section =
    { name = "submission", decoder = decoder, encode = encode }


httpTargetType : String
httpTargetType =
    "HttpSubmissionTarget"


zenodoTargetType : String
zenodoTargetType =
    "ZenodoSubmissionTarget"


zenodoRepositoryName : String -> String
zenodoRepositoryName repository =
    case repository of
        "SandboxZenodoRepository" ->
            "Zenodo Sandbox (sandbox.zenodo.org)"

        "ProductionZenodoRepository" ->
            "Zenodo (zenodo.org)"

        _ ->
            repository


{-| The resource types of the Zenodo vocabulary that fit documents generated
from a project.
-}
resourceTypeOptions : List ( String, String )
resourceTypeOptions =
    [ ( "publication-datamanagementplan", "Output management plan" )
    , ( "publication-report", "Report" )
    , ( "publication", "Publication" )
    , ( "dataset", "Dataset" )
    , ( "other", "Other" )
    ]


accessLevelOptions : List ( String, String )
accessLevelOptions =
    [ ( "public", "Public" )
    , ( "restricted", "Restricted" )
    ]



-- JSON


decoder : Decoder SubmissionSettings
decoder =
    D.succeed SubmissionSettings
        |> D.required "enabled" D.bool
        |> D.required "definitions" (D.list decodeDefinition)


decodeDefinition : Decoder Definition
decodeDefinition =
    D.succeed Definition
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "description" D.string
        |> D.required "exports" (D.list decodeExport)
        |> D.required "target" decodeTarget


decodeExport : Decoder Export
decodeExport =
    D.succeed Export
        |> D.required "id" D.string
        |> D.required "version" D.string
        |> D.required "formatName" D.string
        |> D.optional "madmpMetadata" D.bool False


decodeTarget : Decoder Target
decodeTarget =
    D.field "type" D.string
        |> D.andThen
            (\targetType ->
                if targetType == zenodoTargetType then
                    D.map ZenodoTarget decodeZenodoTarget

                else
                    D.map HttpTarget decodeHttpTarget
            )


decodeHttpTarget : Decoder { props : List String, request : Request }
decodeHttpTarget =
    D.succeed (\props request -> { props = props, request = request })
        |> D.required "props" (D.list D.string)
        |> D.required "request" decodeRequest


decodeZenodoTarget : Decoder ZenodoTarget
decodeZenodoTarget =
    D.succeed ZenodoTarget
        |> D.required "repository" D.string
        |> D.required "resourceType" D.string
        |> D.required "licenseId" D.string
        |> D.required "accessLevel" D.string
        |> D.optional "community" (D.maybe D.string) Nothing


decodeRequest : Decoder Request
decodeRequest =
    D.succeed Request
        |> D.required "url" D.string
        |> D.required "method" D.string
        |> D.required "multipart" decodeMultipart
        |> D.required "headers" (D.dict D.string)


decodeMultipart : Decoder Multipart
decodeMultipart =
    D.succeed Multipart
        |> D.required "enabled" D.bool
        |> D.required "fileName" D.string


encode : SubmissionSettings -> E.Value
encode config =
    E.object
        [ ( "enabled", E.bool config.enabled )
        , ( "definitions", E.list encodeDefinition config.definitions )
        ]


encodeDefinition : Definition -> E.Value
encodeDefinition definition =
    E.object
        [ ( "id", E.string definition.id )
        , ( "name", E.string definition.name )
        , ( "description", E.string definition.description )
        , ( "exports", E.list encodeExport definition.exports )
        , ( "target", encodeTarget definition.target )
        ]


encodeExport : Export -> E.Value
encodeExport export =
    E.object
        [ ( "id", E.string export.id )
        , ( "version", E.string export.version )
        , ( "formatName", E.string export.formatName )
        , ( "madmpMetadata", E.bool export.madmpMetadata )
        ]


encodeTarget : Target -> E.Value
encodeTarget target =
    case target of
        HttpTarget httpTarget ->
            E.object
                [ ( "type", E.string httpTargetType )
                , ( "props", E.list E.string httpTarget.props )
                , ( "request", encodeRequest httpTarget.request )
                ]

        ZenodoTarget zenodoTarget ->
            E.object
                [ ( "type", E.string zenodoTargetType )
                , ( "repository", E.string zenodoTarget.repository )
                , ( "resourceType", E.string zenodoTarget.resourceType )
                , ( "licenseId", E.string zenodoTarget.licenseId )
                , ( "accessLevel", E.string zenodoTarget.accessLevel )
                , ( "community", E.maybe E.string zenodoTarget.community )
                ]


encodeRequest : Request -> E.Value
encodeRequest request =
    E.object
        [ ( "url", E.string request.url )
        , ( "method", E.string request.method )
        , ( "multipart", encodeMultipart request.multipart )
        , ( "headers", E.dict identity E.string request.headers )
        ]


encodeMultipart : Multipart -> E.Value
encodeMultipart multipart =
    E.object
        [ ( "enabled", E.bool multipart.enabled )
        , ( "fileName", E.string multipart.fileName )
        ]



-- Form


validation : Validation FormError SubmissionSettings
validation =
    V.succeed SubmissionSettings
        |> V.andMap (V.field "enabled" V.bool)
        |> V.andMap (V.field "definitions" (V.list validateDefinition))


validateDefinition : Validation FormError Definition
validateDefinition =
    V.succeed Definition
        |> V.andMap (V.field "id" V.string)
        |> V.andMap (V.field "name" V.string)
        |> V.andMap (V.field "description" V.optionalString)
        |> V.andMap (V.field "exports" (V.list validateExport))
        |> V.andMap (V.field "target" validateTarget)


validateExport : Validation FormError Export
validateExport =
    V.succeed Export
        |> V.andMap (V.field "id" V.string)
        |> V.andMap (V.field "version" V.string)
        |> V.andMap (V.field "formatName" V.string)
        |> V.andMap (V.field "madmpMetadata" V.bool)


{-| The form keeps the fields of both target types, so switching the type does
not lose what was filled in; only the chosen type is validated.
-}
validateTarget : Validation FormError Target
validateTarget =
    V.field "type" V.string
        |> V.andThen
            (\targetType ->
                if targetType == zenodoTargetType then
                    V.map ZenodoTarget validateZenodoTarget

                else
                    V.map HttpTarget validateHttpTarget
            )


validateHttpTarget : Validation FormError { props : List String, request : Request }
validateHttpTarget =
    V.succeed (\props request -> { props = props, request = request })
        |> V.andMap (V.field "props" (V.list V.string))
        |> V.andMap (V.field "request" validateRequest)


validateZenodoTarget : Validation FormError ZenodoTarget
validateZenodoTarget =
    V.succeed ZenodoTarget
        |> V.andMap (V.field "repository" V.string)
        |> V.andMap (V.field "resourceType" V.string)
        |> V.andMap (V.field "licenseId" V.string)
        |> V.andMap (V.field "accessLevel" V.string)
        |> V.andMap (V.field "community" V.maybeString)


validateRequest : Validation FormError Request
validateRequest =
    V.succeed Request
        |> V.andMap (V.field "url" V.string)
        |> V.andMap (V.field "method" V.string)
        |> V.andMap (V.field "multipart" validateMultipart)
        |> V.andMap (V.field "headers" (V.dict V.string))


validateMultipart : Validation FormError Multipart
validateMultipart =
    V.succeed Multipart
        |> V.andMap (V.field "enabled" V.bool)
        |> V.andMap (V.field "enabled" V.bool |> V.ifElse "fileName" V.string V.optionalString)


initEmptyForm : Form FormError SubmissionSettings
initEmptyForm =
    Form.initial [] validation


initForm : SubmissionSettings -> Form FormError SubmissionSettings
initForm config =
    Form.initial (initConfig config) validation


initConfig : SubmissionSettings -> List ( String, Field )
initConfig config =
    [ ( "enabled", Field.bool config.enabled )
    , ( "definitions", Field.list (List.map (Field.group << initDefinition) config.definitions) )
    ]


initDefinition : Definition -> List ( String, Field )
initDefinition definition =
    [ ( "id", Field.string definition.id )
    , ( "name", Field.string definition.name )
    , ( "description", Field.string definition.description )
    , ( "exports", Field.list (List.map (Field.group << initExport) definition.exports) )
    , ( "target", Field.group (initTarget definition.target) )
    ]


initExport : Export -> List ( String, Field )
initExport export =
    [ ( "id", Field.string export.id )
    , ( "version", Field.string export.version )
    , ( "formatName", Field.string export.formatName )
    , ( "madmpMetadata", Field.bool export.madmpMetadata )
    ]


initTarget : Target -> List ( String, Field )
initTarget target =
    case target of
        HttpTarget httpTarget ->
            ( "type", Field.string httpTargetType ) :: httpTargetFields httpTarget ++ zenodoTargetFields defaultZenodoTarget

        ZenodoTarget zenodoTarget ->
            ( "type", Field.string zenodoTargetType ) :: zenodoTargetFields zenodoTarget ++ httpTargetFields emptyHttpTarget


emptyHttpTarget : { props : List String, request : Request }
emptyHttpTarget =
    { props = [], request = { url = "", method = "POST", multipart = { enabled = False, fileName = "" }, headers = Dict.empty } }


defaultZenodoTarget : ZenodoTarget
defaultZenodoTarget =
    { repository = "SandboxZenodoRepository"
    , resourceType = "publication-datamanagementplan"
    , licenseId = "cc-by-4.0"
    , accessLevel = "public"
    , community = Nothing
    }


httpTargetFields : { props : List String, request : Request } -> List ( String, Field )
httpTargetFields httpTarget =
    [ ( "props", Field.list (List.map Field.string httpTarget.props) )
    , ( "request", Field.group (initRequest httpTarget.request) )
    ]


zenodoTargetFields : ZenodoTarget -> List ( String, Field )
zenodoTargetFields zenodoTarget =
    [ ( "repository", Field.string zenodoTarget.repository )
    , ( "resourceType", Field.string zenodoTarget.resourceType )
    , ( "licenseId", Field.string zenodoTarget.licenseId )
    , ( "accessLevel", Field.string zenodoTarget.accessLevel )
    , ( "community", Field.maybeString zenodoTarget.community )
    ]


initRequest : Request -> List ( String, Field )
initRequest request =
    [ ( "url", Field.string request.url )
    , ( "method", Field.string request.method )
    , ( "multipart", Field.group (initMultipart request.multipart) )
    , ( "headers", Field.dict Field.string request.headers )
    ]


initMultipart : Multipart -> List ( String, Field )
initMultipart multipart =
    [ ( "enabled", Field.bool multipart.enabled )
    , ( "fileName", Field.string multipart.fileName )
    ]
