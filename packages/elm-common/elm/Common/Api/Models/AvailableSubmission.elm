module Common.Api.Models.AvailableSubmission exposing
    ( AvailableSubmission
    , AvailableSubmissionExport
    , decoder
    , isZenodo
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Uuid exposing (Uuid)


{-| A submission the administrator defined that a project can be submitted with:
the documents it generates and the kind of its target. A target that needs an
account (Zenodo, an HTTP service with OAuth) names it in `oauthProvider`;
`requiresConnection` says the current user has not connected it (yet), and
`connectOnSubmit` whether submitting connects it first.
-}
type alias AvailableSubmission =
    { id : String
    , name : String
    , description : String
    , targetType : String
    , exports : List AvailableSubmissionExport
    , oauthProvider : Maybe String
    , requiresConnection : Bool
    , connectOnSubmit : Bool
    }


type alias AvailableSubmissionExport =
    { documentTemplateUuid : Uuid
    , documentTemplateName : String
    , documentTemplateId : String
    , documentTemplateVersion : String
    , formatUuid : Uuid
    , formatName : String
    }


decoder : Decoder AvailableSubmission
decoder =
    D.succeed AvailableSubmission
        |> D.required "id" D.string
        |> D.required "name" D.string
        |> D.required "description" D.string
        |> D.required "targetType" D.string
        |> D.required "exports" (D.list exportDecoder)
        |> D.optional "oauthProvider" (D.maybe D.string) Nothing
        |> D.optional "requiresConnection" D.bool False
        |> D.optional "connectOnSubmit" D.bool True


exportDecoder : Decoder AvailableSubmissionExport
exportDecoder =
    D.succeed AvailableSubmissionExport
        |> D.required "documentTemplateUuid" Uuid.decoder
        |> D.required "documentTemplateName" D.string
        |> D.required "documentTemplateId" D.string
        |> D.required "documentTemplateVersion" D.string
        |> D.required "formatUuid" Uuid.decoder
        |> D.required "formatName" D.string


isZenodo : AvailableSubmission -> Bool
isZenodo submission =
    submission.targetType == "ZenodoSubmissionTarget"
