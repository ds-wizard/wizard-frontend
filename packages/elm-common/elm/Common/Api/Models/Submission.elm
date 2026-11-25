module Common.Api.Models.Submission exposing
    ( Submission
    , SubmissionDocument
    , compare
    , decoder
    , getReturnedData
    , isZenodo
    , visibleName
    )

import Common.Api.Models.Document.DocumentState as DocumentState exposing (DocumentState)
import Common.Api.Models.Submission.SubmissionState as SubmissionState exposing (SubmissionState)
import Common.Api.Models.UserSuggestion as UserSuggestion exposing (UserSuggestion)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Time
import Time.Extra as Time
import Uuid exposing (Uuid)


{-| A submission of a project: the documents a submission definition generated
from it and where they were sent (`location`, `externalId` is the record of
Zenodo).
-}
type alias Submission =
    { uuid : Uuid
    , state : SubmissionState
    , location : Maybe String
    , returnedData : Maybe String
    , definitionId : String
    , definitionName : Maybe String
    , targetType : Maybe String
    , projectUuid : Uuid
    , externalId : Maybe String
    , documents : List SubmissionDocument
    , createdBy : Maybe UserSuggestion
    , createdAt : Time.Posix
    , updatedAt : Time.Posix
    }


type alias SubmissionDocument =
    { uuid : Uuid
    , name : String
    , state : DocumentState
    }


decoder : Decoder Submission
decoder =
    D.succeed Submission
        |> D.required "uuid" Uuid.decoder
        |> D.required "state" SubmissionState.decoder
        |> D.required "location" (D.maybe D.string)
        |> D.required "returnedData" (D.maybe D.string)
        |> D.required "definitionId" D.string
        |> D.required "definitionName" (D.maybe D.string)
        |> D.required "targetType" (D.maybe D.string)
        |> D.required "projectUuid" Uuid.decoder
        |> D.required "externalId" (D.maybe D.string)
        |> D.required "documents" (D.list documentDecoder)
        |> D.required "createdBy" (D.maybe UserSuggestion.decoder)
        |> D.required "createdAt" D.datetime
        |> D.required "updatedAt" D.datetime


documentDecoder : Decoder SubmissionDocument
documentDecoder =
    D.succeed SubmissionDocument
        |> D.required "uuid" Uuid.decoder
        |> D.required "name" D.string
        |> D.required "state" DocumentState.decoder


visibleName : Submission -> String
visibleName submission =
    Maybe.withDefault submission.definitionId submission.definitionName


isZenodo : Submission -> Bool
isZenodo submission =
    submission.targetType == Just "ZenodoSubmissionTarget"


compare : Submission -> Submission -> Order
compare a b =
    Time.compare b.updatedAt a.updatedAt


getReturnedData : Submission -> String
getReturnedData =
    .returnedData
        >> Maybe.withDefault ""
        >> String.replace "Response Body: \"" "Response Body:\n\n"
        >> String.slice 0 -1
        >> String.replace "\\\\" "\\"
        >> String.replace "\\n" "\n"
        >> String.replace "\\\"" "\""
