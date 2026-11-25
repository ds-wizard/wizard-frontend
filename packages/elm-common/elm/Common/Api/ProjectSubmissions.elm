module Common.Api.ProjectSubmissions exposing
    ( getAvailableSubmissions
    , getSubmission
    , getSubmissions
    , postSubmission
    )

import Common.Api.Context as Context exposing (Context)
import Common.Api.Models.AvailableSubmission as AvailableSubmission exposing (AvailableSubmission)
import Common.Api.Models.Submission as Submission exposing (Submission)
import Common.Api.Request as Request exposing (ToMsg)
import Json.Decode as D
import Json.Encode as E
import Uuid exposing (Uuid)


getAvailableSubmissions : Context ctx -> Uuid -> ToMsg (List AvailableSubmission) msg -> Cmd msg
getAvailableSubmissions appState projectUuid =
    Request.get (Context.toServerInfo appState) ("/projects/" ++ Uuid.toString projectUuid ++ "/available-submissions") (D.list AvailableSubmission.decoder)


getSubmissions : Context ctx -> Uuid -> ToMsg (List Submission) msg -> Cmd msg
getSubmissions appState projectUuid =
    Request.get (Context.toServerInfo appState) ("/projects/" ++ Uuid.toString projectUuid ++ "/submissions") (D.list Submission.decoder)


getSubmission : Context ctx -> Uuid -> Uuid -> ToMsg Submission msg -> Cmd msg
getSubmission appState projectUuid submissionUuid =
    Request.get (Context.toServerInfo appState) ("/projects/" ++ Uuid.toString projectUuid ++ "/submissions/" ++ Uuid.toString submissionUuid) Submission.decoder


postSubmission : Context ctx -> Uuid -> String -> ToMsg Submission msg -> Cmd msg
postSubmission appState projectUuid definitionId =
    let
        body =
            E.object [ ( "definitionId", E.string definitionId ) ]
    in
    Request.post (Context.toServerInfo appState) ("/projects/" ++ Uuid.toString projectUuid ++ "/submissions") Submission.decoder body
