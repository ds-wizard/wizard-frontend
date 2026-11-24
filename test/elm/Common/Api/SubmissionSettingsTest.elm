module Common.Api.SubmissionSettingsTest exposing (submissionSettingsTests)

import Common.Api.Models.Settings.SubmissionSettings as SubmissionSettings exposing (Target(..))
import Dict
import Expect
import Json.Decode as D
import Test exposing (Test, describe, test)


json : String
json =
    """
    {
      "enabled": true,
      "definitions": [
        {
          "id": "repository",
          "name": "Repository",
          "description": "",
          "exports": [{ "id": "dsw:report", "version": "1.0.0", "formatName": "PDF", "madmpMetadata": false }],
          "target": {
            "type": "HttpSubmissionTarget",
            "props": ["Token"],
            "request": { "url": "https://example.com", "method": "POST", "multipart": { "enabled": false, "fileName": "" }, "headers": { "Authorization": "${Token}" } }
          }
        },
        {
          "id": "zenodo",
          "name": "Zenodo",
          "description": "DMP",
          "exports": [
            { "id": "dsw:report", "version": "1.0.0", "formatName": "PDF" },
            { "id": "dsw:madmp", "version": "1.0.0", "formatName": "JSON", "madmpMetadata": true }
          ],
          "target": {
            "type": "ZenodoSubmissionTarget",
            "repository": "SandboxZenodoRepository",
            "resourceType": "publication-datamanagementplan",
            "licenseId": "cc-by-4.0",
            "accessLevel": "public",
            "community": null
          }
        }
      ]
    }
    """


submissionSettingsTests : Test
submissionSettingsTests =
    describe "SubmissionSettings"
        [ test "decodes the HTTP and Zenodo targets" <|
            \_ ->
                case D.decodeString SubmissionSettings.decoder json of
                    Ok settings ->
                        List.map .target settings.definitions
                            |> Expect.equal
                                [ HttpTarget
                                    { props = [ "Token" ]
                                    , request = { url = "https://example.com", method = "POST", multipart = { enabled = False, fileName = "" }, headers = Dict.fromList [ ( "Authorization", "${Token}" ) ] }
                                    }
                                , ZenodoTarget
                                    { repository = "SandboxZenodoRepository"
                                    , resourceType = "publication-datamanagementplan"
                                    , licenseId = "cc-by-4.0"
                                    , accessLevel = "public"
                                    , community = Nothing
                                    }
                                ]

                    Err error ->
                        Expect.fail (D.errorToString error)
        , test "defaults the maDMP flag of an export to false" <|
            \_ ->
                D.decodeString SubmissionSettings.decoder json
                    |> Result.map (.definitions >> List.concatMap .exports >> List.map .madmpMetadata)
                    |> Expect.equal (Ok [ False, False, True ])
        , test "encodes what it decodes" <|
            \_ ->
                let
                    decoded =
                        D.decodeString SubmissionSettings.decoder json
                in
                decoded
                    |> Result.map SubmissionSettings.encode
                    |> Result.andThen (D.decodeValue SubmissionSettings.decoder)
                    |> Expect.equal decoded
        ]
