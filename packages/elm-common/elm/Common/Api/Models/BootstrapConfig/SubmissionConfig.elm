module Common.Api.Models.BootstrapConfig.SubmissionConfig exposing
    ( SubmissionConfig
    , decoder
    , default
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D


{-| `zenodoRepositories` are the Zenodo repositories (`SandboxZenodoRepository`,
`ProductionZenodoRepository`) the administrator configured an OAuth application for.
-}
type alias SubmissionConfig =
    { enabled : Bool
    , zenodoRepositories : List String
    }


decoder : Decoder SubmissionConfig
decoder =
    D.succeed SubmissionConfig
        |> D.required "enabled" D.bool
        |> D.optional "zenodoRepositories" (D.list D.string) []


default : SubmissionConfig
default =
    { enabled = False
    , zenodoRepositories = []
    }
