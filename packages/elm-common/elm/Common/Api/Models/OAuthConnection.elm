module Common.Api.Models.OAuthConnection exposing
    ( OAuthAuthorization
    , OAuthCallbackResult
    , OAuthConnection
    , authorizationDecoder
    , callbackResultDecoder
    , decoder
    )

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Time
import Uuid exposing (Uuid)


{-| The connection of the current user's account to a service the submissions
send to with OAuth: a Zenodo repository (`zenodo:…`) or an HTTP service of a
submission (`submission:…`). `missingScopes` are the permissions a connection
made earlier lacks; it has to be connected again.
-}
type alias OAuthConnection =
    { provider : String
    , name : String
    , connected : Bool
    , scope : Maybe String
    , expiresAt : Maybe Time.Posix
    , missingScopes : List String
    }


type alias OAuthAuthorization =
    { url : String }


{-| What completing a connection did: when it was started by a submission, the
submission created (or why it could not be) and where to return.
-}
type alias OAuthCallbackResult =
    { connection : OAuthConnection
    , returnUrl : Maybe String
    , submissionUuid : Maybe Uuid
    , submissionError : Maybe String
    }


decoder : Decoder OAuthConnection
decoder =
    D.succeed OAuthConnection
        |> D.required "provider" D.string
        |> D.required "name" D.string
        |> D.required "connected" D.bool
        |> D.required "scope" (D.maybe D.string)
        |> D.required "expiresAt" (D.maybe D.datetime)
        |> D.required "missingScopes" (D.list D.string)


authorizationDecoder : Decoder OAuthAuthorization
authorizationDecoder =
    D.succeed OAuthAuthorization
        |> D.required "url" D.string


callbackResultDecoder : Decoder OAuthCallbackResult
callbackResultDecoder =
    D.succeed OAuthCallbackResult
        |> D.required "connection" decoder
        |> D.required "returnUrl" (D.maybe D.string)
        |> D.required "submissionUuid" (D.maybe Uuid.decoder)
        |> D.required "submissionError" (D.maybe D.string)
