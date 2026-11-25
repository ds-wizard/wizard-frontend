module Common.Api.OAuthConnections exposing
    ( authorize
    , completeAuthorization
    , disconnect
    , getConnections
    )

import Common.Api.Context as Context exposing (Context)
import Common.Api.Models.OAuthConnection as OAuthConnection exposing (OAuthAuthorization, OAuthCallbackResult, OAuthConnection)
import Common.Api.Request as Request exposing (ToMsg)
import Json.Decode as D
import Json.Encode as E
import Json.Encode.Extra as E
import Url
import Uuid exposing (Uuid)


getConnections : Context ctx -> ToMsg (List OAuthConnection) msg -> Cmd msg
getConnections appState =
    Request.get (Context.toServerInfo appState) "/users/current/oauth-connections" (D.list OAuthConnection.decoder)


{-| The URL of the service to send the user to; it sends them back to the
submission accounts of their profile with a code and the state. With a pending
submission, the submission is created once connected and the user returns to
`returnUrl`.
-}
authorize :
    Context ctx
    -> { provider : String, returnUrl : Maybe String, pendingSubmission : Maybe { projectUuid : Uuid, definitionId : String } }
    -> ToMsg OAuthAuthorization msg
    -> Cmd msg
authorize appState { provider, returnUrl, pendingSubmission } =
    Request.post (Context.toServerInfo appState)
        "/users/current/oauth-connections/authorize"
        OAuthConnection.authorizationDecoder
        (E.object
            [ ( "provider", E.string provider )
            , ( "returnUrl", E.maybe E.string returnUrl )
            , ( "projectUuid", E.maybe (E.string << Uuid.toString << .projectUuid) pendingSubmission )
            , ( "definitionId", E.maybe (E.string << .definitionId) pendingSubmission )
            ]
        )


completeAuthorization : Context ctx -> { code : String, state : String } -> ToMsg OAuthCallbackResult msg -> Cmd msg
completeAuthorization appState { code, state } =
    Request.post (Context.toServerInfo appState)
        "/users/current/oauth-connections/callback"
        OAuthConnection.callbackResultDecoder
        (E.object [ ( "code", E.string code ), ( "state", E.string state ) ])


disconnect : Context ctx -> String -> ToMsg () msg -> Cmd msg
disconnect appState provider =
    Request.delete (Context.toServerInfo appState) ("/users/current/oauth-connections/" ++ Url.percentEncode provider)
