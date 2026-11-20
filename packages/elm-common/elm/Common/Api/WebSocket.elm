port module Common.Api.WebSocket exposing
    ( RawMsg
    , WebSocket
    , WebSocketMsg(..)
    , close
    , init
    , listen
    , open
    , receive
    , send
    , withHeartbeat
    )

{-| A websocket to the API, held open by the ports (`ports/websocket.js`).

A websocket is known by its path under the API URL, not by its URL: the URL
carries the token, and the token may have changed by the time the socket is
opened again. When the connection drops, the ports say so at once
(`Disconnected`), wait — longer after every failed attempt, not at all once the
browser is back online or the tab is shown again — and then ask for it to be
opened again (`Reconnect`), and the page answers with `open`, which builds the
URL with the current token. The open that follows is `Reopen`, not `Open`:
whatever the server sent while the connection was down is lost, so a page that
keeps a copy of something catches up.

A socket `withHeartbeat` is pinged by the ports, and dropped and opened again
when a server that answers pings stops answering them: a connection can die
without the browser noticing.

`Close` is final: the server ended the connection for good (normally, on lost
access, or on a message too large), and nothing reopens it.

-}

import Common.Api.Context as Context exposing (Context)
import Common.Api.Request as Request
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type WebSocket
    = WebSocket { path : String, heartbeat : Bool }


type alias RawMsg =
    E.Value


type WebSocketMsg a
    = Open
    | Reopen
    | Message a
    | Disconnected
    | Reconnect
    | Close
    | Ignore


type alias WebSocketRawMsg =
    { key : String
    , type_ : String
    , data : E.Value
    }


decodeWebSocketRawMsg : Decoder WebSocketRawMsg
decodeWebSocketRawMsg =
    D.succeed WebSocketRawMsg
        |> D.required "key" D.string
        |> D.required "type" D.string
        |> D.required "data" D.value


{-| The websocket at a path under the API URL.
-}
init : String -> WebSocket
init path =
    WebSocket { path = path, heartbeat = False }


{-| Pinged every 30 seconds, as the server expects of a socket it closes when
it hears nothing, and dropped when the server stops answering.
-}
withHeartbeat : WebSocket -> WebSocket
withHeartbeat (WebSocket socket) =
    WebSocket { socket | heartbeat = True }


open : Context ctx -> WebSocket -> Cmd msg
open appState (WebSocket { path, heartbeat }) =
    wsOpen
        (E.object
            [ ( "key", E.string path )
            , ( "url", E.string (Request.authorizedUrl (Context.toServerInfo appState) path) )
            , ( "heartbeat", E.bool heartbeat )
            ]
        )


close : WebSocket -> Cmd msg
close (WebSocket { path }) =
    wsClose path


receive : Decoder a -> RawMsg -> WebSocket -> WebSocketMsg a
receive messageDecoder value (WebSocket { path }) =
    case D.decodeValue decodeWebSocketRawMsg value of
        Ok rawMsg ->
            if rawMsg.key == path then
                case rawMsg.type_ of
                    "open" ->
                        Open

                    "reopen" ->
                        Reopen

                    "disconnected" ->
                        Disconnected

                    "message" ->
                        case D.decodeValue messageDecoder rawMsg.data of
                            Ok action ->
                                Message action

                            Err _ ->
                                Ignore

                    "reconnect" ->
                        Reconnect

                    "close" ->
                        Close

                    _ ->
                        Ignore

            else
                Ignore

        Err _ ->
            Ignore


send : WebSocket -> E.Value -> Cmd msg
send (WebSocket { path }) value =
    wsSend ( path, value )


listen : (E.Value -> msg) -> Sub msg
listen =
    wsMessage


port wsOpen : E.Value -> Cmd msg


port wsClose : String -> Cmd msg


port wsSend : ( String, E.Value ) -> Cmd msg


port wsMessage : (E.Value -> msg) -> Sub msg
