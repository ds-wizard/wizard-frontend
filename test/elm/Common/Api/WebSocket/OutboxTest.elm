module Common.Api.WebSocket.OutboxTest exposing (outboxTests)

import Common.Api.WebSocket.Outbox as Outbox exposing (Outbox)
import Expect
import Json.Encode as E
import Test exposing (Test, describe, test)
import Time
import Uuid exposing (Uuid)


uuidA : Uuid
uuidA =
    Uuid.fromUuidString "8a7b3b1f-1d6c-4a7e-9d3e-0f6c2b8b4a01"


uuidB : Uuid
uuidB =
    Uuid.fromUuidString "8a7b3b1f-1d6c-4a7e-9d3e-0f6c2b8b4a02"


message : String -> E.Value
message =
    E.string


at : Int -> Time.Posix
at seconds =
    Time.millisToPosix (1000000 + seconds * 1000)


{-| The outbox after a run of ticks, with everything each tick sent again.
-}
ticks : List Int -> Outbox E.Value -> ( Outbox E.Value, List (List E.Value) )
ticks seconds outbox =
    List.foldl
        (\second ( current, sent ) ->
            let
                ( next, resent ) =
                    Outbox.tick (at second) current
            in
            ( next, sent ++ [ resent ] )
        )
        ( outbox, [] )
        seconds


open : Outbox E.Value -> Outbox E.Value
open =
    Outbox.connected >> Tuple.first


outboxTests : Test
outboxTests =
    describe "Outbox"
        [ test "an echo settles its message, and only its own" <|
            \_ ->
                Outbox.empty
                    |> open
                    |> Outbox.add uuidA (message "a")
                    |> Outbox.add uuidB (message "b")
                    |> Outbox.ack uuidA
                    |> Outbox.uuids
                    |> Expect.equal [ uuidB ]
        , test "nothing waiting is idle, something waiting is sending" <|
            \_ ->
                ( Outbox.status (open Outbox.empty)
                , Outbox.status (Outbox.add uuidA (message "a") (open Outbox.empty))
                )
                    |> Expect.equal ( Outbox.Idle, Outbox.Sending )
        , test "an open sends everything waiting, oldest first" <|
            \_ ->
                Outbox.empty
                    |> Outbox.add uuidA (message "a")
                    |> Outbox.add uuidB (message "b")
                    |> Outbox.connected
                    |> Tuple.second
                    |> Expect.equal [ message "a", message "b" ]
        , test "an overdue echo sends its message again after 10, 20 and 40 seconds, then it is stalled" <|
            \_ ->
                let
                    ( outbox, sent ) =
                        Outbox.empty
                            |> open
                            |> Outbox.add uuidA (message "a")
                            |> ticks [ 0, 9, 10, 29, 30, 69, 70 ]
                in
                ( List.map List.length sent, Outbox.status outbox )
                    |> Expect.equal ( [ 0, 0, 1, 0, 1, 0, 1 ], Outbox.Stalled )
        , test "a late echo is retrying before it is stalled" <|
            \_ ->
                Outbox.empty
                    |> open
                    |> Outbox.add uuidA (message "a")
                    |> ticks [ 0, 10 ]
                    |> Tuple.first
                    |> Outbox.status
                    |> Expect.equal Outbox.Retrying
        , test "while the socket is down nothing is sent, and the echo is late all the same" <|
            \_ ->
                let
                    ( outbox, sent ) =
                        Outbox.empty
                            |> open
                            |> Outbox.add uuidA (message "a")
                            |> Outbox.disconnected
                            |> ticks [ 0, 10, 30, 70 ]
                in
                ( List.concat sent, Outbox.status outbox )
                    |> Expect.equal ( [], Outbox.Stalled )
        , test "a socket down with nothing waiting is reconnecting" <|
            \_ ->
                Outbox.empty
                    |> open
                    |> Outbox.disconnected
                    |> Outbox.status
                    |> Expect.equal Outbox.Reconnecting
        , test "opening again gives a stalled message another three tries" <|
            \_ ->
                Outbox.empty
                    |> open
                    |> Outbox.add uuidA (message "a")
                    |> ticks [ 0, 10, 30, 70 ]
                    |> Tuple.first
                    |> Outbox.disconnected
                    |> open
                    |> Outbox.status
                    |> Expect.equal Outbox.Sending
        , test "save now sends everything again and starts the tries over" <|
            \_ ->
                let
                    ( outbox, sent ) =
                        Outbox.empty
                            |> open
                            |> Outbox.add uuidA (message "a")
                            |> ticks [ 0, 10, 30, 70 ]
                            |> Tuple.first
                            |> Outbox.retry
                in
                ( sent, Outbox.status outbox )
                    |> Expect.equal ( [ message "a" ], Outbox.Sending )
        , test "save now while the socket is down sends nothing; the open will" <|
            \_ ->
                Outbox.empty
                    |> open
                    |> Outbox.add uuidA (message "a")
                    |> Outbox.disconnected
                    |> Outbox.retry
                    |> Tuple.second
                    |> Expect.equal []
        , test "a message is timed from the first tick after it was added, and expires after an hour" <|
            \_ ->
                let
                    ( outbox, _ ) =
                        Outbox.empty
                            |> open
                            |> Outbox.add uuidA (message "a")
                            |> ticks [ 100 ]
                in
                ( Outbox.expired (at 99) Outbox.empty
                , Outbox.expired (at (100 + 3599)) outbox
                , Outbox.expired (at (100 + 3600)) outbox
                )
                    |> Expect.equal ( False, False, True )
        , test "a message added again replaces the first, at the end" <|
            \_ ->
                Outbox.empty
                    |> Outbox.add uuidA (message "a")
                    |> Outbox.add uuidB (message "b")
                    |> Outbox.add uuidA (message "a2")
                    |> Outbox.messages
                    |> Expect.equal [ message "b", message "a2" ]
        , test "a settled message is remembered, for an echo that comes twice" <|
            \_ ->
                let
                    outbox =
                        Outbox.empty
                            |> open
                            |> Outbox.add uuidA (message "a")
                            |> Outbox.ack uuidA
                in
                ( Outbox.isSettled uuidA outbox, Outbox.isSettled uuidB outbox, Outbox.member uuidA outbox )
                    |> Expect.equal ( True, False, False )
        ]
