module Common.Api.WebSocket.Outbox exposing
    ( Outbox
    , Status(..)
    , ack
    , add
    , clear
    , connected
    , disconnected
    , empty
    , expired
    , isEmpty
    , isSettled
    , member
    , messages
    , retry
    , status
    , subscriptions
    , tick
    , uuids
    )

{-| What a page sent over its websocket and the server has not acknowledged yet:
the messages themselves, which the page encodes when it sends them, so that it
can also apply them again on top of what it loads after a reconnect.

The server echoes every change it stored, so a change is acknowledged when its
echo arrives (`ack`), and refused when an error frame names it (also `ack`: it
is not coming, and the page says why). Until then it stays here, and it is sent
again: everything at once when the socket opens again (`connected`), and one at
a time when its echo is overdue (`tick`), after 10, 20 and 40 seconds and then
every minute. Sending a change twice is safe — the server stores it once and
echoes it again. So an echo can come twice, and the second one is not
somebody else's change: the outbox remembers what it settled (`isSettled`).

After three overdue echoes the outbox is `Stalled`: something is wrong that
waiting does not fix, and the page offers to save now (`retry`), which sends
everything again. A change older than an hour (`expired`) is not sent again at
all: the server keeps what it needs to recognize a change sent twice only that
long, and a change sent after it could overwrite a newer one. The page gives up
on it and loads what the server has.

The outbox keeps no time of its own: `tick` brings it, every second while
anything is waiting (`subscriptions`). A change is timed from the first tick
after it was added, which is close enough for waits counted in seconds.

-}

import Time
import Uuid exposing (Uuid)


type Outbox a
    = Outbox
        { entries : List (Entry a)
        , connection : Connection

        -- The uuids settled last, newest first, for an echo that comes twice.
        , settled : List Uuid
        }


type alias Entry a =
    { uuid : Uuid
    , message : a
    , addedAt : Maybe Time.Posix

    -- When it was last sent, or, while the socket is down, when it would have
    -- been: an echo is overdue the same way whether or not it could come.
    , sentAt : Maybe Time.Posix

    -- How many times its echo was overdue.
    , overdue : Int
    }


type Connection
    = NotYetOpen
    | Up
    | Down


{-| Where the page's changes are, for the saving indicator.

  - `Idle` — nothing waiting.
  - `Sending` — something waiting, and it is not late.
  - `Retrying` — something is late or the socket is down, and it is being sent
    again.
  - `Stalled` — something has been late three times: saving is not working.
  - `Reconnecting` — nothing waiting, but the socket is down, so nothing made
    now would arrive either.

-}
type Status
    = Idle
    | Sending
    | Retrying
    | Stalled
    | Reconnecting


empty : Outbox a
empty =
    Outbox { entries = [], connection = NotYetOpen, settled = [] }


{-| A message sent, known by the uuid its echo will carry.
-}
add : Uuid -> a -> Outbox a -> Outbox a
add uuid message (Outbox outbox) =
    Outbox
        { outbox
            | entries =
                List.filter (\entry -> entry.uuid /= uuid) outbox.entries
                    ++ [ { uuid = uuid, message = message, addedAt = Nothing, sentAt = Nothing, overdue = 0 } ]
        }


{-| The message is settled: its echo arrived, or the server refused it.
-}
ack : Uuid -> Outbox a -> Outbox a
ack uuid (Outbox outbox) =
    if List.any (\entry -> entry.uuid == uuid) outbox.entries then
        Outbox
            { outbox
                | entries = List.filter (\entry -> entry.uuid /= uuid) outbox.entries
                , settled = List.take rememberSettled (uuid :: outbox.settled)
            }

    else
        Outbox outbox


{-| The message was waiting and is settled: an echo of it now is a second one.
-}
isSettled : Uuid -> Outbox a -> Bool
isSettled uuid (Outbox outbox) =
    List.member uuid outbox.settled


member : Uuid -> Outbox a -> Bool
member uuid (Outbox outbox) =
    List.any (\entry -> entry.uuid == uuid) outbox.entries


isEmpty : Outbox a -> Bool
isEmpty (Outbox outbox) =
    List.isEmpty outbox.entries


{-| The uuids waiting, oldest first.
-}
uuids : Outbox a -> List Uuid
uuids (Outbox outbox) =
    List.map .uuid outbox.entries


{-| The messages waiting, oldest first.
-}
messages : Outbox a -> List a
messages (Outbox outbox) =
    List.map .message outbox.entries


{-| Nothing waits any more: the page gave up on it.
-}
clear : Outbox a -> Outbox a
clear (Outbox outbox) =
    Outbox { outbox | entries = [] }


{-| The socket is open (again): everything waiting goes now, in the order it
was made, and its wait starts over.
-}
connected : Outbox a -> ( Outbox a, List a )
connected (Outbox outbox) =
    ( Outbox { outbox | entries = List.map restart outbox.entries, connection = Up }
    , List.map .message outbox.entries
    )


disconnected : Outbox a -> Outbox a
disconnected (Outbox outbox) =
    Outbox { outbox | connection = Down }


{-| Save now: everything waiting goes again, and is given another three tries
before it is stalled again.
-}
retry : Outbox a -> ( Outbox a, List a )
retry (Outbox outbox) =
    ( Outbox { outbox | entries = List.map restart outbox.entries }
    , if outbox.connection == Up then
        List.map .message outbox.entries

      else
        []
    )


restart : Entry a -> Entry a
restart entry =
    { entry | sentAt = Nothing, overdue = 0 }


{-| A second has passed (or more, in a tab the browser runs timers in only now
and then): an echo overdue sends its message again.
-}
tick : Time.Posix -> Outbox a -> ( Outbox a, List a )
tick now (Outbox outbox) =
    let
        step entry ( entries, resend ) =
            let
                timed =
                    { entry
                        | addedAt = Just (Maybe.withDefault now entry.addedAt)
                        , sentAt = Just (Maybe.withDefault now entry.sentAt)
                    }
            in
            if since timed.sentAt now >= echoTimeout timed.overdue then
                ( { timed | sentAt = Just now, overdue = timed.overdue + 1 } :: entries
                , if outbox.connection == Up then
                    timed.message :: resend

                  else
                    resend
                )

            else
                ( timed :: entries, resend )

        ( steppedEntries, resent ) =
            List.foldl step ( [], [] ) outbox.entries
    in
    ( Outbox { outbox | entries = List.reverse steppedEntries }
    , List.reverse resent
    )


status : Outbox a -> Status
status (Outbox outbox) =
    if List.isEmpty outbox.entries then
        if outbox.connection == Down then
            Reconnecting

        else
            Idle

    else if List.any (\entry -> entry.overdue >= stallAfter) outbox.entries then
        Stalled

    else if outbox.connection == Down || List.any (\entry -> entry.overdue > 0) outbox.entries then
        Retrying

    else
        Sending


{-| Something has waited longer than it may be sent again for.
-}
expired : Time.Posix -> Outbox a -> Bool
expired now (Outbox outbox) =
    List.any (\entry -> entry.addedAt /= Nothing && since entry.addedAt now >= resendWindow) outbox.entries


subscriptions : (Time.Posix -> msg) -> Outbox a -> Sub msg
subscriptions toMsg (Outbox outbox) =
    if List.isEmpty outbox.entries then
        Sub.none

    else
        Time.every 1000 toMsg



-- TIMING


{-| How long an echo may take before its message is sent again, by how many
times it has already been late: 10, 20 and 40 seconds, then a minute.
-}
echoTimeout : Int -> Int
echoTimeout overdue =
    case overdue of
        0 ->
            10000

        1 ->
            20000

        2 ->
            40000

        _ ->
            60000


stallAfter : Int
stallAfter =
    3


rememberSettled : Int
rememberSettled =
    200


{-| An hour: how long the server keeps what it recognizes a change sent twice
by.
-}
resendWindow : Int
resendWindow =
    60 * 60 * 1000


since : Maybe Time.Posix -> Time.Posix -> Int
since mbThen now =
    case mbThen of
        Just then_ ->
            Time.posixToMillis now - Time.posixToMillis then_

        Nothing ->
            0
