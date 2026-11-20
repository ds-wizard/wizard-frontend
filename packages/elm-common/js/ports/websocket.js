// Websockets are known by a key (their path under the API URL); the URL, with
// the token, comes with every open. When a connection drops, the port says so
// at once (`disconnected`), waits and then asks Elm to open it again
// (`reconnect`), so the new attempt carries the current token. An open after a
// drop is a `reopen`: whatever the server sent in the gap is lost, and the
// page has to catch up.
//
// A socket opened with a heartbeat pings the server every 30 s — the server
// closes a connection it hears nothing from for 90 s — and once the server has
// answered a ping, a ping it does not answer in time means the connection is
// dead even though the browser has not noticed: it is dropped and opened again.
// A server that never answers (an older one) is only pinged.

// The server ended the connection for good: normally, on lost access, or on a
// message over its limit.
const FINAL_CLOSE_CODES = [1000, 1008, 1009]

// The server is shutting down; another one takes the connection straight away.
const GOING_AWAY = 1001

// What a connection dropped here, without the server, closes with.
const ABNORMAL_CLOSURE = 1006

const OPEN = 1

const MIN_DELAY = 500
const MAX_DELAY = 30000

const PING_INTERVAL = 30000
const PONG_TIMEOUT = 10000

const PING = 'ping'
const PONG_TYPE = 'Pong_ServerAction'

module.exports = function (app) {
    app.ports.wsOpen?.subscribe(wsOpen)
    app.ports.wsClose?.subscribe(wsClose)
    app.ports.wsSend?.subscribe(wsSend)

    const websockets = {}
    const eventListeners = {}
    const attempts = {}
    const timers = {}

    // Whether the key's socket was open before: its next open is a reopen.
    const opened = {}

    // The heartbeat of a key: its ping interval, whether the server answered a
    // ping on the current connection, and the timer waiting for an answer.
    const heartbeats = {}
    const pingIntervals = {}
    const pongSeen = {}
    const pongTimers = {}

    window.addEventListener('online', wake)
    document.addEventListener('visibilitychange', function () {
        if (document.visibilityState === 'visible') wake()
    })

    function sendMsg(key, type, data) {
        if (app.ports.wsMessage) {
            app.ports.wsMessage.send({key: key, type: type, data: data})
        }
    }

    function wsOpen({key, url, heartbeat}) {
        if (websockets[key]) return

        clearTimeout(timers[key])
        delete timers[key]
        heartbeats[key] = !!heartbeat

        const ws = new WebSocket(toWebSocketUrl(url))
        websockets[key] = ws

        eventListeners[key] = {
            'open': function () {
                attempts[key] = 0
                pongSeen[key] = false
                sendMsg(key, opened[key] ? 'reopen' : 'open', {})
                opened[key] = true

                if (heartbeats[key]) {
                    // At once, to learn whether the server answers pings.
                    ping(key)
                    pingIntervals[key] = setInterval(function () { ping(key) }, PING_INTERVAL)
                }
            },
            'message': function (event) {
                let data
                try {
                    data = JSON.parse(event.data)
                } catch (e) {
                    return
                }

                if (data && data.type === PONG_TYPE) {
                    pongSeen[key] = true
                    clearTimeout(pongTimers[key])
                    delete pongTimers[key]
                } else {
                    sendMsg(key, 'message', data)
                }
            },
            'close': function (event) {
                closed(key, event.code)
            },
            'error': function () {
            }
        }

        bindEventListeners(ws, eventListeners[key])
    }

    // The connection is gone: for good, or to be opened again after a delay.
    function closed(key, code) {
        const ws = websockets[key]
        if (ws && eventListeners[key]) {
            unbindEventListeners(ws, eventListeners[key])
        }
        delete eventListeners[key]
        delete websockets[key]
        stopHeartbeat(key)

        if (FINAL_CLOSE_CODES.includes(code)) {
            delete attempts[key]
            delete opened[key]
            sendMsg(key, 'close', null)
        } else {
            sendMsg(key, 'disconnected', {code: code})
            const attempt = attempts[key] || 0
            attempts[key] = attempt + 1
            const delay = code === GOING_AWAY && attempt === 0 ? 0 : reconnectDelay(attempt)
            timers[key] = setTimeout(function () {
                delete timers[key]
                sendMsg(key, 'reconnect', null)
            }, delay)
        }
    }

    function ping(key) {
        const websocket = websockets[key]
        if (!websocket || websocket.readyState !== OPEN) return

        websocket.send(PING)

        if (pongSeen[key] && !pongTimers[key]) {
            pongTimers[key] = setTimeout(function () {
                delete pongTimers[key]
                drop(key)
            }, PONG_TIMEOUT)
        }
    }

    // A connection that stopped answering. Closing it cleanly would wait for
    // the dead peer, so it is let go and treated as closed now.
    function drop(key) {
        const ws = websockets[key]
        if (!ws) return

        if (eventListeners[key]) {
            unbindEventListeners(ws, eventListeners[key])
            delete eventListeners[key]
        }
        try {
            ws.close()
        } catch (e) {
            // Already closing.
        }
        closed(key, ABNORMAL_CLOSURE)
    }

    // The browser is back online, or the tab is shown again: a socket waiting
    // to reconnect goes now, and an open one is asked whether it is alive.
    function wake() {
        for (const key of Object.keys(timers)) {
            clearTimeout(timers[key])
            delete timers[key]
            attempts[key] = 0
            sendMsg(key, 'reconnect', null)
        }
        for (const key of Object.keys(websockets)) {
            if (heartbeats[key]) ping(key)
        }
    }

    function stopHeartbeat(key) {
        clearInterval(pingIntervals[key])
        delete pingIntervals[key]
        clearTimeout(pongTimers[key])
        delete pongTimers[key]
        delete pongSeen[key]
    }

    // A message for a socket that is not open is not kept: the page keeps what
    // it has not seen acknowledged and sends it again once the socket opens.
    function wsSend([key, message]) {
        const websocket = websockets[key]
        if (websocket && websocket.readyState === OPEN) {
            websocket.send(JSON.stringify(message))
        }
    }

    function wsClose(key) {
        if (websockets[key]) {
            if (eventListeners[key]) {
                unbindEventListeners(websockets[key], eventListeners[key])
                delete eventListeners[key]
            }
            websockets[key].close()
            delete websockets[key]
        }

        clearTimeout(timers[key])
        delete timers[key]
        delete attempts[key]
        delete opened[key]
        delete heartbeats[key]
        stopHeartbeat(key)
    }
}

// Exponential, from 500 ms to 30 s, each delay somewhere between half and the
// whole of its step, so that clients dropped together do not return together.
function reconnectDelay(attempt) {
    const step = Math.min(MAX_DELAY, MIN_DELAY * Math.pow(2, attempt))
    return Math.max(MIN_DELAY, step / 2 + Math.random() * step / 2)
}

// The API URL may be relative or http(s); the socket takes ws(s).
function toWebSocketUrl(url) {
    const wsUrl = new URL(url, window.location.href)
    wsUrl.protocol = wsUrl.protocol === 'https:' ? 'wss:' : 'ws:'
    return wsUrl.toString()
}

function bindEventListeners(ws, eventListeners) {
    for (const [event, eventListener] of Object.entries(eventListeners)) {
        ws.addEventListener(event, eventListener)
    }
}

function unbindEventListeners(ws, eventListeners) {
    for (const [event, eventListener] of Object.entries(eventListeners)) {
        ws.removeEventListener(event, eventListener)
    }
}
