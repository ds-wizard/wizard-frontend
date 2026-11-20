// Exercises packages/elm-common/js/ports/websocket.js against a fake socket and
// a fake clock: what Elm hears when a connection opens, drops and opens again,
// the heartbeat that finds a connection the browser still thinks is open, and
// the waking that skips the wait when the browser is back online.

const timers = new Map()
let now = 0
let nextTimer = 1

global.setTimeout = (fn, delay) => {
    const id = nextTimer++
    timers.set(id, {fn, at: now + delay})
    return id
}
global.clearTimeout = (id) => timers.delete(id)
global.setInterval = (fn, delay) => {
    const id = nextTimer++
    const schedule = () => timers.set(id, {fn: () => { schedule(); fn() }, at: now + delay})
    schedule()
    return id
}
global.clearInterval = (id) => timers.delete(id)

function advance(ms) {
    const until = now + ms
    for (;;) {
        const due = [...timers.entries()].filter(([, t]) => t.at <= until).sort((a, b) => a[1].at - b[1].at)[0]
        if (!due) break
        timers.delete(due[0])
        now = due[1].at
        due[1].fn()
    }
    now = until
}

const windowListeners = {}
const documentListeners = {}
global.window = {
    location: {href: 'https://example.com/projects'},
    addEventListener: (event, fn) => { windowListeners[event] = fn }
}
global.document = {
    visibilityState: 'visible',
    addEventListener: (event, fn) => { documentListeners[event] = fn }
}

const sockets = []
global.WebSocket = class {
    constructor(url) {
        this.url = url
        this.readyState = 0
        this.sent = []
        this.listeners = {}
        this.closed = false
        sockets.push(this)
    }

    addEventListener(event, fn) { this.listeners[event] = fn }
    removeEventListener(event) { delete this.listeners[event] }
    send(data) { this.sent.push(data) }
    close() { this.closed = true }

    // The server's side.
    open() { this.readyState = 1; this.fire('open', {}) }
    receive(data) { this.fire('message', {data: JSON.stringify(data)}) }
    drop(code) { this.readyState = 3; this.fire('close', {code}) }
    fire(event, payload) { if (this.listeners[event]) this.listeners[event](payload) }
}

const received = []
const subscriptions = {}
const app = {
    ports: {
        wsOpen: {subscribe: (fn) => { subscriptions.wsOpen = fn }},
        wsClose: {subscribe: (fn) => { subscriptions.wsClose = fn }},
        wsSend: {subscribe: (fn) => { subscriptions.wsSend = fn }},
        wsMessage: {send: (msg) => received.push(msg)}
    }
}

require('../js/ports/websocket.js')(app)

const checks = []
const key = '/projects/p/websocket'
const types = () => received.map((msg) => msg.type)
const last = () => sockets[sockets.length - 1]

function open(heartbeat) {
    subscriptions.wsOpen({key, url: '/api' + key + '?Authorization=Bearer%20t', heartbeat})
    last().open()
}

// Opening, and the URL it opens.
open(true)
checks.push(['the socket is opened at the ws(s) url', last().url.startsWith('wss://example.com/api/projects/p/websocket')])
checks.push(['the first open is an open', types().join() === 'open'])
checks.push(['a heartbeat socket pings at once, to learn whether the server answers', last().sent.join() === 'ping'])

// The pong is the port's own, and a frame it cannot read is not Elm's problem.
last().receive({type: 'Pong_ServerAction'})
last().fire('message', {data: 'not json'})
last().receive({type: 'Success_ServerAction', data: {type: 'SetUserList_ServerProjectMessage', data: []}})
checks.push(['a pong and an unreadable frame do not reach Elm', types().join() === 'open,message'])

// A ping the server stops answering: the connection is dropped, and Elm hears
// at once that it is down.
advance(30000)
checks.push(['the heartbeat pings every 30 s', last().sent.filter((m) => m === 'ping').length === 2])
advance(10000)
checks.push(['a ping unanswered for 10 s drops the connection', last().closed && types().join() === 'open,message,disconnected'])

// It is opened again after the delay, and that open is a reopen.
advance(1000)
checks.push(['after the delay Elm is asked to open it again', types()[types().length - 1] === 'reconnect'])
open(true)
checks.push(['the open after a drop is a reopen', types()[types().length - 1] === 'reopen'])
checks.push(['the old socket is not reused', sockets.length === 2])

// A message for a socket that is down is not kept: the page sends it again.
last().drop(1006)
subscriptions.wsSend([key, {type: 'SetContent'}])
advance(1000)
open(true)
checks.push(['a message sent while the socket is down is not sent later by the port', !last().sent.some((m) => m.includes('SetContent'))])
subscriptions.wsSend([key, {type: 'SetContent'}])
checks.push(['a message for an open socket is sent', last().sent.some((m) => m.includes('SetContent'))])

// A server that never answers a ping is pinged and nothing more.
advance(30000)
advance(20000)
checks.push(['without a pong ever, no ping drops the connection', !last().closed && types()[types().length - 1] === 'reopen'])

// Back online: the wait before reconnecting is skipped.
last().drop(1006)
checks.push(['a drop says disconnected', types()[types().length - 1] === 'disconnected'])
windowListeners.online()
checks.push(['back online it reconnects without waiting', types()[types().length - 1] === 'reconnect'])
open(true)

// 1011 and 1012 reconnect; 1008 is final, and says close.
last().drop(1012)
advance(30000)
checks.push(['1012 reconnects', types().slice(-2).join() === 'disconnected,reconnect'])
open(true)
last().drop(1008)
advance(60000)
checks.push(['1008 is final', types()[types().length - 1] === 'close'])

// A socket without a heartbeat is never pinged.
open(false)
advance(120000)
checks.push(['a socket without a heartbeat is not pinged', last().sent.length === 0])
checks.push(['an open after a final close is an open again', types()[types().length - 1] === 'open'])

// Closing it from Elm leaves nothing behind.
subscriptions.wsClose(key)
const pending = timers.size
advance(120000)
checks.push(['closing leaves no timers', pending === 0])

const failures = checks.filter(([, ok]) => !ok)

if (failures.length > 0) {
    console.error('websocket port:')
    failures.forEach(([name]) => console.error(`  FAIL  ${name}`))
    console.error('  received: ' + types().join(', '))
    process.exit(1)
}

console.log('websocket port OK')
