# TestLang

Mini toy language with no more value than what I learned during its conception

It is inspired from Livescript, Golang and Haskell

# Exemple

```coffee
add  = (x,y) -> x + y
main = (a) -> add a 8
```

```
$> stack build && stack exec SteelTest-exe test.s
```

```haskell
TestLang> :t main
main : Int -> Int
TestLang> main 1
9 :: Int
TestLang> add3 = add 3
TestLang> :t add3
<<closure>> :: Int -> Int
TestLang> add3 4
7 :: Int
TestLang>
```

# Tests

```
$> stack test
```

# Futur dreams

This is what I intend this language to look like

An exemple of a Chat Server with *fictive code*

```coffee
import
  io
  net

type Client
  in     chan String
  socket net.Socket

  $  -> @socket.recv (@in<-)
  ~$ -> @in.close

  send :: String -> ()
  send: (msg) -> @socket.send msg

type ChatServer
  clients [Client]
  server  net.Server

  $: ->
    @server = net.createServer
      ..listen 1234
      ..onConnect (@clients.push Client) >> @readLoop
      ..onDisconnect @removeClient
      ..onError io.out

  ~$: -> @server.close

  readLoop :: Client -> ()
  readLoop: (c) ->
    c.in.for (msg) ->
      clients.for (.send msg)

  removeClient :: net.Socket -> ()
  removeClient: (s) ->
    @clients.remove @clients.find (.socket == s)

main = -> ChatServer
```