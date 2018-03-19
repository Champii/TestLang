import
  io
  net

type Client
  in     chan String
  socket net.Socket

  $  -> @socket.recv (@in<-)

  ~$ -> @in.close

  send: (msg) -> @socket.send msg

type God
  clients [Client]
  server  net.Server

  $: ->
    @server = net.createServer
      ..listen 1234
      ..onConnect (@clients.push Client) >> @waitFor
      ..onDisconnect socket -> @clients.remove @clients.find (.socket == socket)
      ..onError io.out

  ~$: -> @server.close

  waitFor: (c) ->
  for msg <- c.in
    clients.for (.send msg)

main = -> God
