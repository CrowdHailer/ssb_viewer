-module(secret_handshake).

-include_lib("eunit/include/eunit.hrl").

% takes ephemeral
hello({PublicKey, _SecretKey}, NetworkIdentifier) ->
  HMAC = libsodium_crypto_auth:crypto_auth(PublicKey, NetworkIdentifier),
  <<HMAC/binary, PublicKey/binary>>.

verify_hello(<<HMAC:32/binary, PeerEphemeral:32/binary>>, NetworkIdentifier) ->
  case libsodium_crypto_auth:verify(HMAC, PeerEphemeral, NetworkIdentifier) of
    0 ->
      {ok, PeerEphemeral};
    -1 ->
      {error, could_not_verify}
  end.

shared_ab(PeerPublicKey, {_, <<LHS:32/binary, RHS:32/binary>>}) ->
  libsodium_crypto_scalarmult:crypto_scalarmult(RHS, PeerPublicKey).


run_test() ->
  % From Python source
  NetworkIdentifier = base64:decode("1KHLiKZvAvjbY1ziZEHMXawbCEIM6qwjCDm3VYRan/s="),
  % From scuttlebutt protocol guide
  NetworkIdentifier = <<(erlang:list_to_integer("d4a1cb88a66f02f8db635ce26441cc5dac1b08420ceaac230839b755845a9ffb", 16)):256>>,

  ClientEphemeral = libsodium_crypto_sign_ed25519:keypair(),
  io:format("~p~n", [ClientEphemeral]),
  ServerEphemeral = libsodium_crypto_sign_ed25519:keypair(),

  ClientHello = hello(ClientEphemeral, NetworkIdentifier),
  {ok, ClientEphemeralPK} = verify_hello(ClientHello, NetworkIdentifier),
  io:format("~p~n", [ClientEphemeralPK]),

  ServerHello = hello(ServerEphemeral, NetworkIdentifier),
  {ok, ServerEphemeralPK} = verify_hello(ServerHello, NetworkIdentifier),

  ?assertEqual(shared_ab(ClientEphemeralPK, ServerEphemeral), shared_ab(ServerEphemeralPK, ClientEphemeral)),


  ?assertEqual(flunk, test).
