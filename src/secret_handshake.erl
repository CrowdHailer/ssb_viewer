-module(secret_handshake).

-include_lib("eunit/include/eunit.hrl").

% takes ephemeral
hello(PublicKey, NetworkIdentifier) ->
  HMAC = libsodium_crypto_auth:crypto_auth(PublicKey, NetworkIdentifier),
  <<HMAC/binary, PublicKey/binary>>.

verify_hello(<<HMAC:32/binary, PeerEphemeral:32/binary>>, NetworkIdentifier) ->
  case libsodium_crypto_auth:verify(HMAC, PeerEphemeral, NetworkIdentifier) of
    0 ->
      {ok, PeerEphemeral};
    -1 ->
      {error, could_not_verify}
  end.


curve25519keypair() ->
  {PK, SK} = libsodium_crypto_sign_ed25519:keypair(),
  {libsodium_crypto_sign_ed25519:pk_to_curve25519(PK), libsodium_crypto_sign_ed25519:sk_to_curve25519(SK)}.

run_test() ->
  % From Python source
  NetworkIdentifier = base64:decode("1KHLiKZvAvjbY1ziZEHMXawbCEIM6qwjCDm3VYRan/s="),
  % From scuttlebutt protocol guide
  NetworkIdentifier = <<(erlang:list_to_integer("d4a1cb88a66f02f8db635ce26441cc5dac1b08420ceaac230839b755845a9ffb", 16)):256>>,

  {ClientEphemeralPublicKey, ClientEphemeralSecretKey} = curve25519keypair(),

  {ServerEphemeralPublicKey, ServerEphemeralSecretKey} = curve25519keypair(),

  ClientHello = hello(ClientEphemeralPublicKey, NetworkIdentifier),
  {ok, ClientEphemeralPublicKey} = verify_hello(ClientHello, NetworkIdentifier),
  % io:format("~p~n", [ClientEphemeralPK]),

  ServerHello = hello(ServerEphemeralPublicKey, NetworkIdentifier),
  {ok, ServerEphemeralPublicKey} = verify_hello(ServerHello, NetworkIdentifier),

  {ServerLongTermPublicKey, ServerLongTermSecretKey} = libsodium_crypto_sign_ed25519:keypair(),

  Share_aB = libsodium_crypto_scalarmult:crypto_scalarmult(ClientEphemeralSecretKey, libsodium_crypto_sign_ed25519:pk_to_curve25519(ServerLongTermPublicKey)),
  Share_aB = libsodium_crypto_scalarmult:crypto_scalarmult(libsodium_crypto_sign_ed25519:sk_to_curve25519(ServerLongTermSecretKey), ClientEphemeralPublicKey),

  ?assertEqual(Share_aB, Share_aB),

  Share_ab = libsodium_crypto_scalarmult:crypto_scalarmult(ClientEphemeralSecretKey, ServerEphemeralPublicKey),
  Share_ab = libsodium_crypto_scalarmult:crypto_scalarmult(ServerEphemeralSecretKey, ClientEphemeralPublicKey),


  % libsodium_crypto_scalarmult:crypto_scalarmult(LHS, N).

  % ?assertEqual(shared_ab(ClientEphemeralPK, ServerEphemeral), shared_ab(ServerEphemeralPK, ClientEphemeral)),


  ?assertEqual(flunk, test).
