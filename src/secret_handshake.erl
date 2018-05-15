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

  % SETUP key based identities
  {ClientLongTermPublicKey, ClientLongTermSecretKey} = libsodium_crypto_sign_ed25519:keypair(),
  {ServerLongTermPublicKey, ServerLongTermSecretKey} = libsodium_crypto_sign_ed25519:keypair(),

  % SETUP ephemeral keys for a session
  {ClientEphemeralPublicKey, ClientEphemeralSecretKey} = curve25519keypair(),
  {ServerEphemeralPublicKey, ServerEphemeralSecretKey} = curve25519keypair(),

  % 1. Client hello
  ClientHello = hello(ClientEphemeralPublicKey, NetworkIdentifier),
  {ok, ClientEphemeralPublicKey} = verify_hello(ClientHello, NetworkIdentifier),

  % 2. Server hello
  ServerHello = hello(ServerEphemeralPublicKey, NetworkIdentifier),
  {ok, ServerEphemeralPublicKey} = verify_hello(ServerHello, NetworkIdentifier),

  % Shared secret derivation
  SharedSecret_aB = libsodium_crypto_scalarmult:crypto_scalarmult(ClientEphemeralSecretKey, libsodium_crypto_sign_ed25519:pk_to_curve25519(ServerLongTermPublicKey)),
  SharedSecret_aB = libsodium_crypto_scalarmult:crypto_scalarmult(libsodium_crypto_sign_ed25519:sk_to_curve25519(ServerLongTermSecretKey), ClientEphemeralPublicKey),

  SharedSecret_ab = libsodium_crypto_scalarmult:crypto_scalarmult(ClientEphemeralSecretKey, ServerEphemeralPublicKey),
  SharedSecret_ab = libsodium_crypto_scalarmult:crypto_scalarmult(ServerEphemeralSecretKey, ClientEphemeralPublicKey),

  % 3. Client authenticate
  HashedSecret_ab = libsodium_crypto_hash_sha256:crypto_hash_sha256(SharedSecret_ab),
  DetatchedSignature_A = libsodium_crypto_sign_ed25519:detached(<<NetworkIdentifier/binary, ServerLongTermPublicKey/binary, HashedSecret_ab/binary>>, ClientLongTermSecretKey),
  % User afternm because that's what the Python one does
  ThisKey = libsodium_crypto_hash_sha256:crypto_hash_sha256(<<NetworkIdentifier/binary, SharedSecret_ab/binary, SharedSecret_aB/binary>>),
  % No idea what easy_afternm does that is different to afternm
  ClientBox = libsodium_crypto_box:easy_afternm(<<DetatchedSignature_A/binary, ClientLongTermPublicKey/binary>>, <<0:192>>, ThisKey),
  ?assertEqual(112, erlang:byte_size(ClientBox)),

  % Server assertion of client authentication
  <<DetatchedSignature_A:64/binary, ClientLongTermPublicKey:32/binary>> = libsodium_crypto_box:open_easy_afternm(ClientBox, <<0:192>>, ThisKey),
  % Again -1 for unverified
  0 = libsodium_crypto_sign_ed25519:verify_detached(DetatchedSignature_A, <<NetworkIdentifier/binary, ServerLongTermPublicKey/binary, HashedSecret_ab/binary>>, ClientLongTermPublicKey),

  % Shared secret derivation
  SharedSecret_Ab = libsodium_crypto_scalarmult:crypto_scalarmult(libsodium_crypto_sign_ed25519:sk_to_curve25519(ClientLongTermSecretKey), ServerEphemeralPublicKey),
  SharedSecret_Ab = libsodium_crypto_scalarmult:crypto_scalarmult(ServerEphemeralSecretKey, libsodium_crypto_sign_ed25519:pk_to_curve25519(ClientLongTermPublicKey)),

  % 4. Server accept
  DetatchedSignature_B = libsodium_crypto_sign_ed25519:detached(<<NetworkIdentifier/binary, DetatchedSignature_A/binary, ClientLongTermPublicKey/binary, HashedSecret_ab/binary>>, ServerLongTermSecretKey),
  ThisKey2 = libsodium_crypto_hash_sha256:crypto_hash_sha256(<<NetworkIdentifier/binary, SharedSecret_ab/binary, SharedSecret_aB/binary, SharedSecret_Ab/binary>>),
  ServerBox = libsodium_crypto_box:easy_afternm(<<DetatchedSignature_B/binary>>, <<0:192>>, ThisKey2),
  ?assertEqual(80, erlang:byte_size(ServerBox)),
  DetatchedSignature_B = libsodium_crypto_box:open_easy_afternm(ServerBox, <<0:192>>, ThisKey2),
  0 = libsodium_crypto_sign_ed25519:verify_detached(DetatchedSignature_B, <<NetworkIdentifier/binary, DetatchedSignature_A/binary, ClientLongTermPublicKey/binary, HashedSecret_ab/binary>>, ServerLongTermPublicKey),

  ?assertEqual(flunk, test).
