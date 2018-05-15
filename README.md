ssb_viewer
=====

All the parts necessary to build a viewer for the secure scuttlebutt protocol.

> An identity is an Ed25519 key pair

- [How ed25519 keys work](https://blog.mozilla.org/warner/2011/11/29/ed25519-keys/)
- [Language bindings for libsodium](https://download.libsodium.org/doc/bindings_for_other_languages/)
- [Designing a secret handshake](http://dominictarr.github.io/secret-handshake-paper/shs.pdf)
- [Run a test suite over implementation](https://github.com/AljoschaMeyer/shs1-testsuite)

Build
-----

    $ rebar3 compile
