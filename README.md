# pch-ws-chat
The chat example from Parallel and Concurrent Haskell, but on WebSockets

## Preparation
```bash
git clone https://github.com/Elvecent/pch-ws-chat.git
cd psh-ws-chat
(optional, if using Nix) nix-shell
cabal v2-configure
```
## Running
```bash
cabal v2-run
```
Then open 127.0.0.1:8080 in browser, access the console and use `send()` to send strings. Use multiple tabs to connect as different users.
