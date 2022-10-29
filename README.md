# script-assist
Update global index's through GTA versions by providing each global a list of signatures.

## Usage instructions
To open the help menu:
```
./script-assist
```
To generate global signatures given a script file:
```
./script-assist DECOMPILED_SCRIPT.c
```
This will generate a `DECOMPILED_SCRIPT.c.sigs` file

To then update your globals:
```
./script-assist DECOMPILED_SCRIPT.c.sigs NEW_DECOMPILED_SCRIPT.c
```
This will generate a `NEW_DECOMPILED_SCRIPT.c.map` file, which is human-readable.

## Build instructions
* Download & install [rustup](https://www.rust-lang.org/tools/install)
* You should probably then run `rustup update`
```
git clone https://github.com/jayphen-skid/script-assist
cd script-assist
cargo build --release
cd target/release/
script-assist.exe
```