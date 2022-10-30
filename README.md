# script-assist
Update global index's through GTA versions by providing each global a list of signatures.

## Usage instructions
To open the help menu:
```
./script-assist
```
### Update global, function, local, etc.. (Recommended)
```
./script-assist old_script.c new_script.c token
```
Example:
```
./script-assist freemode_old.c freemode_new.c Global_1852989
```

script-assist supports any lex-able token in GTA V's scripts. Some valid examples include:
* `func_xxxx`
* `Local_xxxx`
* `Global_xxxx`
* `"Strings!"` (on Windows, " characters need to be escaped by \\)


### Update every global in a script

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