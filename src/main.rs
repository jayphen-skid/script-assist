/*
 * Copyright (c) 2022.
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

mod lex;
mod signature;

use lex::*;
use logos::Logos;
use serde_derive::{Deserialize, Serialize};
use signature::*;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::sync::atomic::Ordering;
use std::sync::{Arc, Mutex};

const SIGNATURE_SIZE: u8 = 13;

fn log(name: &str, content: &str) {
    println!("[{name}] {content}");
}
#[derive(Debug, Clone, Serialize, Deserialize)]
struct GlobalEntry {
    global: u64,
    signatures: Vec<String>,
}

impl GlobalEntry {
    fn new(id: u64) -> Self {
        GlobalEntry {
            global: id,
            signatures: vec![],
        }
    }
}

fn main() {
    // Put command line arguments into Vec
    let args = std::env::args().collect::<Vec<String>>();

    if args.len() == 1 {
        println!("script-assist - Invalid arguments.");
        println!("To generate signatures (example):");
        println!("\t./script-assist freemode.c");
        println!("To update globals (example):");
        println!("\t./script-assist freemode_new.c freemode.c.sigs");
        return;
    }

    let file_name = &args[1];
    let src = fs::read_to_string(file_name).unwrap();

    log("lexer", &format!("lexing: {file_name}"));
    let mut tokens = vec![];
    let mut lex = Token::lexer(&src);

    while let Some(token) = lex.next() {
        if token == Token::Error {
            log(
                "lexer",
                &format!(
                    "Internal lexing error, tried to lex: '{}'",
                    // Add some padding to the span to show a wider error region,
                    //     makes debugging easier
                    &lex.source()[lex.span().start..lex.span().end + 10]
                ),
            );
            return;
        }
        tokens.push(token);
    }

    // These tokens can be removed because their existence is effectively a given,
    //     if the context is known
    tokens = tokens
        .iter()
        .filter(|x| {
            !matches!(
                x,
                Token::OpenBracket
                    | Token::CloseBracket
                    | Token::Close
                    | Token::Open
                    | Token::EndStatement
            )
        })
        .map(|x| x.clone())
        .collect::<Vec<Token>>();

    log("lexer", &format!("done. {} tokens.", tokens.len()));

    // Checking arguments like this is flawed, but given the user isn't an idiot,
    //     it should be fine.
    if args.len() == 2 {
        generate_signature_file(&mut tokens, file_name);
    } else if args.len() == 3 {
        update_globals(&args, &file_name, &mut tokens);
    }
}

fn update_globals(args: &Vec<String>, file_name: &&String, tokens: &mut Vec<Token>) {
    // Final output buffer
    let buf = Arc::new(Mutex::new(String::from("")));

    // File containing serialized JSON with a Vec<GlobalEntry>
    let sig_file = fs::read_to_string(&args[2]).unwrap();

    // Deserialize JSON
    let globals = serde_json::from_str::<Vec<GlobalEntry>>(&sig_file).unwrap();

    let count = Arc::new(std::sync::atomic::AtomicI32::new(0));

    let total = globals.len();

    for gbl in globals {
        // RC's need to be cloned to follow the borrow checker
        let gbl = gbl.clone();
        let buf = buf.clone();
        let tokens = tokens.clone();
        let count = count.clone();

        std::thread::spawn(move || {
            count.fetch_add(1, Ordering::SeqCst);
            let mut globals = vec![];

            // Add every found signature to vector
            for signature in gbl.signatures {
                globals.append(&mut find_from_signature(&signature, &tokens).unwrap());
            }

            // Display error if no signatures were able to be found
            if globals.is_empty() {
                buf.lock()
                    .unwrap()
                    .push_str(&format!("{} <=> [not found]\n", gbl.global));
                return;
            }

            // Obtain the mode of the vector, which is just the most occurring value.
            //     Multiple signatures are usually obtained for each global, so we pick
            //     the one which points to a specific global the most
            let mode = mode(&globals);
            buf.lock()
                .unwrap()
                .push_str(&format!("{} <=> {mode}\n", gbl.global));

            log(
                "global-updater",
                &format!(
                    "{} <=> {mode} ({}/{})",
                    gbl.global,
                    count.load(Ordering::SeqCst),
                    total
                ),
            );
        });
    }
    // Prevents us from finishing before all threads are complete
    while total != count.load(Ordering::SeqCst) as usize {}

    fs::write(&format!("{}.map", file_name), &*buf.lock().unwrap()).unwrap();
    log(
        "global-updater",
        &format!("done. saved to: {file_name}.map"),
    );
}

// This code is pasted from the internet, I forgot where
//     I got it from. ¯\_(ツ)_/¯
fn mode(numbers: &[u64]) -> &u64 {
    let mut occurrences = HashMap::new();

    for value in numbers {
        *occurrences.entry(value).or_insert(0) += 1;
    }

    occurrences
        .into_iter()
        .max_by_key(|&(_, count)| count)
        .map(|(val, _)| val)
        .expect("Cannot compute the mode of zero numbers")
}

fn generate_signature_file(tokens: &Vec<Token>, file_name: &str) {
    // Thread safe reference counted vector containing each GlobalEntry
    //     a GlobalEntry contains the global's ID and a vector of strings
    //     contains it's signatures
    let signatures = Arc::new(Mutex::new(vec![]));

    // Stores the total number of globals in script (unique)
    //     used for logging
    let total = Arc::new(std::sync::atomic::AtomicI32::new(0));

    // Stores the total number of processed globals in script
    //     used or logging
    let current = Arc::new(std::sync::atomic::AtomicI32::new(0));

    // A hash-set allows us to add items, while ignoring duplicates
    let mut globals = HashSet::new();

    for tok in tokens {
        if let Token::Global(id) = tok {
            // Duplicates aren't inserted, this process can be replaced
            //     with a Vec and checking if vec.contains(id)
            globals.insert(id);
        }
    }

    // Set total to length of hashset
    total.store(globals.len() as i32, Ordering::SeqCst);

    // For every unique global in script as id
    for id in globals.iter() {
        let id = **id;

        // Clone reference counters to follow borrow-checker
        let tokens = tokens.clone();
        let signatures = Arc::clone(&signatures);
        let total = Arc::clone(&total);
        let current = Arc::clone(&current);
        {
            std::thread::spawn(move || {
                // xxx => Global_XXXX
                let str = format!("Global_{id}");

                // Generate signature given Global_XXXX and the provided tokens
                let sig = generate_global_signatures(&str, &tokens).unwrap();

                // A hashset is used so that the signatures generated from `find_from_signature`
                //     aren't duplicates. As GTA's scripts have a lot of duplicate code.
                //     This check should probably be done in the function instead of here
                //     because of the hard signature amount limit that the function returns.
                let mut lc_signatures = HashSet::new();

                // This entire block could probably be replaced with a stream filter
                for s in sig {
                    // Check if the signature actually finds the correct global
                    //     This slows down the program, and should never happen
                    //     But.. generating signatures isn't a common thing so
                    //     I think it's better to care about quality of the
                    //     signatures
                    let gbl = find_from_signature(&s, &tokens).unwrap();

                    // If a signature doesn't lead to the proper global, don't add it
                    if !gbl.contains(&id) {
                        continue;
                    }
                    lc_signatures.insert(s);
                }
                // Generate a GlobalEntry containing the global's ID and
                //     the vector of signature strings it has
                let mut entry = GlobalEntry::new(id);
                entry.signatures = lc_signatures.into_iter().collect::<Vec<String>>();

                // Append 'current'
                current.fetch_add(1, Ordering::SeqCst);

                log(
                    "sig-gen",
                    &format!(
                        "found {} signatures for: Global_{} ({}/{})",
                        entry.signatures.len(),
                        id,
                        current.load(Ordering::SeqCst),
                        total.load(Ordering::SeqCst)
                    ),
                );

                // Push the GlobalEntry to signatures
                signatures.lock().unwrap().push(entry);
            });
        }
    }

    // Make sure all the threads are done before continuing
    while total.load(Ordering::SeqCst) != current.load(Ordering::SeqCst) {}

    // Serialize the Vec<GlobalEntry> to JSON
    let sigs = signatures.lock().unwrap();
    let json = serde_json::to_string_pretty(&sigs.iter().collect::<Vec<&GlobalEntry>>()).unwrap();

    // Save to file
    fs::write(format!("{file_name}.sigs"), json).unwrap();
    log("sig-gen", &format!("saved to: {file_name}.sigs"));
}
