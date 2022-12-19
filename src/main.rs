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

use crate::lex::Token;
use script_assist::*;
use std::fs;

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

    // Checking arguments like this is flawed, but given the user isn't an idiot,
    //     it should be fine.
    if args.len() == 2 {
        #[cfg(debug_assertions)]
        log("lexer", &format!("lexing: {file_name}"));
        let tokens = lex_file_weak(&src);
        #[cfg(debug_assertions)]
        log("lexer", &format!("done. {} tokens.", tokens.len()));

        generate_signature_file(
            // Convert Vec<(Token, &str)> to Vec<Token>
            &mut tokens.iter().map(|f| f.0.clone()).collect::<Vec<_>>(),
            file_name,
        );
    } else if args.len() == 3 {
        #[cfg(debug_assertions)]
        log("lexer", &format!("lexing: {file_name}"));
        let tokens = lex_file_weak(&src);
        #[cfg(debug_assertions)]
        log("lexer", &format!("done. {} tokens.", tokens.len()));

        if args[2].ends_with(".sigs") {
            update_globals(
                &args,
                &file_name,
                // Convert Vec<(Token, &str)> to Vec<Token>
                &mut tokens.iter().map(|f| f.0.clone()).collect::<Vec<_>>(),
            );
        } else {
            // Generate signature from any (potentially non global) token
            let token_to_find = &args[2];

            let sigs = generate_signature_from_slice(&tokens, token_to_find);
            // Print different messages depending on if multiple tokens with the same name were
            //     found. This probably shouldn't happen, given our lexer.
            if !sigs.is_empty() {
                println!("{}", sigs);
            } else {
                println!("Couldn't find signatures matching: {}", token_to_find);
            }
        }
    } else if args.len() == 4 {
        // Given an input like this:
        //     ./script-assist freemode_old.c freemode_new.c Global_XXXX
        //     Find the equivalent token from freemode_old.c in freemode_new.c
        let new_script = &args[2];
        let token_to_find = &args[3];

        let new_script_src = fs::read_to_string(new_script).unwrap();
        let (tokens, new_script_tokens) = std::thread::scope(|s| {
            let first= s.spawn(|| lex_file_weak(&src) );
            let second= s.spawn(|| lex_file_weak(&new_script_src) );
            (first.join(), second.join())
        });
        let (tokens, new_script_tokens) = (tokens.unwrap(), new_script_tokens.unwrap());
        let signature = generate_signature_from_slice(&tokens, token_to_find);
        let results = find_slice_from_packed_signature(&new_script_tokens, &signature);

        if results.is_empty() {
            println!("Could not find: {}", token_to_find);
            panic!();
        }

        let results = mode(&results);
        println!("{results}");
    } else {
        // Given an input like this:
        //     ./script-assist freemode.c SIGNATURE
        //     Return all matching results

        #[cfg(debug_assertions)]
        log("lexer", &format!("lexing: {file_name}"));
        let tokens = lex_file_weak(&src);
        #[cfg(debug_assertions)]
        log("lexer", &format!("done. {} tokens.", tokens.len()));

        // Extract signature from args
        let signature = &args[2..].join(" ");
        let all_found_tokens = find_slice_from_packed_signature(&tokens, signature);

        if !all_found_tokens.is_empty() {
            let mode = mode(&all_found_tokens);
            println!("{}", mode);
        } else {
            println!("Found no results for input: {}", signature);
        }
    }
}
