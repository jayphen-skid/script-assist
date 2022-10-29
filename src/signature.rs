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

use crate::{Token, SIGNATURE_SIZE};

pub fn generate_global_signatures(global: &str, tokens: &Vec<Token>) -> Option<Vec<String>> {
    // The signature return buffer is a vector that contains every found global
    //     for a given signature
    let mut sig_ret_buf = vec![];

    // The signature buffer is a rotating stream of the last X number of previous tokens
    let mut sig_buffer = vec![];

    // Extract the numeric ID from a string with a format of: Global_XXXX
    let i_id = global.split("_").collect::<Vec<&str>>()[1];
    let i_id = i_id.trim_end_matches("\n").parse::<u64>().ok()?;

    for token in tokens {
        // If we already found 4 signatures for a global, we can stop
        //     this is basically just for performance
        if sig_ret_buf.len() > 3 {
            break;
        }

        // If our vector is greater than our desired signature size,
        //     remove the last element from the vector
        if sig_buffer.len() == SIGNATURE_SIZE as usize {
            sig_buffer.remove(0);
        }

        // If we found a token which is a Global, and the globals ID is what
        //     we are making a signature for. Generate a signature and append
        //     it to the vector
        if let Token::Global(id) = token {
            if id.clone() == i_id {
                sig_ret_buf.push(
                    // Generate signature from sig_buffer
                    sig_buffer
                        .iter()
                        .map(|f: &&Token| f.to_string())
                        .collect::<Vec<_>>()
                        .join(" "),
                );
            }
        }
        sig_buffer.push(token);
    }
    return Some(sig_ret_buf);
}

pub fn find_from_signature(signature: &str, tokens: &Vec<Token>) -> Option<Vec<u64>> {
    // Always keep track of the last X number of scanned elements whilst iterating
    //     used for signature generation
    let mut sig_buffer = vec![];
    let i_len = signature.split(" ").collect::<Vec<&str>>().len() + 1;

    let mut globals = vec![];
    for token in tokens {
        // If the vector's size is greater than the inputted signatures size,
        //     remove the first element of the vector
        if sig_buffer.len() == i_len {
            sig_buffer.remove(0);
        }

        // If our current token is a Global, try generating a signature at our
        //     current position, and see if it matches the signature that was inputted,
        //     if it does, then add it to a vector
        if let Token::Global(id) = token {
            // Generate signature from token buffer
            let current_signature = sig_buffer
                .iter()
                .map(|f: &&Token| f.to_string())
                .collect::<Vec<_>>()
                .join(" ");

            // If the signature matches out inputted signature, add it to the vector
            if signature == current_signature {
                globals.push(id.clone());
            }
        }
        sig_buffer.push(token);
    }
    Some(globals)
}
