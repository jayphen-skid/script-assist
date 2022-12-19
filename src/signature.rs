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
use std::borrow::Borrow;
use crate::{lex, Token, MAX_GENERATED_SIGNATURES, SIGNATURE_SIZE};
use regex::Regex;
use std::default::Default;
use strum::IntoEnumIterator;

pub fn generate_signatures(looking_for: &Token, tokens: &Vec<Token>) -> Option<Vec<String>> {
    // The signature return buffer is a vector that contains every found global
    //     for a given signature
    let mut sig_ret_buf = vec![];

    // The signature buffer is a rotating stream of the last X number of previous tokens
    let mut sig_buffer = vec![];

    for token in tokens {
        // If we already found X signatures for a global, we can stop
        //     this is basically just for performance
        if sig_ret_buf.len() > MAX_GENERATED_SIGNATURES as usize {
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
        if looking_for == token {
            // Generate signature from sig_buffer
            let signature = sig_buffer
                .iter()
                .map(|f: &&Token| f.to_signature())
                .collect::<Vec<_>>()
                .join(" ");

            if !sig_ret_buf.contains(&signature) {
                sig_ret_buf.push(signature);
            }

            // This block acts to remove patterns which are contained in other
            //     patterns.
            let tmp_sig_ret_buf = sig_ret_buf.clone();

            for sig_1 in &tmp_sig_ret_buf {
                for sig_2 in &tmp_sig_ret_buf {
                    if sig_1.len() > sig_2.len() && sig_1.contains(sig_2) {
                        sig_ret_buf = sig_ret_buf
                            .iter()
                            .filter(|f| f != &sig_2).cloned()
                            .collect::<Vec<String>>();
                    }
                }
            }
        }
        sig_buffer.push(token);
    }
    Some(sig_ret_buf)
}

pub fn token_to_byte<T: Borrow<Token>>(token: T) -> u8 {
    match token.borrow() {
        Token::Preprocess => 0,
        Token::VarType => 1,
        Token::Void => 2,
        Token::Long => 3,
        Token::BitTest => 4,
        Token::StringCopy => 5,
        Token::Int => 6,
        Token::Float => 7,
        Token::Bool => 8,
        Token::Null => 9,
        Token::Neg => 10,
        Token::Add => 11,
        Token::Mod => 12,
        Token::MulOrRef => 13,
        Token::Div => 14,
        Token::Native(_) => 15,
        Token::Global(_) => 16,
        Token::And => 17,
        Token::Not => 18,
        Token::Or => 19,
        Token::GlobalOffset => 20,
        Token::FunctionName(_) => 21,
        Token::Char => 22,
        Token::Vector => 23,
        Token::VectorConst => 24,
        Token::CharPtr => 25,
        Token::StructType => 26,
        Token::Assignment => 27,
        Token::EndStatement => 28,
        Token::Open => 29,
        Token::Close => 30,
        Token::OpenOffset => 31,
        Token::OpenOffsetLiteral => 32,
        Token::CloseOffset => 33,
        Token::Greater => 34,
        Token::Less => 35,
        Token::Eq => 36,
        Token::OrEq => 37,
        Token::AndEq => 38,
        Token::Jump => 39,
        Token::Comment => 40,
        Token::Comma => 41,
        Token::OpenBracket => 42,
        Token::If => 43,
        Token::While => 44,
        Token::True => 45,
        Token::False => 46,
        Token::CloseBracket => 47,
        Token::StringConCat => 48,
        Token::MemCopy => 49,
        Token::IntToFloat => 50,
        Token::StringIntConCat => 51,
        Token::EntryPoint => 52,
        Token::NumericLiteral(_) => 53,
        Token::FtoV => 54,
        Token::StackPush => 55,
        Token::CallLoc => 56,
        Token::StackValue => 57,
        Token::IntToString => 58,
        Token::StringLiteral(_) => 59,
        Token::Local(_) => 60,
        Token::Param => 61,
        Token::Switch => 62,
        Token::Case => 63,
        Token::Colon => 64,
        Token::Default => 65,
        Token::Break => 66,
        Token::Continue => 67,
        Token::Return => 68,
        Token::FieldRef => 69,
        Token::Deref => 70,
        Token::BitOr => 71,
        Token::Joaat => 72,
        Token::Else => 73,
        Token::Var => 74,
        Token::ArrayDefLiteral => 75,
        Token::Error => 76,
    }
}

pub fn find_from_signature<'a, T: IntoIterator<Item = &'a u8>>(
    signature: &str,
    tokens: T,
) -> Option<Vec<usize>> {
    // Always keep track of the last X number of scanned elements whilst iterating
    //     used for signature generation
    let mut sig_buffer: Vec<u8> = Vec::with_capacity(signature.len() * 2);
    let i_len = signature.split(' ').count() + 1;

    let signature_ = generate_token_vec_from_signature(signature)?;
    let signature = signature_.iter().map(token_to_byte).collect::<Vec<_>>();

    let mut globals = vec![];

    for (count, token) in tokens.into_iter().enumerate() {
        //let token = token_to_byte(token);
        // If the vector's size is greater than the inputted signatures size,
        //     remove the first element of the vector
        if sig_buffer.len() == i_len {
            sig_buffer.remove(0);
        }

        // If our current token is a Global, try generating a signature at our
        //     current position, and see if it matches the signature that was inputted,
        //     if it does, then add it to a vector

        // If the signature matches out inputted signature, add it to the vector
        let smallest_size = if signature.len() > sig_buffer.len() {
            sig_buffer.len()
        } else {
            signature.len()
        };
        for i in 0..smallest_size {
            if signature[i] != sig_buffer[i] {
                break;
            }
            if i == smallest_size - 1 {
                globals.push(count);
            }
        }


        sig_buffer.push(*token);
        //print!("{} ", token.to_signature());
    }

    Some(globals)
}

fn generate_token_vec_from_signature(signature: &str) -> Option<Vec<Token>> {
    let mut out: Vec<Token> = vec![];
    let parts = signature.split(' ').collect::<Vec<&str>>();
    let regex = Regex::new(r"\d+").unwrap();
    'outer: for part in &parts {
        for tok in lex::Token::iter() {
            let tok: Token = tok;
            match tok {
                //Token::Native(_) => {
                //    if part.starts_with(r"!NTV") {
                //        out.push(Token::Native(
                //			0 // regex.find(part).unwrap().as_str().parse().unwrap(),
                //        ));
                //        continue 'outer;
                //    }
                //}
                Token::NumericLiteral(_) => {
                    if part.starts_with("!L") {
                        out.push(Token::NumericLiteral(
                            regex.find(part).unwrap().as_str().parse().unwrap(),
                        ));
                        continue 'outer;
                    }
                }
                Token::StringLiteral(_) => {
                    if part.starts_with(r"!S") {
                        out.push(Token::StringLiteral(
                            regex.find(part).unwrap().as_str().parse().unwrap(),
                        ));
                        continue 'outer;
                    }
                }
                _ => {
                    if part == &tok.to_signature() {
                        out.push(tok);
                        continue 'outer;
                    }
                }
            };
        }
        println!("unknown: {}", part);
    }
    if parts.len() != out.len() {
        return None;
    }
    Some(out)
}
