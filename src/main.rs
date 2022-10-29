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

use logos::{Lexer, Logos};
use serde_derive::{Deserialize, Serialize};
use std::collections::hash_map::DefaultHasher;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::hash::{Hash, Hasher};
use std::sync::atomic::Ordering;
use std::sync::{Arc, Mutex};

const SIGNATURE_SIZE: u8 = 13;

fn calculate_hash<T: Hash>(t: &T) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    return s.finish();
}

fn hash(lex: &mut Lexer<Token>) -> Option<u64> {
    let n: String = lex.slice().parse().ok()?;
    Some(calculate_hash(&n))
}

fn int(lex: &mut Lexer<Token>) -> Option<String> {
    let n = lex
        .slice()
        .to_string()
        // Remove 'f' floating point suffix
        .replace("f", "");
    Some(n)
}

fn array(lex: &mut Lexer<Token>) -> Option<u64> {
    let n: String = lex.slice().to_string();
    let n: u64 = n
        // [xxx] => xxx
        .trim_start_matches("[")
        .trim_end_matches("]")
        .parse()
        .ok()?;
    Some(n)
}

fn global(lex: &mut Lexer<Token>) -> Option<u64> {
    // Global_xxxx => xxxx
    let n: u64 = lex.slice()[7..].parse().ok()?;
    Some(n)
}

#[derive(Logos, Debug, PartialEq, Clone, Deserialize, Serialize)]
enum Token {
    #[regex(r"#[a-z]+ [\w ]+\s")]
    #[token("#endregion")]
    Preprocess,

    #[token("var")]
    VarType,

    #[token("void")]
    Void,

    #[token("long")]
    Long,

    #[token("BitTest")]
    BitTest,

    #[token("StringCopy")]
    StringCopy,

    #[token("int")]
    Int,

    #[token("float")]
    Float,

    #[token("bool")]
    Bool,

    #[token("NULL")]
    Null,

    #[token("-")]
    Neg,

    #[token("+")]
    Add,

    #[token("%")]
    Mod,

    #[token("*")]
    MulOrRef,

    #[token("/")]
    Div,
    // Supports natives with type:
    //     NAMESPACE::NAME
    //     NAMESPACE::HASH
    //     HASH
    #[regex(
        r"([A-Z_]+::[A-Z_0-9]+)|(unk_)*(0x[0-9A-F]+)|([A-Z]+::_0x[0-9A-F]+)",
        hash
    )]
    Native(u64),

    #[regex(r"Global_[\d]+", global)]
    Global(u64),

    #[token("&&")]
    And,

    #[token("!")]
    Not,

    #[token("||")]
    Or,

    #[regex(r"(\.|->)f_[\d]+")]
    GlobalOffset,

    #[regex(r"func_[\d]+")]
    FunctionName,

    #[token("char")]
    Char,

    #[token("Vector3")]
    Vector,

    #[token("Vector")]
    VectorConst,

    #[token("char*")]
    CharPtr,

    #[regex(r"struct<[\d]+>")]
    StructType,

    #[token("=")]
    Assignment,

    #[token(";")]
    EndStatement,

    #[token("{")]
    Open,

    #[token("}")]
    Close,

    #[token("[")]
    OpenOffset,

    #[regex(r"\[[\d]+")]
    OpenOffsetLiteral,

    #[token("]")]
    CloseOffset,

    #[token(">")]
    Greater,

    #[token("<")]
    Less,

    #[token("==")]
    Eq,

    #[token("|=")]
    OrEq,

    #[token("&=")]
    AndEq,

    // I assume this is a decompilation artifact
    #[regex(r"Jump @[\d]+; //[\w =]+")]
    Jump,

    #[regex(r"/\*[\d]+\*/")]
    Comment,

    #[token(",")]
    Comma,

    #[token("(")]
    OpenBracket,

    #[token("if")]
    If,

    #[token("while")]
    While,

    #[token("true")]
    True,

    #[token("false")]
    False,

    #[token(")")]
    CloseBracket,

    #[token("StringConCat")]
    StringConCat,

    #[token("MemCopy")]
    MemCopy,

    #[token("IntToFloat")]
    IntToFloat,

    #[token("StringIntConCat")]
    StringIntConCat,

    #[token("__EntryFunction__")]
    EntryPoint,

    // Common script decompilers seem to sometimes use complex numbers-
    //      not sure how this is specified in bytecode
    #[regex(r"([\d]+((\.|,)[\d]+)*(f)*)|(E(\+|-)[\d]+f)", int)]
    NumericLiteral(String),

    #[token("FtoV")]
    FtoV,

    #[token("Stack.Push")]
    StackPush,

    #[token("Call_Loc")]
    CallLoc,

    #[token("StackVal")]
    StackValue,

    #[token("IntToString")]
    IntToString,

    // I could probably black-list characters instead of white-listing them
    #[regex("\"[\\^':\\&\\(\\)  ?,\\\\\\|\\]\\[!*$<>~/\\.\\d\\w \t@-]*\"", hash)]
    StringLiteral(u64),

    #[regex(r"(\w?)Local_[\d]+")]
    Local,

    #[regex(r"(\w?)Var[\d]+")]
    Var,

    #[regex(r"(\w?)Param[\d]+")]
    Param,

    #[token(r"switch")]
    Switch,

    #[token(r"case")]
    Case,

    #[token(r":")]
    Colon,

    #[token(r"default")]
    Default,

    #[token(r"break")]
    Break,

    #[token(r"continue")]
    Continue,

    #[token(r"return")]
    Return,

    #[token("->")]
    FieldRef,

    #[token(r"&")]
    Deref,

    #[token(r"|")]
    BitOr,

    #[token(r"joaat")]
    Joaat,

    #[token(r"else")]
    Else,

    #[regex(r"\[[\d]+\]", array)]
    ArrayDefLiteral(u64),

    #[error]
    #[regex(r"[ \t\n\r\f]+", logos::skip)]
    Error,
}

impl ToString for Token {
    // Convert a token to it's signature value
    fn to_string(&self) -> String {
        return match self {
            Token::Preprocess => "PPC".to_owned(),
            Token::VarType => "VT".to_owned(),
            Token::Void => "VD".to_owned(),
            Token::Long => "LG".to_owned(),
            Token::BitTest => "BTST".to_owned(),
            Token::StringCopy => "STC".to_owned(),
            Token::Int => "IT".to_owned(),
            Token::Float => "FL".to_owned(),
            Token::Bool => "BL".to_owned(),
            Token::Null => "NL".to_owned(),
            Token::Neg => "NG".to_owned(),
            Token::Add => "AD".to_owned(),
            Token::Mod => "MD".to_owned(),
            Token::MulOrRef => "MOR".to_owned(),
            Token::Div => "DV".to_owned(),
            Token::Native(x) => format!("NTV{x}"),
            Token::Global(_) => "GB".to_owned(),
            Token::And => "A".to_owned(),
            Token::Not => "N".to_owned(),
            Token::Or => "O".to_owned(),
            Token::GlobalOffset => "GO".to_owned(),
            Token::FunctionName => "FN".to_owned(),
            Token::Char => "CH".to_owned(),
            Token::Vector => "VT".to_owned(),
            Token::VectorConst => "VC".to_owned(),
            Token::CharPtr => "CP".to_owned(),
            Token::StructType => "ST".to_owned(),
            Token::Assignment => "AT".to_owned(),
            Token::EndStatement => "ES".to_owned(),
            Token::Open => "O".to_owned(),
            Token::Close => "C".to_owned(),
            Token::OpenOffset => "OO".to_owned(),
            Token::OpenOffsetLiteral => "OL".to_owned(),
            Token::CloseOffset => "CL".to_owned(),
            Token::Greater => "G".to_owned(),
            Token::Less => "L".to_owned(),
            Token::Eq => "E".to_owned(),
            Token::OrEq => "OE".to_owned(),
            Token::AndEq => "AE".to_owned(),
            Token::Jump => "JMP".to_owned(),
            Token::Comment => "CMT".to_owned(),
            Token::Comma => "CA".to_owned(),
            Token::OpenBracket => "BR".to_owned(),
            Token::If => "IF".to_owned(),
            Token::While => "WH".to_owned(),
            Token::True => "TR".to_owned(),
            Token::False => "FE".to_owned(),
            Token::CloseBracket => "CB".to_owned(),
            Token::StringConCat => "SCC".to_owned(),
            Token::MemCopy => "MCY".to_owned(),
            Token::IntToFloat => "ITF".to_owned(),
            Token::StringIntConCat => "SIC".to_owned(),
            Token::EntryPoint => "EP".to_owned(),
            Token::NumericLiteral(x) => format!("L{x}"),
            Token::FtoV => "FV".to_owned(),
            Token::StackPush => "SP".to_owned(),
            Token::CallLoc => "CL".to_owned(),
            Token::StackValue => "SV".to_owned(),
            Token::IntToString => "ITS".to_owned(),
            Token::StringLiteral(x) => format!("S{x}"),
            Token::Local => "LL".to_owned(),
            Token::Var => "VR".to_owned(),
            Token::Param => "PM".to_owned(),
            Token::Switch => "SW".to_owned(),
            Token::Case => "CS".to_owned(),
            Token::Colon => "CLN".to_owned(),
            Token::Default => "DF".to_owned(),
            Token::Break => "BK".to_owned(),
            Token::Continue => "CI".to_owned(),
            Token::Return => "RN".to_owned(),
            Token::FieldRef => "FR".to_owned(),
            Token::Deref => "DR".to_owned(),
            Token::BitOr => "BO".to_owned(),
            Token::Joaat => "JT".to_owned(),
            Token::Else => "EL".to_owned(),
            Token::ArrayDefLiteral(x) => format!("AD{x}"),
            Token::Error => "".to_owned(),
        };
    }
}

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

fn generate_global_signatures(global: &str, tokens: &Vec<Token>) -> Option<Vec<String>> {
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

fn find_from_signature(signature: &str, tokens: &Vec<Token>) -> Option<Vec<u64>> {
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
