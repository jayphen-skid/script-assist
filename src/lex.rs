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
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use strum_macros::EnumIter;

fn calculate_hash<T: Hash>(t: &T) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    return s.finish();
}

fn hash(lex: &mut Lexer<Token>) -> Option<u64> {
    let n: String = lex.slice().parse().ok()?;
    Some(calculate_hash(&n))
}

fn underscore(lex: &mut Lexer<Token>) -> Option<String> {
    let str = lex.slice().to_string();
    let n = str.split("_").collect::<Vec<&str>>()[1];
    Some(n.to_owned())
}

fn int(lex: &mut Lexer<Token>) -> Option<String> {
    let n = lex
        .slice()
        .to_string()
        // Remove 'f' floating point suffix
        .replace("f", "")
        .replace(",", "");

    Some(n)
}

fn global(lex: &mut Lexer<Token>) -> Option<u64> {
    // Global_xxxx => xxxx
    let n: u64 = lex.slice()[7..].parse().ok()?;
    Some(n)
}

#[derive(
    EnumIter, Logos, Debug, PartialEq, Clone, Deserialize, Serialize, Ord, PartialOrd, Eq, Hash,
)]
pub enum Token {
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

    #[regex(r"func_[\d]+", underscore)]
    FunctionName(String),

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

    #[regex(r"(\w?)Local_[\d]+", underscore)]
    Local(String),

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

    #[regex(r"\[[\d]+\]")]
    ArrayDefLiteral,

    #[error]
    #[regex(r"[ \t\n\r\f]+", logos::skip)]
    Error,
}

impl Token {
    // Convert a token to it's signature value
    pub(crate) fn to_signature(&self) -> String {
        return match self {
            Token::Preprocess => "PPC".to_owned(),
            Token::VarType => "VT".to_owned(),
            Token::Void => "VD".to_owned(),
            Token::Long => "VLG".to_owned(),
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
            Token::Native(x) => format!("!NTV{x}"),
            Token::Global(_) => "GB".to_owned(),
            Token::And => "A".to_owned(),
            Token::Not => "N".to_owned(),
            Token::Or => "O".to_owned(),
            Token::GlobalOffset => "GO".to_owned(),
            Token::FunctionName(_) => "FN".to_owned(),
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
            Token::Less => "CL".to_owned(),
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
            Token::NumericLiteral(x) => format!("!L{x}"),
            Token::FtoV => "FV".to_owned(),
            Token::StackPush => "SP".to_owned(),
            Token::CallLoc => "CL".to_owned(),
            Token::StackValue => "SV".to_owned(),
            Token::IntToString => "ITS".to_owned(),
            Token::StringLiteral(x) => format!("!S{x}"),
            Token::Local(_) => "CLL".to_owned(),
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
            Token::ArrayDefLiteral => format!("AD"),
            Token::Error => "".to_owned(),
        };
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            _ => f.write_str(&format!("{:?}", self)),
        }?;
        Ok(())
    }
}
