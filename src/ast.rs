use std::str;

#[derive(Debug, PartialEq)]
pub enum Literal {
    Fixnum(i32),
}

#[derive(Debug, PartialEq)]
pub enum Term {
    Literal(Literal),
    BinOpTree(Box<BinOpTree>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinOpType {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, PartialEq)]
pub struct BinOpTree(pub BinOpType, pub Term, pub Term);

#[derive(Debug, PartialEq)]
pub struct Path(pub Vec<String>);

#[derive(Debug, PartialEq)]
pub struct MethodCall(pub Path, pub Vec<Term>);

const BINOPS: [(BinOpType, &str); 4] = [(BinOpType::Add, "+"),
                                        (BinOpType::Sub, "-"),
                                        (BinOpType::Mul, "*"),
                                        (BinOpType::Div, "/")];

///////////////////////////////////////////////
// Impl

impl ToString for BinOpType {
    fn to_string(&self) -> String {
        BINOPS
            .iter()
            .find(|&&(ref t, _)| t == self)
            .unwrap()
            .1
            .to_owned()
    }
}

impl BinOpType {
    pub fn from_utf8(ary: &[u8]) -> Result<Self, &str> {
        str::from_utf8(ary)
            .map_err(|_| "input is not utf8 chars")
            .and_then(|input| Self::from_str(input))
    }

    pub fn from_str(input: &str) -> Result<Self, &str> {
        BINOPS
            .iter()
            .find(|&&(_, s)| s == input)
            .ok_or("no such binary operator")
            .map(|&(ref t, _)| t.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn binop_type_from_utf8() {
        assert_eq!(Ok(BinOpType::Add), BinOpType::from_utf8(&b"+"[..]));
        assert_eq!(Ok(BinOpType::Sub), BinOpType::from_utf8(&b"-"[..]));
        assert_eq!(Ok(BinOpType::Mul), BinOpType::from_utf8(&b"*"[..]));
        assert_eq!(Ok(BinOpType::Div), BinOpType::from_utf8(&b"/"[..]));
    }
}
