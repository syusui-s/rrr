#[macro_use]
extern crate nom;

use std::str;
use nom::ErrorKind;
use nom::{digit, oct_digit, hex_digit};

///////////////////////////////////////////////
// Nodes

#[derive(Debug, PartialEq)]
enum Literal {
    Fixnum(i32),
}

#[derive(Debug, PartialEq)]
enum Term {
    Literal(Literal),
    BinOpTree(Box<BinOpTree>),
}

#[derive(Debug, PartialEq, Clone)]
enum BinOpType {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, PartialEq)]
struct BinOpTree(BinOpType, Term, Term);

#[derive(Debug, PartialEq)]
struct Path(String);

#[derive(Debug, PartialEq)]
struct FnCall(Path, Vec<Term>);

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
    fn from_utf8(ary: &[u8]) -> Result<Self, &str> {
        str::from_utf8(ary)
            .map_err(|_| "input is not utf8 chars")
            .and_then(|input| Self::from_str(input))
    }

    fn from_str(input: &str) -> Result<Self, &str> {
        BINOPS
            .iter()
            .find(|&&(_, s)| s == input)
            .ok_or("no such binary operator")
            .map(|&(ref t, _)| t.clone())
    }
}

macro_rules! node {
    ( $type:path > $mem:path ) => (
        $type::$mem
    );
    ( $type:path > ( $( $val:expr ),* ) ) => (
        $type( $($val),* )
    );
    ( $type:path > $mem:path > ( $( $rem:tt ),* ) ) => (
        $type::$mem( $($rem),* )
    );
    ( $type:path > $mem:path > $( $rem:tt ),* ) => (
        $type::$mem( node!($mem > $($rem):*) )
    );
}

///////////////////////////////////////////////
// Printer

fn print_literal(lit: &Literal) {
    use Literal::*;

    match lit {
        &Fixnum(i) => {
            print!("{}", i);
        }
    }
}

fn print_term(term: &Term) {
    use Term::*;

    match term {
        &BinOpTree(ref tree) => print_optree(tree),
        &Literal(ref lit) => print_literal(lit),
    }
}

fn print_optree<'a>(tree: &'a BinOpTree) {
    let &BinOpTree(ref op, ref lhs, ref rhs) = tree;

    print!("(");
    print_term(&lhs);
    print!("{}", op.to_string());
    print_term(&rhs);
    print!(")");
}

///////////////////////////////////////////////
// Parser

named!(bin_digit, is_a!(&b"01"[..]));

named!(sign<Option<char>>, opt!(one_of!(&b"+-"[..])));

named!(lit_fixnum_value<i32>, 
    map_res!(
        alt!(
           do_parse!(
               tag!("0") >>
               res: alt!(
                   do_parse!(one_of!("bB")       >> s: bin_digit >> (s,  2_u32) ) |
                   do_parse!(one_of!("xX")       >> s: hex_digit >> (s, 16_u32) ) |
                   do_parse!(opt!(one_of!("oO")) >> s: oct_digit >> (s,  8_u32) )
               ) >>
               (res)
           ) |
           do_parse!(s: digit >> (s, 10u32) )
       ),
       |(num, radix)| {
           str::from_utf8(num)
               .map_err(|_| "fixnum literal is not utf8")
               .and_then(|s| {
                   i32::from_str_radix(s, radix)
                       .map_err(|_| "invalid fixnum")
               })
       }
    )
);

named!(lit_fixnum<Literal>,
    map!(
        ws!( tuple!( sign, lit_fixnum_value ) ),
        |(sign, val): (Option<char>, i32)| -> Literal {
            let ret = match sign {
                None | Some('+') =>  val,
                Some('-')        => -val,
                Some(_)          => panic!("Sign has unexpected character"),
            };
            Literal::Fixnum(ret)
        }
    )
);

named!(term<Term>,
       alt!(
           do_parse!( fixnum: lit_fixnum >> (Term::Literal(fixnum))) |
           do_parse!( tree: binop_tree   >> (Term::BinOpTree(Box::new(tree))))
      )
);

named!(binop<BinOpType>,
    map_res!(
        alt!(
            tag!("+") |
            tag!("-") |
            tag!("*") |
            tag!("/")
        ),
        BinOpType::from_utf8
    )
);

named!(binop_tree<BinOpTree>,
    do_parse!(
        lhs: term >>
        op: binop >>
        rhs: term >>
        (BinOpTree(op, lhs, rhs))
    )
);

mod test {
    use super::*;
    use nom::IResult;

    macro_rules! exact {
        ( $i:expr ) => ( IResult::Done(&b""[..], $i) );
    }

    #[test]
    fn bin_digit_matches_binary_value() {
        let val = &b"0011"[..];
        let expected = exact!(val);
        let actual = bin_digit(val);
        assert_eq!(expected, actual);
    }

    #[test]
    fn lit_fixnum_matches_integer_value() {
        let expected = exact!(Literal::Fixnum(42));
        let actual = lit_fixnum(b"42");
        assert_eq!(expected, actual)
    }

    #[test]
    fn lit_fixnum_matches_negative_value() {
        let expected = exact!(Literal::Fixnum(-42));
        let actual = lit_fixnum(b"-42");
        assert_eq!(expected, actual)
    }

    #[test]
    fn lit_fixnum_matches_binary_value() {
        let expected = exact!(Literal::Fixnum(42));
        let actual = lit_fixnum(b"0b101010");
        assert_eq!(expected, actual);
    }

    #[test]
    fn lit_fixnum_matches_binary_value_prefixed_upcase() {
        let expected = exact!(Literal::Fixnum(42));
        let actual = lit_fixnum(b"0B101010");
        assert_eq!(expected, actual);
    }


    #[test]
    fn lit_fixnum_matches_octet_value() {
        let expected = exact!(Literal::Fixnum(42));
        let actual = lit_fixnum(b"052");
        assert_eq!(expected, actual)
    }

    #[test]
    fn lit_fixnum_matches_octet_value_prefixed() {
        let expected = exact!(Literal::Fixnum(42));
        let actual = lit_fixnum(b"0o52");
        assert_eq!(expected, actual)
    }

    #[test]
    fn lit_fixnum_matches_octet_value_prefixed_upcase() {
        let expected = exact!(Literal::Fixnum(42));
        let actual = lit_fixnum(b"0O52");
        assert_eq!(expected, actual);
    }

    #[test]
    fn lit_fixnum_matches_hex_value() {
        let expected = exact!(Literal::Fixnum(42));
        let actual = lit_fixnum(b"0x2a");
        assert_eq!(expected, actual);
    }

    #[test]
    fn lit_fixnum_matches_hex_value_prefixed_upcase() {
        let expected = exact!(Literal::Fixnum(42));
        let actual = lit_fixnum(b"0X2a");
        assert_eq!(expected, actual);
    }

    #[test]
    fn lit_fixnum_matches_hex_value_negative() {
        let expected = exact!(Literal::Fixnum(-42));
        let actual = lit_fixnum(b"-0x2a");
        assert_eq!(expected, actual);
    }

    #[test]
    fn lit_fixnum_matches_hex_value_max() {
        let expected = exact!(Literal::Fixnum(i32::max_value()));
        let actual = lit_fixnum(b"0x7fffffff");
        assert_eq!(expected, actual);
    }

    #[test]
    fn lit_fixnum_not_matches_hex_value_invalid() {
        let expected = IResult::Error(nom::ErrorKind::MapRes);
        let actual = lit_fixnum(b"0xffffffff");
        assert_eq!(expected, actual);
    }

    #[test]
    fn binop_tree_matches_addition() {
        let expected = exact!(BinOpTree(BinOpType::from_str("+").unwrap(),
                                 Term::Literal(Literal::Fixnum(1)),
                                 Term::Literal(Literal::Fixnum(2))));
        let actual = binop_tree(b"1 + 2");
        assert_eq!(expected, actual);
    }
}
// let actual = flat_map!(input, eat_separator!(&b"_"[..]), is_a!(&b"01"[..]));

fn main() {
    println!("Hello");
}
