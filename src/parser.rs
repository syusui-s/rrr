use ast::*;

use std::str;
use nom::{digit, oct_digit, hex_digit};

///////////////////////////////////////////////
// Printer

/*
fn print_literal(lit: &Literal) {
    use self::Literal::*;

    match lit {
        &Fixnum(i) => {
            print!("{}", i);
        }
    }
}

fn print_term(term: &Term) {
    use self::Term::*;

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
*/

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
           // TODO return custom error. Currently ignored err and returned ErrorKind::MapRes;
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


#[cfg(test)]
mod test {
    use super::*;
    use nom::{IResult, ErrorKind};

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
        let expected = IResult::Error(ErrorKind::MapRes);
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
