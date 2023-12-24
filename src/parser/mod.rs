use std::path::PathBuf;
use nom::{
    IResult,
    branch::alt,
    bytes::complete::tag,
    multi::{many0, many1, separated_list1},
    combinator::{recognize, verify},
    character::complete::{char, alpha1, alphanumeric1, one_of, space0, space1, multispace1, multispace0},
};

use crate::types::*;

pub fn parse_file(path: &PathBuf) -> Result<Program, ParseError> {
    match std::fs::read_to_string(path) {
	Ok(contents) => {
	    match program(&contents) {
		Ok(("", program)) => Ok(program),
		_ => Err(ParseError::ErrorParsingFile),
	    }
	},
	Err(_) => Err(ParseError::ErrorReadingFile)
    }
}

pub enum ParseError {
    ErrorReadingFile,
    ErrorParsingFile,
}

fn spaced_comma(input: &str) -> IResult<&str, ()> {
    let (input, _) = many0(char(' '))(input)?;
    let (input, _) = char(',')(input)?;
    let (input, _) = many0(char(' '))(input)?;

    Ok((input, ()))
}

fn program(input: &str) -> IResult<&str, Program> {
    let (input, rules) = separated_list1(multispace1, rule)(input)?;
    let (input, _) = multispace0(input)?;

    Ok((input, Program { rules }))
}

fn rule(input: &str) -> IResult<&str, Rule> {
    let (input, head) = basic_type(input)?;
    let (input, _) = space1(input)?;
    let (input, _) = tag(":-")(input)?;
    let (input, _) = space1(input)?;
    let (input, body) = separated_list1(spaced_comma, body)(input)?;
    let (input, _) = space0(input)?;
    let (input, _) = tag(".")(input)?;

    let rule = Rule {
	head,
	guard: Vec::new(),
	body
    };

    Ok((input, rule))
}

fn body(input: &str) -> IResult<&str, BodyExpr> {
    alt((body_print, body_str_call, body_assign, body_atom_call))(input)
}

fn body_str_call(input: &str) -> IResult<&str, BodyExpr> {
    let (input, str_type) = str_type(input)?;

    Ok((input, BodyExpr::Call(str_type)))
}

fn body_atom_call(input: &str) -> IResult<&str, BodyExpr> {
    let (input, atom) = atom(input)?;

    Ok((input, BodyExpr::Call(atom)))
}

fn body_assign(input: &str) -> IResult<&str, BodyExpr> {
    let (input, var) = var(input)?;
    let (input, _) = space1(input)?;
    let (input, _) = tag(":=")(input)?;
    let (input, _) = space1(input)?;
    let (input, basic_type) = basic_type(input)?;

    Ok((input, BodyExpr::Assign { var, value: basic_type }))
}

fn body_print(input: &str) -> IResult<&str, BodyExpr> {
    let (input, _) = tag("print ")(input)?;

    let (input, data) = basic_type(input)?;

    Ok((input, BodyExpr::Print {
	msg: data
    }))
}

fn basic_type(input: &str) -> IResult<&str, BasicType> {
    alt((str_type, number, atom, var))(input)
}

fn atom(input: &str) -> IResult<&str, BasicType> {
    let (input, atom) = verify(alphanumeric1, |s: &str| s.chars().next().unwrap().is_ascii_lowercase())(input)?;

    Ok((input, BasicType::Atom(atom.to_string())))
}

fn var(input: &str) -> IResult<&str, BasicType> {
    let (input, var) = verify(alphanumeric1, |s: &str| s.chars().next().unwrap().is_ascii_uppercase())(input)?;

    Ok((input, BasicType::Var{ name: var.to_string(), value: VarValue::new()}))
}

fn number(input: &str) -> IResult<&str, BasicType> {
    let (input, num) = nom::character::complete::i64(input)?;

    Ok((input, BasicType::Number(num)))
}

fn str_type(input: &str) -> IResult<&str, BasicType> {
    let (input, name) = atom(input)?;

    let BasicType::Atom(name) = name else {
	panic!("Not an Atom");
    };
    
    let (input, _) = char('(')(input)?;

    let (input, args) = separated_list1(spaced_comma, basic_type)(input)?;
    let (input, _) = char(')')(input)?;

    let str_type = BasicType::Str {
	name,
	args
    };

    Ok((input, str_type))
}


#[test]
fn parse_number() {
    let input = "42";
    let result = basic_type(input);
    assert_eq!(result, Ok(("", BasicType::Number(42))));
}

#[test]
fn parse_atom() {
    let input = "stop".into();
    let result = basic_type(input);
    assert_eq!(result, Ok(("", BasicType::Atom("stop".into()))));
}

#[test]
fn parse_var() {
    let input = "Var".into();
    let result = basic_type(input);
    assert_eq!(result, Ok(("", BasicType::Var("Var".into()))));
}

#[test]
fn parse_str_type() {
    let input = "person(adrian, arroyo, calle)";
    let result = basic_type(input);
    assert_eq!(result, Ok(("", BasicType::Str {
	name: "person".into(),
	args: vec![BasicType::Atom("adrian".into()), BasicType::Atom("arroyo".into()), BasicType::Atom("calle".into())]
    })));
}

#[test]
fn parse_rule() {
    let input = "print(X) :- print X.";
    let result = rule(input);
    let expected = Rule {
	head: BasicType::Str {
	    name: "print".into(),
	    args: vec![BasicType::Var("X".into())],
	},
	guard: vec![],
	body: vec![BodyExpr::Print {
	    msg: BasicType::Var("X".into())
	}]
    };
    assert_eq!(result, Ok(("", expected)));
}


#[test]
fn parse_program() {
    let input = "print(X) :- print X.\nprint(X) :- print X.";
    let result = program(input);
    let expected = Program {
	rules: vec![
	    Rule {
		head: BasicType::Str {
		    name: "print".into(),
		    args: vec![BasicType::Var("X".into())]
		},
		guard: vec![],
		body: vec![BodyExpr::Print {
		    msg: BasicType::Var("X".into())
		}]
	    },
	    Rule {
		head: BasicType::Str {
		    name: "print".into(),
		    args: vec![BasicType::Var("X".into())]
		},
		guard: vec![],
		body: vec![BodyExpr::Print {
		    msg: BasicType::Var("X".into())
		}]
	    }],
    };
    assert_eq!(result, Ok(("", expected)));
}

#[test]
fn parse_program2() {
    let input = include_str!("../../examples/hello.duero");
    let result = program(input);
    let expected = Program {
	rules: vec![Rule {
	    head: BasicType::Atom("main".into()),
	    guard: vec![],
	    body: vec![
		BodyExpr::Print {
		    msg: BasicType::Atom("hello".into())
		},
	        BodyExpr::Print {
		    msg: BasicType::Atom("world".into())
		}]
	}],
    };
    assert_eq!(result, Ok(("", expected)));
}

#[test]
fn parse_program3() {
    let input = include_str!("../../examples/f1.duero");
    let result = program(input);
    let expected = Program {
	rules: vec![Rule {
	    head: BasicType::Atom("main".into()),
	    guard: vec![],
	    body: vec![
		BodyExpr::Print {
		    msg: BasicType::Atom("hello".into())
		},
	        BodyExpr::Print {
		    msg: BasicType::Atom("world".into())
		}]
	}],
    };
    assert_eq!(result, Ok(("", expected)));
}

