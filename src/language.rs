use nom::{
  branch::alt,
  bytes::complete::{is_not, tag},
  bytes::complete::{take_while, take_while1},
  character::complete::{char, digit1, one_of, satisfy},
  combinator::{eof, not, peek, recognize, value},
  multi::{fold_many0, many0, separated_list0},
  sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
  Err, IResult, Parser, error::{ParseError, ErrorKind}
};

type Answer<'a, T> = IResult<Span<'a>, T>;

use nom_locate::LocatedSpan;
type Span<'a> = LocatedSpan<&'a str>;
use itertools::Itertools;

use std::fmt;
use std::str::FromStr;

// Types
// =====

struct Error<'a> {
  span: Span<'a>,
  kind: Kind,
}

enum Kind {
  Token(&'static str),
  Term,
  Other,
}

impl<'a> ParseError<Span<'a>> for Error<'a> {
  fn from_error_kind(input: Span<'a>, kind: ErrorKind) -> Self {
    Self {
      span: input,
      kind: Kind::Other,
    }
  }

  fn append(_: Span<'a>, _: ErrorKind, other: Self) -> Self {
    other
  }
}

// Term
// ----

#[derive(Clone)]
pub enum RTerm<'a> {
  Var { name: &'a str },
  Dup { nam0: &'a str, nam1: &'a str, expr: RBTerm<'a>, body: RBTerm<'a> },
  Let { name: &'a str, expr: RBTerm<'a>, body: RBTerm<'a> },
  Lam { name: &'a str, body: RBTerm<'a> },
  App { func: RBTerm<'a>, argm: RBTerm<'a> },
  Ctr { name: &'a str, args: Vec<RTerm<'a>> },
  U32 { numb: u32 },
  Op2 { oper: Oper, val0: RBTerm<'a>, val1: RBTerm<'a> },
}

pub type RBTerm<'a> = Box<RTerm<'a>>;

impl RTerm<'_> {
  pub fn to_bterm(&self) -> BTerm {
    Box::new(match self {
      RTerm::Var { name } => Term::Var { name: name.to_string() },
      RTerm::Dup { nam0, nam1, expr, body } => Term::Dup {
        nam0: nam0.to_string(),
        nam1: nam1.to_string(),
        expr: expr.to_bterm(),
        body: body.to_bterm(),
      },
      RTerm::Let { name, expr, body } => {
        Term::Let { name: name.to_string(), expr: expr.to_bterm(), body: body.to_bterm() }
      }
      RTerm::Lam { name, body } => Term::Lam { name: name.to_string(), body: body.to_bterm() },
      RTerm::App { func, argm } => Term::App { func: func.to_bterm(), argm: argm.to_bterm() },
      RTerm::Ctr { name, args } => {
        Term::Ctr { name: name.to_string(), args: args.iter().map(|a| a.to_bterm()).collect() }
      }
      RTerm::U32 { numb } => Term::U32 { numb: *numb },
      RTerm::Op2 { oper, val0, val1 } => {
        Term::Op2 { oper: *oper, val0: val0.to_bterm(), val1: val1.to_bterm() }
      }
    })
  }
}

#[derive(Clone, Debug)]
pub enum Term {
  Var { name: String },
  Dup { nam0: String, nam1: String, expr: BTerm, body: BTerm },
  Let { name: String, expr: BTerm, body: BTerm },
  Lam { name: String, body: BTerm },
  App { func: BTerm, argm: BTerm },
  Ctr { name: String, args: Vec<BTerm> },
  U32 { numb: u32 },
  Op2 { oper: Oper, val0: BTerm, val1: BTerm },
}

pub type BTerm = Box<Term>;

#[derive(Clone, Copy, Debug)]
pub enum Oper {
  Add,
  Sub,
  Mul,
  Div,
  Mod,
  And,
  Or,
  Xor,
  Shl,
  Shr,
  Ltn,
  Lte,
  Eql,
  Gte,
  Gtn,
  Neq,
}

impl FromStr for Oper {
  type Err = ();
  fn from_str(s: &str) -> Result<Self, Self::Err> {
    use Oper::*;
    let op = match s {
      "+" => Add,
      "-" => Sub,
      "*" => Mul,
      "/" => Div,
      "%" => Mod,
      "&" => And,
      "|" => Or,
      "^" => Xor,
      "<<" => Shl,
      ">>" => Shr,
      "<" => Ltn,
      "<=" => Lte,
      "==" => Eql,
      ">=" => Gte,
      ">" => Gtn,
      "!=" => Neq,
      _ => return Err(()),
    };
    Ok(op)
  }
}

// Rule
// ----

#[derive(Clone, Debug)]
pub struct Rule {
  pub lhs: BTerm,
  pub rhs: BTerm,
}

pub struct RRule<'a> {
  pub lhs: RTerm<'a>,
  pub rhs: RTerm<'a>,
}

#[cfg(test)]
impl RRule<'_> {
  pub fn to_rule(&self) -> Rule {
    Rule { lhs: self.lhs.to_bterm(), rhs: self.rhs.to_bterm() }
  }
}

// File
// ----

pub struct File {
  pub rules: Vec<Rule>,
}

pub struct RFile<'a> {
  pub rules: Vec<RRule<'a>>,
}

impl RFile<'_> {
  pub fn to_file(&self) -> File {
    File {
      rules: self
        .rules
        .iter()
        .map(|RRule { lhs, rhs }| Rule { lhs: lhs.to_bterm(), rhs: rhs.to_bterm() })
        .collect(),
    }
  }
}

// Stringifier
// ===========

// Term
// ----

impl fmt::Display for Oper {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "{}",
      match self {
        Self::Add => "+",
        Self::Sub => "-",
        Self::Mul => "*",
        Self::Div => "/",
        Self::Mod => "%",
        Self::And => "&",
        Self::Or => "|",
        Self::Xor => "^",
        Self::Shl => "<<",
        Self::Shr => ">>",
        Self::Ltn => "<",
        Self::Lte => "<=",
        Self::Eql => "==",
        Self::Gte => ">=",
        Self::Gtn => ">",
        Self::Neq => "!=",
      }
    )
  }
}

impl fmt::Display for Term {
  // WARN: I think this could overflow, might need to rewrite it to be iterative instead of recursive?
  // NOTE: Another issue is complexity. This function is O(N^2). Should use ropes to be linear.
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    fn str_sugar(term: &Term) -> Option<String> {
      fn go(term: &Term, text: &mut String) -> Option<()> {
        if let Term::Ctr { name, args } = term {
          if name == "StrCons" && args.len() == 2 {
            if let Term::U32 { numb } = *args[0] {
              text.push(std::char::from_u32(numb)?);
              go(&args[1], text)?;
            }
            return Some(());
          }
          if name == "StrNil" && args.is_empty() {
            return Some(());
          }
        }
        None
      }
      let mut result = String::new();
      result.push('"');
      go(term, &mut result)?;
      result.push('"');
      Some(result)
    }
    match self {
      Self::Var { name } => write!(f, "{}", name),
      Self::Dup { nam0, nam1, expr, body } => {
        write!(f, "dup {} {} = {}; {}", nam0, nam1, expr, body)
      }
      Self::Let { name, expr, body } => write!(f, "let {} = {}; {}", name, expr, body),
      Self::Lam { name, body } => write!(f, "λ{} {}", name, body),
      Self::App { func, argm } => write!(f, "({} {})", func, argm),
      Self::Ctr { name, args } => {
        if let Some(term) = str_sugar(self) {
          write!(f, "{}", term)
        } else {
          write!(f, "({}{})", name, args.iter().map(|x| format!(" {}", x)).collect::<String>())
        }
      }
      Self::U32 { numb } => write!(f, "{}", numb),
      Self::Op2 { oper, val0, val1 } => write!(f, "({} {} {})", oper, val0, val1),
    }
  }
}

// Rule
// ----

impl fmt::Display for Rule {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{} = {}", self.lhs, self.rhs)
  }
}

// File
// ----

impl fmt::Display for File {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "{}",
      self.rules.iter().map(|rule| format!("{}", rule)).collect::<Vec<String>>().join("\n")
    )
  }
}

// Parser
// ======

fn parse_let(input: Span) -> Answer<RTerm> {
  after_tag(
    "let",
    expect(tuple((var_name, after_tag("=", parse_term), after_tag(";", parse_term)))),
  )
  .map(|(name, expr, body)| RTerm::Let { name, expr: Box::new(expr), body: Box::new(body) })
  .parse(input)
}

fn parse_dup(input: Span) -> Answer<RTerm> {
  after_tag(
    "dup",
    expect(tuple((
      pair(var_name, var_name),
      after_tag("=", parse_term),
      after_tag(";", parse_term),
    ))),
  )
  .map(|((nam0, nam1), expr, body)| RTerm::Dup {
    nam0,
    nam1,
    expr: Box::new(expr),
    body: Box::new(body),
  })
  .parse(input)
}

fn parse_lam(input: Span) -> Answer<RTerm> {
  preceded(one_of("λ@"), expect(pair(var_name, parse_term)))
    .map(|(name, body)| RTerm::Lam { name, body: Box::new(body) })
    .parse(input)
}

fn fold_args<'a>(init: RTerm<'a>) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, RTerm<'a>> {
  fold_many0(
    parse_term,
    move || init.clone(),
    |a, b| RTerm::App { func: Box::new(a), argm: Box::new(b) },
  )
}

fn parse_app(input: Span) -> Answer<RTerm> {
  parse_term.flat_map(fold_args).parse(input)
}

fn parse_ctr(input: Span) -> Answer<RTerm> {
  pair(ctr_name, many0(parse_term)).map(|(name, args)| RTerm::Ctr { name, args }).parse(input)
}

fn parse_u32(input: Span) -> Answer<RTerm> {
  terminated(digit1, not(peek(satisfy(|c| matches!(c, 'A'..='Z' | 'a'..='z' | 'λ' | '@')))))
    .map(|digits: Span| RTerm::U32 {
      numb: digits.parse::<u32>().expect("integer literal too large for a u32"),
    })
    .parse(input)
}

fn parse_op2(input: Span) -> Answer<RTerm> {
  let parse_oper = ws(take_while1(|c| {
    matches!(c, '+' | '-' | '*' | '/' | '%' | '&' | '|' | '^' | '<' | '>' | '=' | '!')
  }))
  .map(|s| Oper::from_str(*s).unwrap());

  let parse_args = |oper| {
    expect(pair(parse_term, parse_term)).map(move |(val0, val1)| RTerm::Op2 {
      oper,
      val0: Box::new(val0),
      val1: Box::new(val1),
    })
  };

  parse_oper.flat_map(parse_args).parse(input)
}

fn parse_var(input: Span) -> IResult<Span, RTerm> {
  var_name.map(|name| RTerm::Var { name }).parse(input)
}

// TODO: parse escape sequences
pub fn parse_str_sugar(input: Span) -> Answer<RTerm> {
  let parse_chars = delimited(char('"'), is_not("\""), char('"')).map(|s: Span| *s);
  parse_chars
    .map(|chars: &str| {
      chars.chars().rfold(RTerm::Ctr { name: "StrNil", args: Vec::new() }, move |list, c| {
        RTerm::Ctr { name: "StrCons", args: vec![RTerm::U32 { numb: c as u32 }, list] }
      })
    })
    .parse(input)
}

pub fn parse_lst_sugar(input: Span) -> Answer<RTerm> {
  let parse_elems = delimited(char('['), separated_list0(ws(char(',')), parse_term), ws(char(']')));
  parse_elems
    .map(|elems| {
      elems.into_iter().rfold(RTerm::Ctr { name: "Nil", args: Vec::new() }, move |list, elem| {
        RTerm::Ctr { name: "Cons", args: vec![elem, list] }
      })
    })
    .parse(input)
}

fn parse_term(input: Span) -> Answer<RTerm> {
  ws(alt((
    parenthesized(alt((parse_ctr, parse_op2, parse_app))),
    parse_lam,
    parse_let,
    parse_dup,
    parse_u32,
    parse_var,
    parse_str_sugar,
    parse_lst_sugar,
  )))(input)
}

fn parse_rule(input: Span) -> Answer<RRule> {
  separated_pair(parse_term, ws(char('=')), parse_term)
    .map(|(lhs, rhs)| RRule { lhs, rhs })
    .parse(input)
}

fn after_tag<'a, O>(
  t: &'a str,
  parser: impl Parser<Span<'a>, O, nom::error::Error<Span<'a>>>,
) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, O> {
  preceded(ws(tag(t)), parser)
}

fn name<'a>(
  first_char: impl Fn(char) -> bool,
) -> impl Parser<Span<'a>, &'a str, nom::error::Error<Span<'a>>> {
  ws(recognize(pair(
    satisfy(first_char),
    take_while(|c| matches!(c, 'A'..='Z' | 'a'..='z' | '0'..='9' | '_' | '.')),
  )))
  .map(|s| *s)
}

fn var_name(input: Span) -> IResult<Span, &str> {
  name(|c| matches!(c, 'a'..='z')).parse(input)
}

fn ctr_name(input: Span) -> IResult<Span, &str> {
  name(|c| matches!(c, 'A'..='Z')).parse(input)
}

fn parenthesized<'a, O>(
  parser: impl Parser<Span<'a>, O, nom::error::Error<Span<'a>>>,
) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, O> {
  delimited(char('('), parser, ws(char(')')))
}

/// skips preceding whitespace and comments
fn ws<'a, O>(
  parser: impl Parser<Span<'a>, O, nom::error::Error<Span<'a>>>,
) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, O> {
  preceded(
    // make these return `()` so that `Vec<()>` does not allocate
    many0(alt((
      value((), take_while1(char::is_whitespace)),
      value((), pair(tag("//"), is_not("\n\r"))),
    ))),
    parser,
  )
}

fn expect<'a, O>(
  mut parser: impl Parser<Span<'a>, O, nom::error::Error<Span<'a>>>,
) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, O> {
  move |input| {
    parser.parse(input).map_err(|err| match err {
      Err::Incomplete(_) => unreachable!(),
      Err::Error(e) | Err::Failure(e) => Err::Failure(e),
    })
  }
}

pub fn read_term(code: &str) -> Box<Term> {
  parse_term(Span::new(code)).unwrap().1.to_bterm()
}

pub fn read_file(code: &str) -> Box<File> {
  Box::new(
    terminated(many0(parse_rule), ws(eof))
      .map(|rules| RFile { rules })
      .parse(Span::new(code))
      .unwrap()
      .1
      .to_file(),
  )
}

#[cfg(test)]
pub fn read_rule(code: &str) -> Box<Rule> {
  Box::new(parse_rule(Span::new(code)).unwrap().1.to_rule())
}

fn find(text: &str, target: &str) -> usize {
  text.find(target).unwrap_or_else(|| panic!("`{}` not in `{}`.", target, text))
}

// WARN: This fails if `from_index` or `to_index` are not `char` boundaries.
// Should probably not use slice indexing directly, maybe use `get` method and
// handle possible error instead?
pub fn highlight(from_index: usize, to_index: usize, code: &str) -> String {
  debug_assert!(to_index >= from_index);
  debug_assert!(code.get(from_index..to_index).is_some());
  //let open = "<<<<####";
  //let close = "####>>>>";
  let open = "««««";
  let close = "»»»»";
  let open_color = "\x1b[4m\x1b[31m";
  let close_color = "\x1b[0m";
  let mut from_line = 0;
  let mut to_line = 0;
  for (i, c) in
    code.chars().enumerate().filter(|(_, c)| c == &'\n').take_while(|(i, _)| i < &to_index)
  {
    if i < from_index {
      from_line += c.len_utf8();
    }
    to_line += c.len_utf8();
  }
  let code =
    [&code[0..from_index], open, &code[from_index..to_index], close, &code[to_index..code.len()]]
      .concat();
  let block_from_line = std::cmp::max(from_line as i64 - 3, 0) as usize;
  let block_to_line = std::cmp::min(to_line + 3, code.lines().count());
  code
    .lines()
    .enumerate()
    .skip_while(|(i, _)| i < &block_from_line)
    .take_while(|(i, _)| i < &block_to_line)
    .map(|(_, line)| line)
    .enumerate()
    .format_with("", |(i, line), f| {
      let numb = block_from_line + i;
      // TODO: An allocation of an intermediate string still occurs here
      // which is inefficient. Should figure out how to improve this.
      let rest = if numb == from_line && numb == to_line {
        [
          &line[0..find(line, open)],
          open_color,
          &line[find(line, open) + open.len()..find(line, close)],
          close_color,
          &line[find(line, close) + close.len()..line.len()],
          "\n",
        ]
        .concat()
      } else if numb == from_line {
        [&line[0..find(line, open)], open_color, &line[find(line, open)..line.len()], "\n"].concat()
      } else if numb > from_line && numb < to_line {
        [open_color, line, close_color, "\n"].concat()
      } else if numb == to_line {
        [
          &line[0..find(line, open)],
          open_color,
          &line[find(line, open)..find(line, close) + close.len()],
          close_color,
          "\n",
        ]
        .concat()
      } else {
        [line, "\n"].concat()
      };
      f(&format_args!("    {} | {}", numb, rest))
    })
    .to_string()
}
