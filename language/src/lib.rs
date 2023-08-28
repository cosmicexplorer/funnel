/*
 * Description: ???
 *
 * Copyright (C) 2023 Danny McClanahan <dmcC2@hypnicjerk.ai>
 * SPDX-License-Identifier: GPL-3.0
 *
 * FIXME: is this sufficient license notice?
 * Licensed under the GNU GPL, Version 3.0 (see LICENSE).
 */

//! ???

/* These clippy lint descriptions are purely non-functional and do not affect the functionality
 * or correctness of the code. */
// #![warn(missing_docs)]

/* TODO: these can be removed, i guess... */
#![feature(let_chains)]
/* Note: run clippy with: rustup run nightly cargo-clippy! */
#![deny(unsafe_code)]
/* Ensure any doctest warnings fails the doctest! */
#![doc(test(attr(deny(warnings))))]
/* Enable all clippy lints except for many of the pedantic ones. It's a shame this needs to be
 * copied and pasted across crates, but there doesn't appear to be a way to include inner
 * attributes from a common source. */
#![deny(
  clippy::all,
  clippy::default_trait_access,
  clippy::expl_impl_clone_on_copy,
  clippy::if_not_else,
  clippy::needless_continue,
  clippy::single_match_else,
  clippy::unseparated_literal_suffix,
  clippy::used_underscore_binding
)]
/* It is often more clear to show that nothing is being moved. */
#![allow(clippy::match_ref_pats)]
/* Subjective style. */
#![allow(
  clippy::derived_hash_with_manual_eq,
  clippy::len_without_is_empty,
  clippy::redundant_field_names,
  clippy::too_many_arguments,
  clippy::single_component_path_imports,
  clippy::double_must_use
)]
/* Default isn't as big a deal as people seem to think it is. */
#![allow(clippy::new_without_default, clippy::new_ret_no_self)]

use chumsky::{input::ValueInput, prelude::*, text::whitespace};
use displaydoc::Display;
use thiserror::Error;


#[derive(Debug, Display, Error)]
pub enum LanguageError {
  /// idk
  Idk,
}


#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum Direction {
  Left,
  Right,
}

/// A constraint to act in the value or type space.
#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum Level {
  Value,
  r#Type,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Copy, Ord, PartialOrd)]
pub enum Token<'a> {
  GlobalDereference(&'a str),
  LocalDereference(&'a str),
  LocalLambdaArg(&'a str),
  NamespaceDereference(&'a str),
  GlobalAssignment(Direction, Level),
  Assertion(Level),
  Application(Direction, Level),
  CaseLiteralDereference(&'a str),
  CaseDeclaration(Option<&'a str>),
  CaseValueAssertion(Option<&'a str>),
  GroupStart(Level),
  GroupClose(Level),
  FreeCaseMarker,
  ParallelSeparator,
  SerialSeparator,
  FreeSerialReturn,
  NumericLiteral(&'a str),
  StringLiteral(&'a str),
  TypeSpecStart,
  TypeSpecClose,
  ImplicitArrow(Direction),
  AbbreviatedImplicitLink,
  NamespaceStart,
  NamespaceClose,
  ImportHighlight(&'a str),
  Whitespace,
}

fn global_symbol<'a>() -> impl Parser<'a, &'a str, &'a str> { regex("[a-zA-Z][a-zA-Z0-9_-]*") }

fn local_symbol<'a>() -> impl Parser<'a, &'a str, &'a str> { regex("[a-zA-Z_-][a-zA-Z0-9_-]*") }

fn case_symbol<'a>() -> impl Parser<'a, &'a str, &'a str> { regex("[a-z]+") }


fn tokenize<'a>() -> impl Parser<'a, &'a str, Token<'a>> {
  choice((
    just('$')
      .ignore_then(global_symbol())
      .map(Token::GlobalDereference),
    just('.')
      .ignore_then(local_symbol())
      .map(Token::LocalDereference),
    just("\\.")
      .ignore_then(local_symbol())
      .map(Token::LocalLambdaArg),
    just(':')
      .ignore_then(global_symbol())
      .map(Token::NamespaceDereference),
    choice((
      just("-<-").to(Token::GlobalAssignment(Direction::Left, Level::r#Type)),
      just("=<=").to(Token::GlobalAssignment(Direction::Left, Level::Value)),
      just("->-").to(Token::GlobalAssignment(Direction::Right, Level::r#Type)),
      just("=>=").to(Token::GlobalAssignment(Direction::Right, Level::Value)),
    )),
    choice((
      just("-!-").to(Token::Assertion(Level::r#Type)),
      just("=!=").to(Token::Assertion(Level::Value)),
    )),
    choice((
      just("<-").to(Token::Application(Direction::Left, Level::r#Type)),
      just("<=").to(Token::Application(Direction::Left, Level::Value)),
      just("->").to(Token::Application(Direction::Right, Level::r#Type)),
      just("=>").to(Token::Application(Direction::Right, Level::Value)),
    )),
    just('+')
      .ignore_then(case_symbol())
      .map(Token::CaseLiteralDereference),
    just("\\+")
      .ignore_then(case_symbol().or_not())
      .map(Token::CaseDeclaration),
    just("+!")
      .ignore_then(case_symbol().or_not())
      .map(Token::CaseValueAssertion),
    just('+').to(Token::FreeCaseMarker),
    choice((
      just('(').to(Token::GroupStart(Level::Value)),
      just(')').to(Token::GroupClose(Level::Value)),
      just('[').to(Token::GroupStart(Level::r#Type)),
      just(']').to(Token::GroupClose(Level::r#Type)),
    )),
    choice((just('/'), just(','))).to(Token::ParallelSeparator),
    just(';').to(Token::SerialSeparator),
    just(':').to(Token::FreeSerialReturn),
    regex("[0-9]+").map(Token::NumericLiteral),
    just('"')
      .ignore_then(regex("([^\"]|\\\")*"))
      .then_ignore(just('"'))
      .map(Token::StringLiteral),
    choice((
      just("[(").to(Token::TypeSpecStart),
      just(")]").to(Token::TypeSpecClose),
    )),
    choice((
      just("<~").to(Token::ImplicitArrow(Direction::Left)),
      just("~>").to(Token::ImplicitArrow(Direction::Right)),
    )),
    just('~').to(Token::AbbreviatedImplicitLink),
    choice((
      just("{").to(Token::NamespaceStart),
      just("}").to(Token::NamespaceClose),
    )),
    just("$$")
      .ignore_then(global_symbol())
      .map(Token::ImportHighlight),
    whitespace().at_least(1).to(Token::Whitespace),
  ))
}


#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct NamespaceComponent<'a> {
  pub component: &'a str,
}

fn namespace_components<'a, I>() -> impl Parser<'a, I, NamespaceComponent<'a>>+Clone
where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
  select! {
    Token::NamespaceDereference(name) => NamespaceComponent { component: name },
  }
}


/* TODO: use this in typechecking! */
fn symbol_level(s: &str) -> Level {
  assert!(!s.is_empty());
  let initial = s.chars().next().unwrap();
  match initial {
    '-' => Level::Value,
    '_' => Level::r#Type,
    _ => {
      if initial.is_ascii_uppercase() {
        Level::r#Type
      } else {
        assert!(initial.is_ascii_lowercase());
        Level::Value
      }
    },
  }
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub struct GlobalName<'a> {
  pub name: &'a str,
  pub prefix: Vec<NamespaceComponent<'a>>,
}

fn global_names<'a, I>() -> impl Parser<'a, I, GlobalName<'a>>+Clone
where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
  let global_name = select! {
    Token::GlobalDereference(name) => name,
  };
  namespace_components()
    .repeated()
    .collect::<Vec<_>>()
    .then(global_name)
    .map(|(namespace_components, global_name)| GlobalName {
      name: global_name,
      prefix: namespace_components,
    })
}


#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub enum EvalDirection {
  Dereference,
  DeclareLambdaArg,
}


#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct LocalName<'a> {
  pub name: &'a str,
  pub eval_direction: EvalDirection,
}

fn local_names<'a, I>() -> impl Parser<'a, I, LocalName<'a>>+Clone
where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
  select! {
    Token::LocalDereference(name) => LocalName { name, eval_direction: EvalDirection::Dereference },
    Token::LocalLambdaArg(name) => LocalName {
      name,
      eval_direction: EvalDirection::DeclareLambdaArg
    },
  }
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Name<'a> {
  Global(GlobalName<'a>),
  Local(LocalName<'a>),
  Case(CaseNameDereference<'a>),
}

fn names<'a, I>() -> impl Parser<'a, I, Name<'a>>+Clone
where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
  choice((
    global_names().map(Name::Global),
    local_names().map(Name::Local),
    case_name_dereferences().map(Name::Case),
  ))
}


macro_rules! impl_eq_given_fields {
  ($t:ident, $($field:ident),+) => {
    impl<'a> ::core::cmp::PartialEq for $t<'a> {
      fn eq(&self, other: &Self) -> bool {
        $(
          if self.$field != other.$field {
            return false;
          }
        )+
        true
      }
    }

    impl<'a> ::core::cmp::Eq for $t<'a> {}
  };
}


#[derive(Debug, Clone)]
pub struct ParallelJoin<'a> {
  pub exprs: Vec<Box<Expression<'a>>>,
}

impl_eq_given_fields![ParallelJoin, exprs];

fn parallel_joins<'a, I>() -> impl Parser<'a, I, ParallelJoin<'a>>+Clone
where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
  let joined = just(Token::Whitespace)
    .or_not()
    .ignore_then(just(Token::ParallelSeparator))
    .ignore_then(just(Token::Whitespace).or_not())
    .ignore_then(expressions())
    .repeated()
    .at_least(1)
    .collect::<Vec<_>>();
  expressions().then(joined).map(|(expr, joined)| {
    let mut ret: Vec<Expression<'a>> = vec![expr];
    ret.extend(joined.into_iter());
    ParallelJoin {
      exprs: ret.into_iter().map(Box::new).collect(),
    }
  })
}


#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct CaseNameDereference<'a> {
  pub name: &'a str,
}

fn case_name_dereferences<'a, I>() -> impl Parser<'a, I, CaseNameDereference<'a>>+Clone
where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
  select! {
    Token::CaseLiteralDereference(name) => CaseNameDereference { name }
  }
}


#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct CaseValueAssertion<'a> {
  pub name: Option<&'a str>,
}

fn case_value_assertions<'a, I>() -> impl Parser<'a, I, CaseValueAssertion<'a>>+Clone
where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
  select! {
    Token::CaseValueAssertion(name) => CaseValueAssertion { name }
  }
}


#[derive(Debug, Clone)]
pub struct Assertion<'a> {
  pub lhs: Box<Expression<'a>>,
  pub rhs: Box<Expression<'a>>,
  pub level: Level,
}

impl_eq_given_fields![Assertion, lhs, rhs, level];

fn assertions<'a, I>() -> impl Parser<'a, I, Assertion<'a>>+Clone
where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
  let assertions = select! {
    Token::Assertion(level) => level,
  };
  expressions()
    .clone()
    .then_ignore(just(Token::Whitespace).or_not())
    .then(assertions)
    .then_ignore(just(Token::Whitespace).or_not())
    .then(expressions())
    .map(|((lhs, level), rhs)| Assertion {
      lhs: Box::new(lhs),
      rhs: Box::new(rhs),
      level,
    })
}


#[derive(Debug, Clone)]
pub struct ArrowApplication<'a> {
  pub source: Box<Expression<'a>>,
  pub target: Box<Expression<'a>>,
  pub direction: Direction,
  pub level: Level,
}

impl_eq_given_fields![ArrowApplication, source, target, direction, level];

fn arrow_applications<'a, I>() -> impl Parser<'a, I, ArrowApplication<'a>>+Clone
where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
  let applications = select! {
    Token::Application(direction, level) => (direction, level),
  };
  expressions()
    .clone()
    .then_ignore(just(Token::Whitespace).or_not())
    .then(applications)
    .then_ignore(just(Token::Whitespace).or_not())
    .then(expressions())
    .map(|((lhs, (direction, level)), rhs)| match direction {
      Direction::Left => ArrowApplication {
        source: Box::new(rhs),
        target: Box::new(lhs),
        direction,
        level,
      },
      Direction::Right => ArrowApplication {
        source: Box::new(lhs),
        target: Box::new(rhs),
        direction,
        level,
      },
    })
}


#[derive(Debug, Clone)]
pub struct CaseDeclaration<'a> {
  pub name: Option<&'a str>,
  pub immediates: Vec<LeftwardImmediateSource<'a>>,
  pub application: Option<(Level, Box<Expression<'a>>)>,
}

impl_eq_given_fields![CaseDeclaration, name, immediates, application];

fn case_declarations<'a, I>() -> impl Parser<'a, I, CaseDeclaration<'a>>+Clone
where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
  let case_decls = select! {
    Token::CaseDeclaration(name) => name,
  };
  /* NB: only select rightward arrows. */
  let arrows = select! {
    Token::Application(Direction::Right, level) => level,
  };

  let application = arrows
    .then_ignore(just(Token::Whitespace).or_not())
    .then(expressions())
    .then_ignore(just(Token::Whitespace).or_not())
    .map(|(arrow_level, expr)| (arrow_level, Box::new(expr)))
    .or_not();

  case_decls
    .then(leftward_immediate_sources().repeated().collect::<Vec<_>>())
    .then(application)
    .map(|((name, immediates), application)| CaseDeclaration {
      name,
      immediates,
      application,
    })
    /* compilation fails with a type too long error if this is omitted lol */
    .boxed()
}


fn value_group_start<'a, I>() -> impl Parser<'a, I, ()>+Clone
where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
  select! {
    Token::GroupStart(Level::Value) => (),
  }
}
fn type_group_start<'a, I>() -> impl Parser<'a, I, ()>+Clone
where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
  select! {
    Token::GroupStart(Level::r#Type) => (),
  }
}
fn value_group_end<'a, I>() -> impl Parser<'a, I, ()>+Clone
where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
  select! {
    Token::GroupClose(Level::Value) => (),
  }
}
fn type_group_end<'a, I>() -> impl Parser<'a, I, ()>+Clone
where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
  select! {
    Token::GroupClose(Level::r#Type) => (),
  }
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub struct CaseDeconstruction<'a> {
  pub cases: Vec<CaseDeclaration<'a>>,
  pub level: Level,
}

fn case_deconstructions<'a, I>() -> impl Parser<'a, I, CaseDeconstruction<'a>>+Clone
where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
  fn parse_decls<'a, I>() -> impl Parser<'a, I, CaseDeclaration<'a>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    case_declarations().then_ignore(just(Token::Whitespace).or_not())
  }
  fn parse_inner<'a, I>() -> impl Parser<'a, I, Vec<CaseDeclaration<'a>>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    just(Token::Whitespace)
      .or_not()
      .ignore_then(parse_decls().repeated().at_least(1).collect::<Vec<_>>())
  }

  choice((
    just(Token::FreeCaseMarker)
      .ignore_then(value_group_start())
      .ignore_then(parse_inner())
      .then_ignore(value_group_end())
      .map(|cases| CaseDeconstruction {
        cases,
        level: Level::Value,
      }),
    just(Token::FreeCaseMarker)
      .ignore_then(type_group_start())
      .ignore_then(parse_inner())
      .then_ignore(type_group_end())
      .map(|cases| CaseDeconstruction {
        cases,
        level: Level::r#Type,
      }),
  ))
}


#[derive(Debug, Clone)]
pub struct SerialGroup<'a> {
  pub ordered_statements: Vec<Box<Expression<'a>>>,
  pub return_value: Box<Expression<'a>>,
  pub level: Level,
}

impl_eq_given_fields![SerialGroup, ordered_statements, return_value, level];

fn serial_groups<'a, I>() -> impl Parser<'a, I, SerialGroup<'a>>+Clone
where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
  /* (; <expr>)+ */
  fn parse_statements<'a, I>() -> impl Parser<'a, I, Vec<Expression<'a>>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    just(Token::SerialSeparator)
      .ignore_then(just(Token::Whitespace).or_not())
      .ignore_then(expressions())
      .then_ignore(just(Token::Whitespace).or_not())
      .repeated()
      .at_least(1)
      .collect::<Vec<_>>()
  }
  /* : <expr> */
  fn parse_return<'a, I>() -> impl Parser<'a, I, Expression<'a>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    just(Token::FreeSerialReturn)
      .ignore_then(just(Token::Whitespace).or_not())
      .ignore_then(expressions())
  }

  /* ( (; <expr>)+ : <expr> ) */
  fn parse_inner<'a, I>() -> impl Parser<'a, I, (Vec<Expression<'a>>, Expression<'a>)>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    just(Token::Whitespace)
      .or_not()
      .ignore_then(parse_statements())
      .then(parse_return())
      .then_ignore(just(Token::Whitespace).or_not())
  }

  choice((
    just(Token::SerialSeparator)
      .ignore_then(value_group_start())
      .ignore_then(parse_inner())
      .then_ignore(value_group_end())
      .map(|(statements, ret)| SerialGroup {
        ordered_statements: statements.into_iter().map(Box::new).collect(),
        return_value: Box::new(ret),
        level: Level::Value,
      }),
    just(Token::SerialSeparator)
      .ignore_then(type_group_start())
      .ignore_then(parse_inner())
      .then_ignore(type_group_end())
      .map(|(statements, ret)| SerialGroup {
        ordered_statements: statements.into_iter().map(Box::new).collect(),
        return_value: Box::new(ret),
        level: Level::r#Type,
      }),
  ))
}


#[derive(Debug, Clone)]
pub struct BasicGroup<'a> {
  pub inner: Box<Expression<'a>>,
  pub level: Level,
}

impl_eq_given_fields![BasicGroup, inner, level];

fn basic_groups<'a, I>() -> impl Parser<'a, I, BasicGroup<'a>>+Clone
where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
  fn parse_inner<'a, I>() -> impl Parser<'a, I, Expression<'a>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    just(Token::Whitespace)
      .or_not()
      .ignore_then(expressions())
      .then_ignore(just(Token::Whitespace).or_not())
  }

  choice((
    value_group_start()
      .ignore_then(parse_inner())
      .then_ignore(value_group_end())
      .map(|expr| BasicGroup {
        inner: Box::new(expr),
        level: Level::Value,
      }),
    type_group_start()
      .ignore_then(parse_inner())
      .then_ignore(type_group_end())
      .map(|expr| BasicGroup {
        inner: Box::new(expr),
        level: Level::r#Type,
      }),
  ))
}


#[derive(Debug, Clone)]
pub struct TypeSpec<'a> {
  pub inner: Box<Expression<'a>>,
}

impl_eq_given_fields![TypeSpec, inner];

fn type_specs<'a, I>() -> impl Parser<'a, I, TypeSpec<'a>>+Clone
where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
  just(Token::TypeSpecStart)
    .ignore_then(just(Token::Whitespace).or_not())
    .ignore_then(expressions())
    .then_ignore(just(Token::Whitespace).or_not())
    .then_ignore(just(Token::TypeSpecClose))
    .map(|expr| TypeSpec {
      inner: Box::new(expr),
    })
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Group<'a> {
  Basic(BasicGroup<'a>),
  Serial(SerialGroup<'a>),
  TypeSpec(TypeSpec<'a>),
  Case(CaseDeconstruction<'a>),
}

fn groups<'a, I>() -> impl Parser<'a, I, Group<'a>>+Clone
where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
  choice((
    serial_groups().map(Group::Serial),
    case_deconstructions().map(Group::Case),
    basic_groups().map(Group::Basic),
    type_specs().map(Group::TypeSpec),
  ))
}


#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct LocalNameDereference<'a> {
  pub name: &'a str,
}


fn local_name_dereferences<'a, I>() -> impl Parser<'a, I, LocalNameDereference<'a>>+Clone
where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
  select! {
    Token::LocalDereference(name) => LocalNameDereference { name }
  }
}


/// things that apply to their left
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum LeftwardImmediateSource<'a> {
  /// e.g. "$f.x" retrieves the ".x" field from the value "$f"
  LocalNameDereference(LocalNameDereference<'a>),
  /// e.g. ".x+!true" extracts the "+true" case from ".x"
  CaseValueAssertion(CaseValueAssertion<'a>),
  /// e.g. ".x$f" applies "$f" to ".x"
  /* how to make ".x$f(3)" parse to "$f(.x, 3)" vs "$f(.x)(3)"? this should not matter if we
   * auto-curry/etc */
  GlobalName(GlobalName<'a>),
  /// e.g. ".x(...)" applies "(...)" to ".x"
  Group(Group<'a>),
}

fn leftward_immediate_sources<'a, I>() -> impl Parser<'a, I, LeftwardImmediateSource<'a>>+Clone
where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
  choice((
    local_name_dereferences().map(LeftwardImmediateSource::LocalNameDereference),
    case_value_assertions().map(LeftwardImmediateSource::CaseValueAssertion),
    global_names().map(LeftwardImmediateSource::GlobalName),
    groups().map(LeftwardImmediateSource::Group),
  ))
}


#[derive(Debug, Clone)]
pub struct ImmediateApplication<'a> {
  pub target: Box<Expression<'a>>,
  pub source: LeftwardImmediateSource<'a>,
}

impl_eq_given_fields![ImmediateApplication, target, source];

fn immediate_applications<'a, I>() -> impl Parser<'a, I, ImmediateApplication<'a>>+Clone
where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
  expressions()
    .clone()
    .then(leftward_immediate_sources())
    .map(|(target, source)| ImmediateApplication {
      target: Box::new(target),
      source,
    })
}


#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct NumericLiteral<'a> {
  pub value: &'a str,
}

fn numbers<'a, I>() -> impl Parser<'a, I, NumericLiteral<'a>>+Clone
where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
  select! {
    Token::NumericLiteral(value) => NumericLiteral { value },
  }
}


#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct StringLiteral<'a> {
  pub value: &'a str,
}

fn strings<'a, I>() -> impl Parser<'a, I, StringLiteral<'a>>+Clone
where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
  select! {
    Token::StringLiteral(value) => StringLiteral { value },
  }
}


#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum Literal<'a> {
  Num(NumericLiteral<'a>),
  Str(StringLiteral<'a>),
}

fn literals<'a, I>() -> impl Parser<'a, I, Literal<'a>>+Clone
where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
  choice((numbers().map(Literal::Num), strings().map(Literal::Str)))
}


#[derive(Debug, Clone)]
pub enum Expression<'a> {
  Name(Name<'a>),
  Lit(Literal<'a>),
  Assertion(Assertion<'a>),
  Arrow(ArrowApplication<'a>),
  Immediate(ImmediateApplication<'a>),
  Join(ParallelJoin<'a>),
  Group(Group<'a>),
}

fn expressions<'a, I>() -> impl Parser<'a, I, Expression<'a>>+Clone
where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
  recursive(|expressions| {
    choice((
      literals().map(Expression::Lit),
      names().map(Expression::Name),
      immediate_applications().map(Expression::Immediate),
      parallel_joins().map(Expression::Join),
      groups().map(Expression::Group),
      arrow_applications().map(Expression::Arrow),
    ))
  })
}


/* pub enum GlobalPlaceExpression<'a> {} */


#[derive(Debug, Clone)]
pub enum TopLevelStatement<'a> {
  GlobalAssignment(Expression<'a>, Expression<'a>, Direction, Level),
  SimpleExpression(Expression<'a>),
  NamespaceExpansion,
  ImportHighlight,
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub enum C {
  A,
  B,
}


pub fn parser<'a>() -> impl Parser<'a, &'a str, C> {
  choice((just('a').to(C::A), just('b').to(C::B)))
}


#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn it_works() {
    let (result, errs) = parser().parse("a").into_output_errors();
    assert!(errs.is_empty());
    assert_eq!(result.unwrap(), C::A);

    let (result, errs) = parser().parse("b").into_output_errors();
    assert!(errs.is_empty());
    assert_eq!(result.unwrap(), C::B);

    let (result, errs) = parser().parse("c").into_output_errors();
    assert!(result.is_none());
    assert!(!errs.is_empty());
  }

  #[test]
  fn tokenize_example() {
    let result = tokenize()
      .repeated()
      .collect_exactly::<[_; 2]>()
      .parse(".x.y")
      .into_result()
      .unwrap();
    assert_eq!(result, [
      Token::LocalDereference("x"),
      Token::LocalDereference("y")
    ]);
    let result = tokenize()
      .repeated()
      .collect::<Vec<_>>()
      .parse(".x.y(3)")
      .into_result()
      .unwrap();
    assert_eq!(result, vec![
      Token::LocalDereference("x"),
      Token::LocalDereference("y"),
      Token::GroupStart(Level::Value),
      Token::NumericLiteral("3"),
      Token::GroupClose(Level::Value),
    ]);
  }

  #[test]
  fn parse_local_names() {
    let token_stream = tokenize().repeated().stream(".x").into_result().unwrap();
    let result = local_names().parse(token_stream).into_result().unwrap();
    assert_eq!(result, LocalName {
      name: "x",
      eval_direction: EvalDirection::Dereference
    });
  }

  #[test]
  fn parse_local_into_name() {
    let token_stream = tokenize().repeated().stream(".x").into_result().unwrap();
    let result = names().parse(token_stream).into_result().unwrap();
    assert!(matches![
      result,
      Name::Local(LocalName {
        name: "x",
        eval_direction: EvalDirection::Dereference
      }),
    ]);
  }

  #[test]
  fn parse_expr_local_name() {
    let token_stream = tokenize().repeated().stream(".x").into_result().unwrap();
    let result = expressions().parse(token_stream).into_result().unwrap();
    assert_eq!(
      result,
      Expression::Name(Name::Local(LocalName {
        name: "x",
        eval_direction: EvalDirection::Dereference
      }))
    );
    /* assert!(matches![ */
    /* result, */
    /* Expression::Name(Name::Local(LocalName { */
    /* name: "x", */
    /* eval_direction: EvalDirection::Dereference */
    /* })) */
    /* ]); */
  }
}
