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


pub trait GivenInputParseable<'a>: Sized {
  type I: Input<'a>;
  fn parser() -> Boxed<'a, 'a, Self::I, Self, extra::Default>;
}

pub trait TokenParseable<'a>: Sized {
  type Tok;
  fn parser<I>() -> Boxed<'a, 'a, I, Self, extra::Default>
  where I: ValueInput<'a, Token=Self::Tok, Span=SimpleSpan>;
}

pub trait RecursivelyParseable<'a>: Sized {
  type Tok;
  type InnerExpr;
  fn parser<I>(
    inner: Boxed<'a, 'a, I, Self::InnerExpr, extra::Default>,
  ) -> Boxed<'a, 'a, I, Self, extra::Default>
  where I: ValueInput<'a, Token=Self::Tok, Span=SimpleSpan>;
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
  NamedCaseDeclaration(&'a str),
  UnnamedCaseDeclaration,
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
  ImportLift,
  ImportEdit,
  Whitespace,
}

impl<'a> Token<'a> {
  fn global_symbol() -> impl Parser<'a, &'a str, &'a str> { regex("[a-zA-Z][a-zA-Z0-9_-]*") }

  fn local_symbol() -> impl Parser<'a, &'a str, &'a str> { regex("[a-zA-Z_-][a-zA-Z0-9_-]*") }

  fn namespace_symbol() -> impl Parser<'a, &'a str, &'a str> { regex("[a-z][a-z-]*") }

  fn case_symbol() -> impl Parser<'a, &'a str, &'a str> { regex("[a-z]+") }

  fn tokenize() -> impl Parser<'a, &'a str, Token<'a>> {
    choice((
      just('$')
        .ignore_then(Self::global_symbol())
        .map(Token::GlobalDereference),
      just('.')
        .ignore_then(Self::local_symbol())
        .map(Token::LocalDereference),
      just("\\.")
        .ignore_then(Self::local_symbol())
        .map(Token::LocalLambdaArg),
      just(':')
        .ignore_then(Self::namespace_symbol())
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
        .ignore_then(Self::case_symbol())
        .map(Token::CaseLiteralDereference),
      just("\\+")
        .ignore_then(Self::case_symbol())
        .map(Token::NamedCaseDeclaration),
      just("\\+").to(Token::UnnamedCaseDeclaration),
      just("+!")
        .ignore_then(Self::case_symbol().or_not())
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
      just('<').to(Token::ImportLift),
      just('>').to(Token::ImportEdit),
      whitespace().at_least(1).to(Token::Whitespace),
    ))
  }
}

impl<'a> GivenInputParseable<'a> for Token<'a> {
  type I = &'a str;

  fn parser() -> Boxed<'a, 'a, Self::I, Self, extra::Default> { Self::tokenize().boxed() }
}


#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct NamespaceComponent<'a> {
  pub component: &'a str,
}

impl<'a> NamespaceComponent<'a> {
  fn namespace_components<I>() -> impl Parser<'a, I, NamespaceComponent<'a>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    select! {
      Token::NamespaceDereference(name) => NamespaceComponent { component: name },
    }
  }
}

impl<'a> TokenParseable<'a> for NamespaceComponent<'a> {
  type Tok = Token<'a>;

  fn parser<I>() -> Boxed<'a, 'a, I, Self, extra::Default>
  where I: ValueInput<'a, Token=Self::Tok, Span=SimpleSpan> {
    Self::namespace_components::<I>().boxed()
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

impl<'a> GlobalName<'a> {
  fn global_names<I>() -> impl Parser<'a, I, GlobalName<'a>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    let global_name = select! {
      Token::GlobalDereference(name) => name,
    };
    NamespaceComponent::parser()
      .repeated()
      .collect::<Vec<_>>()
      .then(global_name)
      .map(|(namespace_components, global_name)| GlobalName {
        name: global_name,
        prefix: namespace_components,
      })
  }
}

impl<'a> TokenParseable<'a> for GlobalName<'a> {
  type Tok = Token<'a>;

  fn parser<I>() -> Boxed<'a, 'a, I, Self, extra::Default>
  where I: ValueInput<'a, Token=Self::Tok, Span=SimpleSpan> {
    Self::global_names::<I>().boxed()
  }
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

impl<'a> LocalName<'a> {
  fn local_names<I>() -> impl Parser<'a, I, LocalName<'a>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    select! {
      Token::LocalDereference(name) => LocalName { name, eval_direction: EvalDirection::Dereference },
      Token::LocalLambdaArg(name) => LocalName {
        name,
        eval_direction: EvalDirection::DeclareLambdaArg
      },
    }
  }
}

impl<'a> TokenParseable<'a> for LocalName<'a> {
  type Tok = Token<'a>;

  fn parser<I>() -> Boxed<'a, 'a, I, Self, extra::Default>
  where I: ValueInput<'a, Token=Self::Tok, Span=SimpleSpan> {
    Self::local_names::<I>().boxed()
  }
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Name<'a> {
  Global(GlobalName<'a>),
  Local(LocalName<'a>),
  Case(CaseNameDereference<'a>),
}

impl<'a> Name<'a> {
  fn names<I>() -> impl Parser<'a, I, Name<'a>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    choice((
      GlobalName::parser().map(Name::Global),
      LocalName::parser().map(Name::Local),
      CaseNameDereference::parser().map(Name::Case),
    ))
  }
}

impl<'a> TokenParseable<'a> for Name<'a> {
  type Tok = Token<'a>;

  fn parser<I>() -> Boxed<'a, 'a, I, Self, extra::Default>
  where I: ValueInput<'a, Token=Self::Tok, Span=SimpleSpan> {
    Self::names::<I>().boxed()
  }
}

pub trait HasArgSep<'a> {
  fn arg_sep<I>() -> Boxed<'a, 'a, I, Token<'a>, extra::Default>
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    just(Token::Whitespace)
      .or_not()
      .ignore_then(just(Token::ParallelSeparator))
      .then_ignore(just(Token::Whitespace).or_not())
      .boxed()
  }
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ParallelJoin<'a> {
  pub exprs: Vec<Box<Expression<'a>>>,
}

impl<'a> HasArgSep<'a> for ParallelJoin<'a> {}

impl<'a> ParallelJoin<'a> {
  fn parallel_joins<I>(
    expressions: impl Parser<'a, I, Expression<'a>>+Clone+'a,
  ) -> impl Parser<'a, I, ParallelJoin<'a>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    expressions
      .separated_by(Self::arg_sep())
      .allow_trailing()
      .collect::<Vec<_>>()
      .map(|exprs| ParallelJoin {
        exprs: exprs.into_iter().map(Box::new).collect(),
      })
  }
}

impl<'a> RecursivelyParseable<'a> for ParallelJoin<'a> {
  type InnerExpr = Expression<'a>;
  type Tok = Token<'a>;

  fn parser<I>(
    inner: Boxed<'a, 'a, I, Self::InnerExpr, extra::Default>,
  ) -> Boxed<'a, 'a, I, Self, extra::Default>
  where I: ValueInput<'a, Token=Self::Tok, Span=SimpleSpan> {
    Self::parallel_joins::<I>(inner).boxed()
  }
}


#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct CaseNameDereference<'a> {
  pub name: &'a str,
}

impl<'a> CaseNameDereference<'a> {
  fn case_name_dereferences<I>() -> impl Parser<'a, I, CaseNameDereference<'a>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    select! {
      Token::CaseLiteralDereference(name) => CaseNameDereference { name }
    }
  }
}

impl<'a> TokenParseable<'a> for CaseNameDereference<'a> {
  type Tok = Token<'a>;

  fn parser<I>() -> Boxed<'a, 'a, I, Self, extra::Default>
  where I: ValueInput<'a, Token=Self::Tok, Span=SimpleSpan> {
    Self::case_name_dereferences::<I>().boxed()
  }
}


#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct CaseValueAssertion<'a> {
  pub name: Option<&'a str>,
}

impl<'a> CaseValueAssertion<'a> {
  fn case_value_assertions<I>() -> impl Parser<'a, I, CaseValueAssertion<'a>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    select! {
      Token::CaseValueAssertion(name) => CaseValueAssertion { name }
    }
  }
}

impl<'a> TokenParseable<'a> for CaseValueAssertion<'a> {
  type Tok = Token<'a>;

  fn parser<I>() -> Boxed<'a, 'a, I, Self, extra::Default>
  where I: ValueInput<'a, Token=Self::Tok, Span=SimpleSpan> {
    Self::case_value_assertions::<I>().boxed()
  }
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Assertion<'a> {
  pub lhs: Box<Expression<'a>>,
  pub rhs: Box<Expression<'a>>,
  pub level: Level,
}

impl<'a> Assertion<'a> {
  fn assertions<I>(
    expressions: impl Parser<'a, I, Expression<'a>>+Clone+'a,
  ) -> impl Parser<'a, I, Assertion<'a>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    let assertions = select! {
      Token::Assertion(level) => level,
    };
    expressions
      .clone()
      .then_ignore(just(Token::Whitespace).or_not())
      .then(assertions)
      .then_ignore(just(Token::Whitespace).or_not())
      .then(expressions)
      .map(|((lhs, level), rhs)| Assertion {
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        level,
      })
  }
}

impl<'a> RecursivelyParseable<'a> for Assertion<'a> {
  type InnerExpr = Expression<'a>;
  type Tok = Token<'a>;

  fn parser<I>(
    inner: Boxed<'a, 'a, I, Self::InnerExpr, extra::Default>,
  ) -> Boxed<'a, 'a, I, Self, extra::Default>
  where I: ValueInput<'a, Token=Self::Tok, Span=SimpleSpan> {
    Self::assertions::<I>(inner).boxed()
  }
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ArrowApplication<'a> {
  pub source: Box<Expression<'a>>,
  pub target: Box<Expression<'a>>,
  pub direction: Direction,
  pub level: Level,
}

impl<'a> ArrowApplication<'a> {
  fn arrow_applications<I>(
    expressions: impl Parser<'a, I, Expression<'a>>+Clone+'a,
  ) -> impl Parser<'a, I, ArrowApplication<'a>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    let applications = select! {
      Token::Application(direction, level) => (direction, level),
    };
    expressions
      .clone()
      .then_ignore(just(Token::Whitespace).or_not())
      .then(applications)
      .then_ignore(just(Token::Whitespace).or_not())
      .then(expressions)
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
      .boxed()
  }
}

impl<'a> RecursivelyParseable<'a> for ArrowApplication<'a> {
  type InnerExpr = Expression<'a>;
  type Tok = Token<'a>;

  fn parser<I>(
    inner: Boxed<'a, 'a, I, Self::InnerExpr, extra::Default>,
  ) -> Boxed<'a, 'a, I, Self, extra::Default>
  where I: ValueInput<'a, Token=Self::Tok, Span=SimpleSpan> {
    Self::arrow_applications::<I>(inner).boxed()
  }
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub struct NamedCaseDeclaration<'a> {
  pub name: &'a str,
  pub immediates: Vec<BasicGroup<'a>>,
  pub application: Option<(Level, Box<Expression<'a>>)>,
}

impl<'a> NamedCaseDeclaration<'a> {
  fn case_declarations<I>(
    expressions: Boxed<'a, 'a, I, Expression<'a>, extra::Default>,
  ) -> impl Parser<'a, I, NamedCaseDeclaration<'a>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    let case_decls = select! {
      Token::NamedCaseDeclaration(name) => name,
    };
    /* NB: only select rightward arrows. */
    let arrows = select! {
      Token::Application(Direction::Right, level) => level,
    };

    let application = arrows
      .then_ignore(just(Token::Whitespace).or_not())
      .then(expressions.clone())
      .then_ignore(just(Token::Whitespace).or_not())
      .map(|(arrow_level, expr)| (arrow_level, Box::new(expr)))
      .or_not();

    case_decls
      .then(BasicGroup::parser(expressions.boxed()).repeated().collect::<Vec<_>>())
      .then(application)
      .map(|((name, immediates), application)| NamedCaseDeclaration {
        name,
        immediates,
        application,
      })
      /* compilation fails with a type too long error if this is omitted lol */
      .boxed()
  }
}

impl<'a> RecursivelyParseable<'a> for NamedCaseDeclaration<'a> {
  type InnerExpr = Expression<'a>;
  type Tok = Token<'a>;

  fn parser<I>(
    inner: Boxed<'a, 'a, I, Self::InnerExpr, extra::Default>,
  ) -> Boxed<'a, 'a, I, Self, extra::Default>
  where I: ValueInput<'a, Token=Self::Tok, Span=SimpleSpan> {
    Self::case_declarations::<I>(inner).boxed()
  }
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub struct UnnamedCaseDeclaration<'a> {
  pub value: Box<Expression<'a>>,
}

impl<'a> UnnamedCaseDeclaration<'a> {
  fn unnamed_case_decls<I>(
    expressions: Boxed<'a, 'a, I, Expression<'a>, extra::Default>,
  ) -> impl Parser<'a, I, UnnamedCaseDeclaration<'a>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    just(Token::UnnamedCaseDeclaration)
      .ignore_then(expressions)
      .map(|expr| UnnamedCaseDeclaration {
        value: Box::new(expr),
      })
  }
}

impl<'a> RecursivelyParseable<'a> for UnnamedCaseDeclaration<'a> {
  type InnerExpr = Expression<'a>;
  type Tok = Token<'a>;

  fn parser<I>(
    inner: Boxed<'a, 'a, I, Self::InnerExpr, extra::Default>,
  ) -> Boxed<'a, 'a, I, Self, extra::Default>
  where I: ValueInput<'a, Token=Self::Tok, Span=SimpleSpan> {
    Self::unnamed_case_decls::<I>(inner).boxed()
  }
}


trait LeveledGroup<'a> {
  fn within_value_group<I, T>(
    inner: Boxed<'a, 'a, I, T, extra::Default>,
  ) -> Boxed<'a, 'a, I, T, extra::Default>
  where
    I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan>,
    T: 'a,
  {
    let value_group_start = select! {
      Token::GroupStart(Level::Value) => (),
    };
    let value_group_end = select! {
      Token::GroupClose(Level::Value) => (),
    };
    inner
      .delimited_by(
        value_group_start.then(just(Token::Whitespace).or_not()),
        just(Token::Whitespace).or_not().then(value_group_end),
      )
      .boxed()
  }

  fn within_type_group<I, T>(
    inner: Boxed<'a, 'a, I, T, extra::Default>,
  ) -> Boxed<'a, 'a, I, T, extra::Default>
  where
    I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan>,
    T: 'a,
  {
    let type_group_start = select! {
      Token::GroupStart(Level::r#Type) => (),
    };
    let type_group_end = select! {
      Token::GroupClose(Level::r#Type) => (),
    };
    inner
      .delimited_by(
        type_group_start.then(just(Token::Whitespace).or_not()),
        just(Token::Whitespace).or_not().then(type_group_end),
      )
      .boxed()
  }
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub struct CaseDeconstruction<'a> {
  pub cases: Vec<NamedCaseDeclaration<'a>>,
  pub default_case: Option<UnnamedCaseDeclaration<'a>>,
  pub level: Level,
}

impl<'a> LeveledGroup<'a> for CaseDeconstruction<'a> {}

impl<'a> CaseDeconstruction<'a> {
  fn case_deconstructions<I>(
    expressions: Boxed<'a, 'a, I, Expression<'a>, extra::Default>,
  ) -> impl Parser<'a, I, CaseDeconstruction<'a>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    fn parse_decls<'a, I>(
      expressions: Boxed<'a, 'a, I, Expression<'a>, extra::Default>,
    ) -> impl Parser<'a, I, NamedCaseDeclaration<'a>>+Clone
    where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
      NamedCaseDeclaration::parser(expressions).then_ignore(just(Token::Whitespace).or_not())
    }
    fn parse_inner<'a, I>(
      expressions: Boxed<'a, 'a, I, Expression<'a>, extra::Default>,
    ) -> impl Parser<
      'a,
      I,
      (
        Vec<NamedCaseDeclaration<'a>>,
        Option<UnnamedCaseDeclaration<'a>>,
      ),
    >+Clone
    where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
      just(Token::Whitespace)
        .or_not()
        .ignore_then(
          parse_decls(expressions.clone())
            .repeated()
            .at_least(1)
            .collect::<Vec<_>>(),
        )
        .then(
          UnnamedCaseDeclaration::parser(expressions.boxed())
            .then_ignore(just(Token::Whitespace).or_not())
            .or_not(),
        )
    }

    choice((
      just(Token::FreeCaseMarker)
        .ignore_then(Self::within_value_group(
          parse_inner(expressions.clone()).boxed(),
        ))
        .map(|(cases, default_case)| CaseDeconstruction {
          cases,
          default_case,
          level: Level::Value,
        }),
      just(Token::FreeCaseMarker)
        .ignore_then(Self::within_type_group(
          parse_inner(expressions.clone()).boxed(),
        ))
        .map(|(cases, default_case)| CaseDeconstruction {
          cases,
          default_case,
          level: Level::r#Type,
        }),
    ))
  }
}

impl<'a> RecursivelyParseable<'a> for CaseDeconstruction<'a> {
  type InnerExpr = Expression<'a>;
  type Tok = Token<'a>;

  fn parser<I>(
    inner: Boxed<'a, 'a, I, Self::InnerExpr, extra::Default>,
  ) -> Boxed<'a, 'a, I, Self, extra::Default>
  where I: ValueInput<'a, Token=Self::Tok, Span=SimpleSpan> {
    Self::case_deconstructions::<I>(inner).boxed()
  }
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub struct SerialGroup<'a> {
  pub ordered_statements: Vec<Box<Expression<'a>>>,
  pub return_value: Box<Expression<'a>>,
  pub level: Level,
}

impl<'a> LeveledGroup<'a> for SerialGroup<'a> {}

impl<'a> SerialGroup<'a> {
  fn serial_groups<I>(
    expressions: Boxed<'a, 'a, I, Expression<'a>, extra::Default>,
  ) -> impl Parser<'a, I, SerialGroup<'a>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    /* (; <expr>)+ */
    fn parse_statements<'a, I>(
      expressions: impl Parser<'a, I, Expression<'a>>+Clone,
    ) -> impl Parser<'a, I, Vec<Expression<'a>>>+Clone
    where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
      just(Token::SerialSeparator)
        .ignore_then(just(Token::Whitespace).or_not())
        .ignore_then(expressions)
        .then_ignore(just(Token::Whitespace).or_not())
        .repeated()
        .at_least(1)
        .collect::<Vec<_>>()
    }
    /* : <expr> */
    fn parse_return<'a, I>(
      expressions: impl Parser<'a, I, Expression<'a>>+Clone,
    ) -> impl Parser<'a, I, Expression<'a>>+Clone
    where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
      just(Token::FreeSerialReturn)
        .ignore_then(just(Token::Whitespace).or_not())
        .ignore_then(expressions)
    }

    /* ( (; <expr>)+ : <expr> ) */
    fn parse_inner<'a, I>(
      expressions: impl Parser<'a, I, Expression<'a>>+Clone,
    ) -> impl Parser<'a, I, (Vec<Expression<'a>>, Expression<'a>)>+Clone
    where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
      just(Token::Whitespace)
        .or_not()
        .ignore_then(parse_statements(expressions.clone()))
        .then(parse_return(expressions))
        .then_ignore(just(Token::Whitespace).or_not())
    }

    choice((
      just(Token::SerialSeparator)
        .ignore_then(Self::within_value_group(
          parse_inner(expressions.clone()).boxed(),
        ))
        .map(|(statements, ret)| SerialGroup {
          ordered_statements: statements.into_iter().map(Box::new).collect(),
          return_value: Box::new(ret),
          level: Level::Value,
        }),
      just(Token::SerialSeparator)
        .ignore_then(Self::within_type_group(parse_inner(expressions).boxed()))
        .map(|(statements, ret)| SerialGroup {
          ordered_statements: statements.into_iter().map(Box::new).collect(),
          return_value: Box::new(ret),
          level: Level::r#Type,
        }),
    ))
  }
}

impl<'a> RecursivelyParseable<'a> for SerialGroup<'a> {
  type InnerExpr = Expression<'a>;
  type Tok = Token<'a>;

  fn parser<I>(
    inner: Boxed<'a, 'a, I, Self::InnerExpr, extra::Default>,
  ) -> Boxed<'a, 'a, I, Self, extra::Default>
  where I: ValueInput<'a, Token=Self::Tok, Span=SimpleSpan> {
    Self::serial_groups::<I>(inner).boxed()
  }
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub struct BasicGroup<'a> {
  pub inner: Box<Expression<'a>>,
  pub level: Level,
}

impl<'a> LeveledGroup<'a> for BasicGroup<'a> {}

impl<'a> BasicGroup<'a> {
  fn basic_groups<I>(
    expressions: Boxed<'a, 'a, I, Expression<'a>, extra::Default>,
  ) -> impl Parser<'a, I, BasicGroup<'a>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    fn parse_inner<'a, I>(
      expressions: impl Parser<'a, I, Expression<'a>>+Clone,
    ) -> impl Parser<'a, I, Expression<'a>>+Clone
    where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
      just(Token::Whitespace)
        .or_not()
        .ignore_then(expressions)
        .then_ignore(just(Token::Whitespace).or_not())
    }

    choice((
      Self::within_value_group(parse_inner(expressions.clone()).boxed()).map(|expr| BasicGroup {
        inner: Box::new(expr),
        level: Level::Value,
      }),
      Self::within_type_group(parse_inner(expressions).boxed()).map(|expr| BasicGroup {
        inner: Box::new(expr),
        level: Level::r#Type,
      }),
    ))
  }
}

impl<'a> RecursivelyParseable<'a> for BasicGroup<'a> {
  type InnerExpr = Expression<'a>;
  type Tok = Token<'a>;

  fn parser<I>(
    inner: Boxed<'a, 'a, I, Self::InnerExpr, extra::Default>,
  ) -> Boxed<'a, 'a, I, Self, extra::Default>
  where I: ValueInput<'a, Token=Self::Tok, Span=SimpleSpan> {
    Self::basic_groups::<I>(inner).boxed()
  }
}


pub trait HasInsideTypeSpec<'a> {
  fn within_type_spec<I, T>(
    inner: Boxed<'a, 'a, I, T, extra::Default>,
  ) -> Boxed<'a, 'a, I, T, extra::Default>
  where
    I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan>,
    T: 'a,
  {
    inner
      .delimited_by(
        just(Token::TypeSpecStart).then(just(Token::Whitespace).or_not()),
        just(Token::Whitespace)
          .or_not()
          .then(just(Token::TypeSpecClose)),
      )
      .boxed()
  }
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TypeSpec<'a> {
  pub inner: Box<Expression<'a>>,
}

impl<'a> HasInsideTypeSpec<'a> for TypeSpec<'a> {}

impl<'a> TypeSpec<'a> {
  fn type_specs<I>(
    expressions: Boxed<'a, 'a, I, Expression<'a>, extra::Default>,
  ) -> impl Parser<'a, I, TypeSpec<'a>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    Self::within_type_spec(expressions).map(|expr| TypeSpec {
      inner: Box::new(expr),
    })
  }
}

impl<'a> RecursivelyParseable<'a> for TypeSpec<'a> {
  type InnerExpr = Expression<'a>;
  type Tok = Token<'a>;

  fn parser<I>(
    inner: Boxed<'a, 'a, I, Self::InnerExpr, extra::Default>,
  ) -> Boxed<'a, 'a, I, Self, extra::Default>
  where I: ValueInput<'a, Token=Self::Tok, Span=SimpleSpan> {
    Self::type_specs::<I>(inner).boxed()
  }
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Group<'a> {
  Basic(BasicGroup<'a>),
  Serial(SerialGroup<'a>),
  TypeSpec(TypeSpec<'a>),
  Case(CaseDeconstruction<'a>),
}

impl<'a> Group<'a> {
  fn groups<I>(
    expressions: Boxed<'a, 'a, I, Expression<'a>, extra::Default>,
  ) -> impl Parser<'a, I, Group<'a>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    choice((
      SerialGroup::parser(expressions.clone()).map(Group::Serial),
      CaseDeconstruction::parser(expressions.clone()).map(Group::Case),
      BasicGroup::parser(expressions.clone()).map(Group::Basic),
      TypeSpec::parser(expressions).map(Group::TypeSpec),
    ))
    .boxed()
  }
}

impl<'a> RecursivelyParseable<'a> for Group<'a> {
  type InnerExpr = Expression<'a>;
  type Tok = Token<'a>;

  fn parser<I>(
    inner: Boxed<'a, 'a, I, Self::InnerExpr, extra::Default>,
  ) -> Boxed<'a, 'a, I, Self, extra::Default>
  where I: ValueInput<'a, Token=Self::Tok, Span=SimpleSpan> {
    Self::groups::<I>(inner).boxed()
  }
}


#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct LocalNameDereference<'a> {
  pub name: &'a str,
}

impl<'a> LocalNameDereference<'a> {
  fn local_name_dereferences<I>() -> impl Parser<'a, I, LocalNameDereference<'a>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    select! {
      Token::LocalDereference(name) => LocalNameDereference { name }
    }
  }
}

impl<'a> TokenParseable<'a> for LocalNameDereference<'a> {
  type Tok = Token<'a>;

  fn parser<I>() -> Boxed<'a, 'a, I, Self, extra::Default>
  where I: ValueInput<'a, Token=Self::Tok, Span=SimpleSpan> {
    Self::local_name_dereferences::<I>().boxed()
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

impl<'a> LeftwardImmediateSource<'a> {
  fn leftward_immediate_sources<I>(
    expressions: Boxed<'a, 'a, I, Expression<'a>, extra::Default>,
  ) -> impl Parser<'a, I, LeftwardImmediateSource<'a>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    choice((
      LocalNameDereference::parser().map(LeftwardImmediateSource::LocalNameDereference),
      CaseValueAssertion::parser().map(LeftwardImmediateSource::CaseValueAssertion),
      GlobalName::parser().map(LeftwardImmediateSource::GlobalName),
      Group::parser(expressions).map(LeftwardImmediateSource::Group),
    ))
  }
}

impl<'a> RecursivelyParseable<'a> for LeftwardImmediateSource<'a> {
  type InnerExpr = Expression<'a>;
  type Tok = Token<'a>;

  fn parser<I>(
    inner: Boxed<'a, 'a, I, Self::InnerExpr, extra::Default>,
  ) -> Boxed<'a, 'a, I, Self, extra::Default>
  where I: ValueInput<'a, Token=Self::Tok, Span=SimpleSpan> {
    Self::leftward_immediate_sources::<I>(inner).boxed()
  }
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ImmediateApplication<'a> {
  pub target: Box<Expression<'a>>,
  pub source: LeftwardImmediateSource<'a>,
}

impl<'a> ImmediateApplication<'a> {
  fn immediate_applications<I>(
    expressions: Boxed<'a, 'a, I, Expression<'a>, extra::Default>,
  ) -> impl Parser<'a, I, ImmediateApplication<'a>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    expressions
      .clone()
      .then(LeftwardImmediateSource::parser(expressions))
      .map(|(target, source)| ImmediateApplication {
        target: Box::new(target),
        source,
      })
      .boxed()
  }
}

impl<'a> RecursivelyParseable<'a> for ImmediateApplication<'a> {
  type InnerExpr = Expression<'a>;
  type Tok = Token<'a>;

  fn parser<I>(
    inner: Boxed<'a, 'a, I, Self::InnerExpr, extra::Default>,
  ) -> Boxed<'a, 'a, I, Self, extra::Default>
  where I: ValueInput<'a, Token=Self::Tok, Span=SimpleSpan> {
    Self::immediate_applications::<I>(inner).boxed()
  }
}


#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct NumericLiteral<'a> {
  pub value: &'a str,
}

impl<'a> NumericLiteral<'a> {
  fn numbers<I>() -> impl Parser<'a, I, NumericLiteral<'a>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    select! {
      Token::NumericLiteral(value) => NumericLiteral { value },
    }
  }
}

impl<'a> TokenParseable<'a> for NumericLiteral<'a> {
  type Tok = Token<'a>;

  fn parser<I>() -> Boxed<'a, 'a, I, Self, extra::Default>
  where I: ValueInput<'a, Token=Self::Tok, Span=SimpleSpan> {
    Self::numbers::<I>().boxed()
  }
}


#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct StringLiteral<'a> {
  pub value: &'a str,
}

impl<'a> StringLiteral<'a> {
  fn strings<I>() -> impl Parser<'a, I, StringLiteral<'a>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    select! {
      Token::StringLiteral(value) => StringLiteral { value },
    }
  }
}

impl<'a> TokenParseable<'a> for StringLiteral<'a> {
  type Tok = Token<'a>;

  fn parser<I>() -> Boxed<'a, 'a, I, Self, extra::Default>
  where I: ValueInput<'a, Token=Self::Tok, Span=SimpleSpan> {
    Self::strings::<I>().boxed()
  }
}


#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum Literal<'a> {
  Num(NumericLiteral<'a>),
  Str(StringLiteral<'a>),
}

impl<'a> Literal<'a> {
  fn literals<I>() -> impl Parser<'a, I, Literal<'a>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    choice((
      NumericLiteral::parser().map(Literal::Num),
      StringLiteral::parser().map(Literal::Str),
    ))
  }
}

impl<'a> TokenParseable<'a> for Literal<'a> {
  type Tok = Token<'a>;

  fn parser<I>() -> Boxed<'a, 'a, I, Self, extra::Default>
  where I: ValueInput<'a, Token=Self::Tok, Span=SimpleSpan> {
    Self::literals::<I>().boxed()
  }
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expression<'a> {
  Name(Name<'a>),
  Lit(Literal<'a>),
  Assertion(Assertion<'a>),
  Arrow(ArrowApplication<'a>),
  Immediate(ImmediateApplication<'a>),
  Join(ParallelJoin<'a>),
  Group(Group<'a>),
}

impl<'a> Expression<'a> {
  fn expressions<I>() -> impl Parser<'a, I, Expression<'a>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    recursive(|expressions| {
      choice((
        Literal::parser().map(Expression::Lit),
        Name::parser().map(Expression::Name),
        ImmediateApplication::parser(expressions.clone().boxed()).map(Expression::Immediate),
        Assertion::parser(expressions.clone().boxed()).map(Expression::Assertion),
        ParallelJoin::parser(expressions.clone().boxed()).map(Expression::Join),
        Group::parser(expressions.clone().boxed()).map(Expression::Group),
        ArrowApplication::parser(expressions.boxed()).map(Expression::Arrow),
      ))
    })
    .boxed()
  }
}

impl<'a> TokenParseable<'a> for Expression<'a> {
  type Tok = Token<'a>;

  fn parser<I>() -> Boxed<'a, 'a, I, Self, extra::Default>
  where I: ValueInput<'a, Token=Self::Tok, Span=SimpleSpan> {
    Self::expressions::<I>().boxed()
  }
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub struct GlobalPlaceExpression<'a> {
  pub place: GlobalName<'a>,
  pub immediates: Vec<BasicGroup<'a>>,
}

impl<'a> GlobalPlaceExpression<'a> {
  fn global_place_expressions<I>() -> impl Parser<'a, I, GlobalPlaceExpression<'a>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    GlobalName::parser()
      .then(
        BasicGroup::parser(Expression::parser())
          .repeated()
          .collect::<Vec<_>>(),
      )
      .map(|(place, immediates)| GlobalPlaceExpression { place, immediates })
  }
}

impl<'a> TokenParseable<'a> for GlobalPlaceExpression<'a> {
  type Tok = Token<'a>;

  fn parser<I>() -> Boxed<'a, 'a, I, Self, extra::Default>
  where I: ValueInput<'a, Token=Self::Tok, Span=SimpleSpan> {
    Self::global_place_expressions::<I>().boxed()
  }
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub struct GlobalAssignment<'a> {
  pub place: GlobalPlaceExpression<'a>,
  pub value: Expression<'a>,
  pub direction: Direction,
  pub level: Level,
}

impl<'a> GlobalAssignment<'a> {
  fn global_assignments<I>() -> impl Parser<'a, I, GlobalAssignment<'a>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    let left_arrows = select! {
      Token::GlobalAssignment(Direction::Left, level) => level,
    };
    let right_arrows = select! {
      Token::GlobalAssignment(Direction::Right, level) => level,
    };
    choice((
      GlobalPlaceExpression::parser()
        .then(left_arrows)
        .then(Expression::parser())
        .map(|((place, level), value)| GlobalAssignment {
          place,
          value,
          direction: Direction::Left,
          level,
        }),
      Expression::parser()
        .then(right_arrows)
        .then(GlobalPlaceExpression::parser())
        .map(|((value, level), place)| GlobalAssignment {
          place,
          value,
          direction: Direction::Right,
          level,
        }),
    ))
  }
}

impl<'a> TokenParseable<'a> for GlobalAssignment<'a> {
  type Tok = Token<'a>;

  fn parser<I>() -> Boxed<'a, 'a, I, Self, extra::Default>
  where I: ValueInput<'a, Token=Self::Tok, Span=SimpleSpan> {
    Self::global_assignments::<I>().boxed()
  }
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TopLevelStatement<'a> {
  GlobalAssignment(GlobalAssignment<'a>),
  SimpleExpression(Expression<'a>),
  NamespaceEdit(NamespaceEdit<'a>),
  ImportHighlight(ImportHighlight<'a>),
}

impl<'a> TopLevelStatement<'a> {
  fn top_level_statements<I>() -> impl Parser<'a, I, TopLevelStatement<'a>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    recursive(|inner| {
      choice((
        GlobalAssignment::parser().map(TopLevelStatement::GlobalAssignment),
        Expression::parser().map(TopLevelStatement::SimpleExpression),
        NamespaceEdit::parser(inner.boxed()).map(TopLevelStatement::NamespaceEdit),
        ImportHighlight::parser().map(TopLevelStatement::ImportHighlight),
      ))
    })
  }
}

impl<'a> TokenParseable<'a> for TopLevelStatement<'a> {
  type Tok = Token<'a>;

  fn parser<I>() -> Boxed<'a, 'a, I, Self, extra::Default>
  where I: ValueInput<'a, Token=Self::Tok, Span=SimpleSpan> {
    Self::top_level_statements::<I>().boxed()
  }
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub struct NamespacePath<'a> {
  pub components: Vec<NamespaceComponent<'a>>,
}

impl<'a> NamespacePath<'a> {
  fn namespace_paths<I>() -> impl Parser<'a, I, NamespacePath<'a>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    NamespaceComponent::parser()
      .repeated()
      .at_least(1)
      .collect::<Vec<_>>()
      .map(|components| NamespacePath { components })
  }
}

impl<'a> TokenParseable<'a> for NamespacePath<'a> {
  type Tok = Token<'a>;

  fn parser<I>() -> Boxed<'a, 'a, I, Self, extra::Default>
  where I: ValueInput<'a, Token=Self::Tok, Span=SimpleSpan> {
    Self::namespace_paths::<I>().boxed()
  }
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ModuleContent<'a> {
  pub statements: Vec<Box<TopLevelStatement<'a>>>,
}

impl<'a> ModuleContent<'a> {
  fn module_contents<I>(
    inner: Boxed<'a, 'a, I, TopLevelStatement<'a>, extra::Default>,
  ) -> impl Parser<'a, I, ModuleContent<'a>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    inner
      .separated_by(just(Token::Whitespace).or_not())
      .allow_leading()
      .allow_trailing()
      .collect::<Vec<_>>()
      .map(|statements| ModuleContent {
        statements: statements.into_iter().map(Box::new).collect(),
      })
  }
}

impl<'a> RecursivelyParseable<'a> for ModuleContent<'a> {
  type InnerExpr = TopLevelStatement<'a>;
  type Tok = Token<'a>;

  fn parser<I>(
    inner: Boxed<'a, 'a, I, Self::InnerExpr, extra::Default>,
  ) -> Boxed<'a, 'a, I, Self, extra::Default>
  where I: ValueInput<'a, Token=Self::Tok, Span=SimpleSpan> {
    Self::module_contents::<I>(inner).boxed()
  }
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub struct NamespaceEdit<'a> {
  pub path: NamespacePath<'a>,
  pub content: ModuleContent<'a>,
}

impl<'a> HasInsideNamespace<'a> for NamespaceEdit<'a> {}

impl<'a> NamespaceEdit<'a> {
  fn namespace_edits<I>(
    inner: Boxed<'a, 'a, I, TopLevelStatement<'a>, extra::Default>,
  ) -> impl Parser<'a, I, NamespaceEdit<'a>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    just(Token::ImportEdit)
      .ignore_then(NamespacePath::parser())
      .then_ignore(just(Token::Whitespace).or_not())
      .then(Self::within_namespace(ModuleContent::parser(inner)))
      .map(|(path, content)| NamespaceEdit { path, content })
  }
}

impl<'a> RecursivelyParseable<'a> for NamespaceEdit<'a> {
  type InnerExpr = TopLevelStatement<'a>;
  type Tok = Token<'a>;

  fn parser<I>(
    inner: Boxed<'a, 'a, I, Self::InnerExpr, extra::Default>,
  ) -> Boxed<'a, 'a, I, Self, extra::Default>
  where I: ValueInput<'a, Token=Self::Tok, Span=SimpleSpan> {
    Self::namespace_edits::<I>(inner).boxed()
  }
}


#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd, Default)]
pub enum LiftStatus {
  #[default]
  NotLifted,
  Lifted,
}


#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd, Default)]
pub enum NamespaceRewriteStatus<'a> {
  Rewritten(NamespaceComponent<'a>),
  #[default]
  NotRewritten,
}


#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct IntermediateImportComponent<'a> {
  pub name: NamespaceComponent<'a>,
  pub lifted: LiftStatus,
  pub rewritten: NamespaceRewriteStatus<'a>,
}

impl<'a> IntermediateImportComponent<'a> {
  fn intermediate_import_components<I>() -> impl Parser<'a, I, IntermediateImportComponent<'a>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    just(Token::ImportLift)
      .or_not()
      .then(NamespaceComponent::parser())
      .then(
        just(Token::Whitespace)
          .or_not()
          .ignore_then(just(Token::ImportEdit))
          .ignore_then(just(Token::Whitespace).or_not())
          .ignore_then(NamespaceComponent::parser())
          .or_not(),
      )
      .map(
        |((lift_result, name), rewrite_result)| IntermediateImportComponent {
          name,
          lifted: if lift_result.is_some() {
            LiftStatus::Lifted
          } else {
            LiftStatus::NotLifted
          },
          rewritten: match rewrite_result {
            Some(x) => NamespaceRewriteStatus::Rewritten(x),
            None => NamespaceRewriteStatus::NotRewritten,
          },
        },
      )
  }
}

impl<'a> TokenParseable<'a> for IntermediateImportComponent<'a> {
  type Tok = Token<'a>;

  fn parser<I>() -> Boxed<'a, 'a, I, Self, extra::Default>
  where I: ValueInput<'a, Token=Self::Tok, Span=SimpleSpan> {
    Self::intermediate_import_components::<I>().boxed()
  }
}


#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd, Default)]
pub enum ValueRewriteStatus<'a> {
  Rewritten(&'a str),
  #[default]
  NotRewritten,
}


#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct TerminalImportComponent<'a> {
  pub name: &'a str,
  pub lifted: LiftStatus,
  pub rewritten: ValueRewriteStatus<'a>,
}

impl<'a> TerminalImportComponent<'a> {
  fn global_symbol<I>() -> impl Parser<'a, I, &'a str>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    select! {
      Token::GlobalDereference(name) => name,
    }
  }

  fn terminal_import_components<I>() -> impl Parser<'a, I, TerminalImportComponent<'a>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    just(Token::ImportLift)
      .or_not()
      .then(Self::global_symbol())
      .then(
        just(Token::Whitespace)
          .or_not()
          .ignore_then(just(Token::ImportEdit))
          .ignore_then(just(Token::Whitespace).or_not())
          .ignore_then(Self::global_symbol())
          .or_not(),
      )
      .map(
        |((lift_result, name), rewrite_result)| TerminalImportComponent {
          name,
          lifted: if lift_result.is_some() {
            LiftStatus::Lifted
          } else {
            LiftStatus::NotLifted
          },
          rewritten: match rewrite_result {
            Some(x) => ValueRewriteStatus::Rewritten(x),
            None => ValueRewriteStatus::NotRewritten,
          },
        },
      )
  }
}

impl<'a> TokenParseable<'a> for TerminalImportComponent<'a> {
  type Tok = Token<'a>;

  fn parser<I>() -> Boxed<'a, 'a, I, Self, extra::Default>
  where I: ValueInput<'a, Token=Self::Tok, Span=SimpleSpan> {
    Self::terminal_import_components::<I>().boxed()
  }
}


pub trait HasInsideNamespace<'a> {
  fn within_namespace<I, T>(
    inner: Boxed<'a, 'a, I, T, extra::Default>,
  ) -> Boxed<'a, 'a, I, T, extra::Default>
  where
    I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan>,
    T: 'a,
  {
    inner
      .delimited_by(
        just(Token::NamespaceStart).then(just(Token::Whitespace).or_not()),
        just(Token::Whitespace)
          .or_not()
          .then(just(Token::NamespaceClose)),
      )
      .boxed()
  }
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ImportTailComponent<'a> {
  Value(TerminalImportComponent<'a>),
  Namespace(IntermediateImportComponent<'a>, Option<Box<ImportTail<'a>>>),
}

impl<'a> HasInsideNamespace<'a> for ImportTailComponent<'a> {}

impl<'a> ImportTailComponent<'a> {
  fn import_tail_components<I>(
    inner: Boxed<'a, 'a, I, ImportTail<'a>, extra::Default>,
  ) -> impl Parser<'a, I, ImportTailComponent<'a>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    choice((
      TerminalImportComponent::parser().map(ImportTailComponent::Value),
      IntermediateImportComponent::parser()
        .then(Self::within_namespace(inner).or_not())
        .map(|(component, tail)| ImportTailComponent::Namespace(component, tail.map(Box::new))),
    ))
  }
}

impl<'a> RecursivelyParseable<'a> for ImportTailComponent<'a> {
  type InnerExpr = ImportTail<'a>;
  type Tok = Token<'a>;

  fn parser<I>(
    inner: Boxed<'a, 'a, I, Self::InnerExpr, extra::Default>,
  ) -> Boxed<'a, 'a, I, Self, extra::Default>
  where I: ValueInput<'a, Token=Self::Tok, Span=SimpleSpan> {
    Self::import_tail_components::<I>(inner).boxed()
  }
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ImportTail<'a> {
  pub components: Vec<ImportTailComponent<'a>>,
}

impl<'a> HasArgSep<'a> for ImportTail<'a> {}

impl<'a> ImportTail<'a> {
  fn import_tails<I>() -> impl Parser<'a, I, ImportTail<'a>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    recursive(|inner| {
      ImportTailComponent::parser(inner.boxed())
        .separated_by(Self::arg_sep())
        .allow_trailing()
        .collect::<Vec<_>>()
        .map(|components| ImportTail { components })
    })
  }
}

impl<'a> TokenParseable<'a> for ImportTail<'a> {
  type Tok = Token<'a>;

  fn parser<I>() -> Boxed<'a, 'a, I, Self, extra::Default>
  where I: ValueInput<'a, Token=Self::Tok, Span=SimpleSpan> {
    Self::import_tails::<I>().boxed()
  }
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ImportHighlight<'a> {
  pub prefix: Vec<IntermediateImportComponent<'a>>,
  pub tail: Option<ImportTail<'a>>,
}

impl<'a> HasInsideNamespace<'a> for ImportHighlight<'a> {}

impl<'a> ImportHighlight<'a> {
  fn import_highlights<I>() -> impl Parser<'a, I, ImportHighlight<'a>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    just(Token::ImportLift)
      .ignore_then(
        IntermediateImportComponent::parser()
          .repeated()
          .collect::<Vec<_>>(),
      )
      .then(Self::within_namespace(ImportTail::parser()).or_not())
      .map(|(prefix, tail)| ImportHighlight { prefix, tail })
  }
}

impl<'a> TokenParseable<'a> for ImportHighlight<'a> {
  type Tok = Token<'a>;

  fn parser<I>() -> Boxed<'a, 'a, I, Self, extra::Default>
  where I: ValueInput<'a, Token=Self::Tok, Span=SimpleSpan> {
    Self::import_highlights::<I>().boxed()
  }
}


#[cfg(test)]
mod tests {
  use super::*;
  #[test]
  fn tokenize_example() {
    let result = Token::parser()
      .repeated()
      .collect_exactly::<[_; 2]>()
      .parse(".x.y")
      .into_result()
      .unwrap();
    assert_eq!(result, [
      Token::LocalDereference("x"),
      Token::LocalDereference("y")
    ]);
    let result = Token::parser()
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
  fn parseable_impl_token() {
    let result = Token::parser().parse(".x").into_result().unwrap();
    assert_eq!(result, Token::LocalDereference("x"));
  }

  #[test]
  fn parse_local_names() {
    let token_stream = Token::parser()
      .repeated()
      .stream(".x")
      .into_result()
      .unwrap();
    let result = LocalName::parser()
      .parse(token_stream)
      .into_result()
      .unwrap();
    assert_eq!(result, LocalName {
      name: "x",
      eval_direction: EvalDirection::Dereference
    });
  }

  #[test]
  fn parse_local_into_name() {
    let token_stream = Token::parser()
      .repeated()
      .stream(".x")
      .into_result()
      .unwrap();
    let result = Name::parser().parse(token_stream).into_result().unwrap();
    assert_eq!(
      result,
      Name::Local(LocalName {
        name: "x",
        eval_direction: EvalDirection::Dereference
      })
    );
  }

  #[test]
  fn parse_expr_local_name() {
    let token_stream = Token::parser()
      .repeated()
      .stream(".x")
      .into_result()
      .unwrap();
    let result = Expression::parser()
      .parse(token_stream)
      .into_result()
      .unwrap();
    assert_eq!(
      result,
      Expression::Name(Name::Local(LocalName {
        name: "x",
        eval_direction: EvalDirection::Dereference
      }))
    );
  }

  #[test]
  fn parse_import_highlight() {
    let result = Token::parser()
      .repeated()
      .collect::<Vec<_>>()
      .parse("<:package>:a{<:module, <$function, :inner-mod{<$f > $h, <:mod>:other-mod{<$g}}}")
      .into_result()
      .unwrap();
    assert_eq!(result, vec![
      Token::ImportLift,
      Token::NamespaceDereference("package"),
      Token::ImportEdit,
      Token::NamespaceDereference("a"),
      Token::NamespaceStart,
      Token::ImportLift,
      Token::NamespaceDereference("module"),
      Token::ParallelSeparator,
      Token::Whitespace,
      Token::ImportLift,
      Token::GlobalDereference("function"),
      Token::ParallelSeparator,
      Token::Whitespace,
      Token::NamespaceDereference("inner-mod"),
      Token::NamespaceStart,
      Token::ImportLift,
      Token::GlobalDereference("f"),
      Token::Whitespace,
      Token::ImportEdit,
      Token::Whitespace,
      Token::GlobalDereference("h"),
      Token::ParallelSeparator,
      Token::Whitespace,
      Token::ImportLift,
      Token::NamespaceDereference("mod"),
      Token::ImportEdit,
      Token::NamespaceDereference("other-mod"),
      Token::NamespaceStart,
      Token::ImportLift,
      Token::GlobalDereference("g"),
      Token::NamespaceClose,
      Token::NamespaceClose,
      Token::NamespaceClose,
    ]);

    let token_stream = Token::parser()
      .repeated()
      .stream(
        "<:package>:a{<:module, <$function, :inner-mod{<$f > $h,
    <:mod>:other-mod{<$g}}}",
      )
      .into_result()
      .unwrap();
    let result = ImportHighlight::parser()
      .parse(token_stream)
      .into_result()
      .unwrap();
    assert_eq!(result, ImportHighlight {
      prefix: vec![IntermediateImportComponent {
        name: NamespaceComponent {
          component: "package",
        },
        /* The top-level package is not marked as "lifted", because this would be a tautology. */
        lifted: LiftStatus::NotLifted,
        rewritten: NamespaceRewriteStatus::Rewritten(NamespaceComponent { component: "a" }),
      }],
      tail: Some(ImportTail {
        components: vec![
          ImportTailComponent::Namespace(
            IntermediateImportComponent {
              name: NamespaceComponent {
                component: "module"
              },
              lifted: LiftStatus::Lifted,
              rewritten: NamespaceRewriteStatus::NotRewritten,
            },
            None
          ),
          ImportTailComponent::Value(TerminalImportComponent {
            name: "function",
            lifted: LiftStatus::Lifted,
            rewritten: ValueRewriteStatus::NotRewritten,
          }),
          ImportTailComponent::Namespace(
            IntermediateImportComponent {
              name: NamespaceComponent {
                component: "inner-mod"
              },
              lifted: LiftStatus::NotLifted,
              rewritten: NamespaceRewriteStatus::NotRewritten,
            },
            Some(Box::new(ImportTail {
              components: vec![
                ImportTailComponent::Value(TerminalImportComponent {
                  name: "f",
                  lifted: LiftStatus::Lifted,
                  rewritten: ValueRewriteStatus::Rewritten("h"),
                }),
                ImportTailComponent::Namespace(
                  IntermediateImportComponent {
                    name: NamespaceComponent { component: "mod" },
                    lifted: LiftStatus::Lifted,
                    rewritten: NamespaceRewriteStatus::Rewritten(NamespaceComponent {
                      component: "other-mod"
                    })
                  },
                  Some(Box::new(ImportTail {
                    components: vec![ImportTailComponent::Value(TerminalImportComponent {
                      name: "g",
                      lifted: LiftStatus::Lifted,
                      rewritten: ValueRewriteStatus::NotRewritten,
                    })]
                  }))
                ),
              ]
            }))
          ),
        ]
      }),
    });
  }
}
