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
  ImportHighlight(&'a str),
  Whitespace,
}

impl<'a> Token<'a> {
  fn global_symbol() -> impl Parser<'a, &'a str, &'a str> { regex("[a-zA-Z][a-zA-Z0-9_-]*") }

  fn local_symbol() -> impl Parser<'a, &'a str, &'a str> { regex("[a-zA-Z_-][a-zA-Z0-9_-]*") }

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
        .ignore_then(Self::global_symbol())
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
      just("$$")
        .ignore_then(Self::global_symbol())
        .map(Token::ImportHighlight),
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


#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ParallelJoin<'a> {
  pub exprs: Vec<Box<Expression<'a>>>,
}

impl<'a> ParallelJoin<'a> {
  fn parallel_joins<I>(
    expressions: impl Parser<'a, I, Expression<'a>>+Clone+'a,
  ) -> impl Parser<'a, I, ParallelJoin<'a>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    let joined = just(Token::Whitespace)
      .or_not()
      .ignore_then(just(Token::ParallelSeparator))
      .ignore_then(just(Token::Whitespace).or_not())
      .ignore_then(expressions.clone())
      .repeated()
      .at_least(1)
      .collect::<Vec<_>>();
    expressions
      .then(joined)
      .map(|(expr, joined)| {
        let mut ret: Vec<Expression<'a>> = vec![expr];
        ret.extend(joined.into_iter());
        ParallelJoin {
          exprs: ret.into_iter().map(Box::new).collect(),
        }
      })
      .boxed()
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
  fn value_group_start<I>() -> Boxed<'a, 'a, I, (), extra::Default>
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    select! {
      Token::GroupStart(Level::Value) => (),
    }
    .boxed()
  }
  fn type_group_start<I>() -> Boxed<'a, 'a, I, (), extra::Default>
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    select! {
      Token::GroupStart(Level::r#Type) => (),
    }
    .boxed()
  }
  fn value_group_end<I>() -> Boxed<'a, 'a, I, (), extra::Default>
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    select! {
      Token::GroupClose(Level::Value) => (),
    }
    .boxed()
  }
  fn type_group_end<I>() -> Boxed<'a, 'a, I, (), extra::Default>
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    select! {
      Token::GroupClose(Level::r#Type) => (),
    }
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
        .ignore_then(Self::value_group_start())
        .ignore_then(parse_inner(expressions.clone()))
        .then_ignore(Self::value_group_end())
        .map(|(cases, default_case)| CaseDeconstruction {
          cases,
          default_case,
          level: Level::Value,
        }),
      just(Token::FreeCaseMarker)
        .ignore_then(Self::type_group_start())
        .ignore_then(parse_inner(expressions))
        .then_ignore(Self::type_group_end())
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
        .ignore_then(Self::value_group_start())
        .ignore_then(parse_inner(expressions.clone()))
        .then_ignore(Self::value_group_end())
        .map(|(statements, ret)| SerialGroup {
          ordered_statements: statements.into_iter().map(Box::new).collect(),
          return_value: Box::new(ret),
          level: Level::Value,
        }),
      just(Token::SerialSeparator)
        .ignore_then(Self::type_group_start())
        .ignore_then(parse_inner(expressions))
        .then_ignore(Self::type_group_end())
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
      Self::value_group_start()
        .ignore_then(parse_inner(expressions.clone()))
        .then_ignore(Self::value_group_end())
        .map(|expr| BasicGroup {
          inner: Box::new(expr),
          level: Level::Value,
        }),
      Self::type_group_start()
        .ignore_then(parse_inner(expressions))
        .then_ignore(Self::type_group_end())
        .map(|expr| BasicGroup {
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


#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TypeSpec<'a> {
  pub inner: Box<Expression<'a>>,
}

impl<'a> TypeSpec<'a> {
  fn type_specs<I>(
    expressions: Boxed<'a, 'a, I, Expression<'a>, extra::Default>,
  ) -> impl Parser<'a, I, TypeSpec<'a>>+Clone
  where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> {
    just(Token::TypeSpecStart)
      .ignore_then(just(Token::Whitespace).or_not())
      .ignore_then(expressions)
      .then_ignore(just(Token::Whitespace).or_not())
      .then_ignore(just(Token::TypeSpecClose))
      .map(|expr| TypeSpec {
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


/* pub enum GlobalPlaceExpression<'a> {} */

/* #[derive(Debug, Clone)] */
/* pub enum TopLevelStatement<'a> { */
/* GlobalAssignment(Expression<'a>, Expression<'a>, Direction, Level), */
/* SimpleExpression(Expression<'a>), */
/* NamespaceExpansion, */
/* ImportHighlight, */
/* } */


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
}
