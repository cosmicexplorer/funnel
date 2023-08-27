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
//!
//! # Tokens
//! 1. global dereference: `\$[a-zA-Z][a-zA-Z0-9_-]*`
//! 2. local dereference: `\.[a-zA-Z_-]?[a-zA-Z0-9_-]*`
//! 3. local lambda arg: `\\\.[a-zA-Z_-]?[a-zA-Z0-9_-]*`
//! 4. namespace dereference: `:[a-zA-Z][a-zA-Z0-9_-]*`
//! 5. global assignment: `-<-|=<=|->-|=>=`
//! 6. assertion: `-!-|=!=`
//! 7. application: `<-|<=|=>|->`
//! 8. case literal dereference: `\+[a-z]+`
//! 9. case declaration: `\\\+[a-z]+`
//! 10. case value assertion: `\+!([a-z]+)?`
//! 11. grouping: `(|)|[|]`
//! 12. arg separator: `/|,`
//! 13. serial statement separator: `;`
//! 14. numeric literal: `[0-9]+`
//! 15. string literal: `"([^"]|\\")*"`
//! 16. type spec operator: `[\(|\)]`
//! 17. implicit arrow operator: `<~|~>`
//! 18. abbreviated implicit operator: `~`
//! 19. namespace editing: `{|}`
//! 20. import highlight: `\$\$[a-zA-Z][a-zA-Z0-9_-]*`

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
/* Arc<Mutex> can be more clear than needing to grok Orderings. */
#![allow(clippy::mutex_atomic)]

use chumsky::{
  input::{BorrowInput, Stream, ValueInput},
  prelude::*,
};


#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum Direction {
  Left,
  Right,
}

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
  CaseDeclaration(&'a str),
  CaseValueAssertion(Option<&'a str>),
  GroupStart(Level),
  GroupClose(Level),
  ArgSeparator,
  SerialSeparator,
  NumericLiteral(&'a str),
  StringLiteral(&'a str),
  TypeSpecStart,
  TypeSpecClose,
  ImplicitArrow(Direction),
  AbbreviatedImplicitLink,
  NamespaceStart,
  NamespaceClose,
  ImportHighlight(&'a str),
}

fn global_symbol<'a>() -> impl Parser<'a, &'a str, &'a str> { regex("[a-zA-Z][a-zA-Z0-9_-]*") }

fn local_symbol<'a>() -> impl Parser<'a, &'a str, &'a str> { regex("[a-zA-Z_-][a-zA-Z0-9_-]*") }

fn case_symbol<'a>() -> impl Parser<'a, &'a str, &'a str> { regex("[a-z]+") }


fn tokenize<'a>() -> impl Parser<'a, &'a str, Token<'a>> {
  /* global_symbol().map(Token::GlobalDereference) */
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
      .ignore_then(case_symbol())
      .map(Token::CaseDeclaration),
    just("+!")
      .ignore_then(case_symbol().or_not())
      .map(Token::CaseValueAssertion),
    choice((
      just('(').to(Token::GroupStart(Level::Value)),
      just(')').to(Token::GroupClose(Level::Value)),
      just('[').to(Token::GroupStart(Level::r#Type)),
      just(']').to(Token::GroupClose(Level::r#Type)),
    )),
    choice((just('/'), just(','))).to(Token::ArgSeparator),
    just(';').to(Token::SerialSeparator),
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
  ))
}


#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub enum EvalDirection {
  Dereference,
  DeclareLambdaArg,
}


#[derive(Debug, Clone)]
pub enum Name<'a> {
  Global(&'a str, Vec<&'a str>),
  Local(&'a str, EvalDirection),
}


fn name_values<'a, I>() -> impl Parser<'a, I, Name<'a>>
where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan>+BorrowInput<'a> {
  let local_names = select_ref! {
    Token::LocalDereference(name) => Name::Local(name, EvalDirection::Dereference),
    Token::LocalLambdaArg(name) => Name::Local(name, EvalDirection::DeclareLambdaArg),
  };
  let global_name = select_ref! {
    Token::GlobalDereference(name) => name,
  };
  let namespace_dereference = select_ref! {
    Token::NamespaceDereference(name) => *name,
  };
  choice((
    local_names,
    global_name
      .then(namespace_dereference.repeated().collect::<Vec<_>>())
      .map(|(global_name, namespaces)| Name::Global(global_name, namespaces)),
  ))
}


#[derive(Debug, Clone)]
pub enum Assertion<'a> {
  Value(Box<Expression<'a>>, Box<Expression<'a>>),
  r#Type(Box<Expression<'a>>, Box<Expression<'a>>),
}


/* fn assertions<'a, I>() -> impl Parser<'a, I, Assertion<'a>> */
/* where I: ValueInput<'a, Token=Token<'a>, Span=SimpleSpan> { */
/* let */
/* } */


#[derive(Debug, Clone)]
pub enum Expression<'a> {
  Name(Name<'a>),
  Assertion(Assertion<'a>),
}


#[derive(Debug, Clone)]
pub enum TopLevelStatement {
  GlobalAssignment,
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
}
