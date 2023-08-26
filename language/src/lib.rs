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
//! 11. value grouping: `(|)`
//! 12. type grouping: `[|]`
//! 13. arg separator: `/|,`
//! 14. serial statement separator: `;`
//! 15. numeric literal: `[0-9]+`
//! 16. string literal: `"([^"]|\\")*"`
//! 17. type spec operator: `[\(|\)]`
//! 18. implicit arrow operator: `<~|~>`
//! 19. abbreviated implicit operator: `~`
//! 20. namespace editing: `{|}`
//! 21. import highlight: `\$\$[a-zA-Z][a-zA-Z0-9_-]*`

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

use chumsky::prelude::*;


#[derive(Clone, Debug, Eq, PartialEq)]
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
