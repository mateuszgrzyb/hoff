use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
  pub static ref STRING_TEMPLATE_RE: Regex =
    Regex::new(r"\{\{|\}\}|\{([^}]+)\}").unwrap();
}
