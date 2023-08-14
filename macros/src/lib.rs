use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{
  parse::{Parse, ParseStream, Result},
  parse_macro_input, Ident, Token, Type,
};

struct DefineMap {
  from_name: Ident,
  arg_t: Type,
  rt: Option<Type>,
}

impl Parse for DefineMap {
  fn parse(input: ParseStream) -> Result<Self> {
    let from_name = input.parse()?;
    input.parse::<Token![,]>()?;
    let arg_t = input.parse()?;

    let rt = {
      let lookahead = input.lookahead1();

      if input.is_empty() {
        None
      } else if lookahead.peek(Token![,]) {
        input.parse::<Token![,]>()?;
        Some(input.parse()?)
      } else {
        return Err(lookahead.error());
      }
    };

    Ok(Self {
      from_name,
      arg_t,
      rt,
    })
  }
}
struct DefineMapMacroArgs {
  from_name: Ident,
  to_name: Ident,
  arg_t: Type,
  rt: Type,
}

impl From<DefineMap> for DefineMapMacroArgs {
  fn from(value: DefineMap) -> Self {
    let from_name = value.from_name;
    let to_name = if from_name.to_string().ends_with("s") {
      format_ident!("{}es", from_name)
    } else {
      format_ident!("{}s", from_name)
    };
    let arg_t = value.arg_t;
    let rt = match value.rt {
      Some(rt) => rt,
      None => arg_t.clone(),
    };
    Self {
      from_name,
      to_name,
      arg_t,
      rt,
    }
  }
}

#[proc_macro]
pub fn define_map(input: TokenStream) -> TokenStream {
  let input = parse_macro_input!(input as DefineMap);
  let DefineMapMacroArgs {
    from_name,
    to_name,
    arg_t,
    rt,
  } = input.into();

  let output = quote! {
    fn #to_name(
      &self,
      es: Vec<#arg_t>,
    ) -> Vec<#rt> {
      es.into_iter().map(|e| self.#from_name(e)).collect()
    }
  };

  output.into()
}

#[proc_macro]
pub fn define_map_result(input: TokenStream) -> TokenStream {
  let input = parse_macro_input!(input as DefineMap);
  let DefineMapMacroArgs {
    from_name,
    to_name,
    arg_t,
    rt,
  } = input.into();

  let output = quote! {
    fn #to_name(
      &self,
      es: Vec<#arg_t>,
    ) -> Result<Vec<#rt>> {
      es.into_iter().map(|e| self.#from_name(e)).collect()
    }
  };

  output.into()
}