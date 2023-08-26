use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{
  parse::{Parse, ParseStream, Result},
  parse_macro_input, Ident, Token, Type, ItemStruct,
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
    let to_name = if from_name.to_string().ends_with('s') {
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

struct LockInput {
  mut_: Option<Token![mut]>,
  var: Ident,
}

impl Parse for LockInput {
  fn parse(input: ParseStream) -> Result<Self> {
    let lookahead = input.lookahead1();

    let mut_ = if input.is_empty() {
      return Err(lookahead.error());
    } else if lookahead.peek(Token![mut]) {
      Some(input.parse()?)
    } else {
      None
    };

    let var = input.parse()?;

    Ok(Self { mut_, var })
  }
}

#[proc_macro]
pub fn lock(input: TokenStream) -> TokenStream {
  let LockInput { mut_, var } = parse_macro_input!(input as LockInput);

  let output = quote! {
    let #mut_ #var = self.#var.lock().map_err(|e| anyhow!(e.to_string()))?;
  };

  output.into()
}

#[proc_macro_attribute]
pub fn args_converter(_: TokenStream, input: TokenStream) -> TokenStream {
  let old_input: proc_macro2::TokenStream = input.clone().into();
  let input = parse_macro_input!(input as ItemStruct);

  let str_name = input.ident;
  let str_gen_args = input.generics.params.into_iter().map(|p| {
    let t = match p {
      syn::GenericParam::Lifetime(_) => todo!(),
      syn::GenericParam::Type(t) => t,
      syn::GenericParam::Const(_) => todo!(),
    };
    t.ident
  }).collect::<Vec<_>>();

  let str_gen_args_1 = str_gen_args.iter().map(|i| 
    Ident::new(format!("{}1", i.to_string()).as_str(), i.span())
  ).collect::<Vec<_>>();
  let str_gen_args_2 = str_gen_args.iter().map(|i| 
    Ident::new(format!("{}", i.to_string()).as_str(), i.span())
  ).collect::<Vec<_>>();

  let syn::Fields::Named(args) = input.fields else {
    todo!()
  };

  let args = args.named.into_iter().collect::<Vec<_>>();

  let (_generic_args, _non_generic_args): (Vec<_>, Vec<_>) = args.into_iter().partition(|f| {
    let p = match &f.ty {
      Type::Path(p) => p,
      _ => todo!(),
    };
    p.path.segments.iter().any(|s| {
      str_gen_args_2.contains(&s.ident)
      || match s.arguments {
        syn::PathArguments::AngleBracketed(_) => true,
        _ => false,
      }
    })
  });

  //panic!(
  //  "

  //  ga {_generic_args:?}
  //  nga {_non_generic_args:?}

  //  "
  //);

  let non_generic_args = _non_generic_args.into_iter().map(|f| f.ident).collect::<Vec<_>>();
  let generic_args = _generic_args.clone().into_iter().map(|f| f.ident).collect::<Vec<_>>();
  let generic_arg_types = _generic_args.into_iter().map(|f| f.ty).collect::<Vec<_>>();
  
  let ga1 = quote!{
    <#(#str_gen_args_1),*>
  };
  let ga2 = quote!{
    <#(#str_gen_args_2),*>
  };

  let unpack = if non_generic_args.is_empty() {
    quote!{}
  } else {
    quote!{
      let #str_name {
        #(
          #non_generic_args
        ),*
        ,
        ..
      } = self;
    }
  };

  let comma = if non_generic_args.is_empty() {
    quote! {}
  } else {
    quote! { , }
  };

  let output = quote!{
    #old_input

    impl #ga1 #str_name #ga1 {
      pub fn conv #ga2 (
        self, 
        #(
          #generic_args: #generic_arg_types
        ),*
      ) -> anyhow::Result<#str_name #ga2> {
        Ok(self.uconv(
          #(
            #generic_args
          ),*
        ))
      }

      pub fn uconv #ga2 (
        self, 
        #(
          #generic_args: #generic_arg_types
        ),*
      ) -> #str_name #ga2 {
        #unpack
        #str_name {
          #(
            #non_generic_args
          ),*
          #comma
          #(
            #generic_args
          ),*
        }
      }
    }
  };

  //panic!("{}", output.to_string());
  
  output.into()
}