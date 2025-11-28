use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::AngleBracketedGenericArguments;
use syn::AttrStyle;
use syn::Attribute;
use syn::DeriveInput;
use syn::Error;
use syn::GenericArgument;
use syn::Ident;
use syn::Lit;
use syn::Token;
use syn::Type;
use syn::TypePath;
use syn::parse_macro_input;
use syn::spanned::Spanned;

#[derive(Debug, Clone)]
struct Field {
    name: Ident,
    ty: syn::Type,
    optional: bool,
    repeated: bool,
    repeated_name: Option<String>,
}

impl Field {
    fn from_raw(f: &syn::Field) -> syn::Result<Self> {
        let name = f.ident.clone();
        if name.is_none() {
            return syn::Result::Err(Error::new(name.span(), "Invalid field name."));
        }
        let name = name.unwrap();
        let ty = &f.ty;
        let optional = is_optional(ty);
        let attr: Vec<syn::Result<String>> = f.attrs.iter().map(get_builder_attr).collect();
        let (oks, errs): (Vec<_>, Vec<_>) = attr.iter().partition(|a| a.is_ok());
        if !errs.is_empty()
            && let Some(Err(e)) = errs.first()
        {
            return syn::Result::Err(e.clone());
        }
        let oks: Vec<&String> = oks.iter().filter_map(|o| o.as_ref().ok()).collect();
        let repeated = !oks.is_empty() && is_repeated(ty);
        let repeated_name = oks.first().cloned();
        Ok(Field {
            name,
            ty: ty.clone(),
            optional,
            repeated,
            repeated_name: repeated_name.cloned(),
        })
    }

    fn setter(&self) -> proc_macro2::TokenStream {
        let name_ident = self.name.clone();
        let ty = self.ty.clone();
        if self.optional {
            let unwrapped_type = unwrap_option(&ty);
            quote! {
                 fn #name_ident(&mut self, arg: #unwrapped_type) -> &mut Self{
                     self.#name_ident = Some(arg);
                     self
                 }
            }
        } else if self.repeated {
            let fn_name = &self
                .repeated_name
                .clone()
                .expect("No valid function name provided.");
            let fn_ident = Ident::new(fn_name, Span::call_site());
            let ty = unwrap_vec(&ty);
            quote! {
                fn #fn_ident(&mut self, arg: #ty) -> &mut Self{
                    self.#name_ident.push(arg);
                    self
                }
            }
        } else {
            quote! {
                 fn #name_ident(&mut self, arg: #ty) -> &mut Self{
                     self.#name_ident = Some(arg);
                     self
                 }
            }
        }
    }

    fn builder(&self) -> proc_macro2::TokenStream {
        let name_ident = self.name.clone();
        let ty = self.ty.clone();

        if self.optional || self.repeated {
            quote! {
                #name_ident: #ty,
            }
        } else {
            quote! {
                #name_ident: Option<#ty>,
            }
        }
    }

    fn init(&self) -> proc_macro2::TokenStream {
        let name_ident = self.name.clone();
        if self.repeated {
            quote! {
                #name_ident: vec![],
            }
        } else {
            quote! {
                #name_ident : None,
            }
        }
    }

    fn finaliser(&self) -> proc_macro2::TokenStream {
        let name_ident = self.name.clone();
        let error_message = format!("missing parameter: {}", name_ident);
        if self.optional || self.repeated {
            quote! {
                #name_ident: self.#name_ident.clone(),
            }
        } else {
            quote! {
                #name_ident: self.#name_ident.clone().ok_or(#error_message)?,
            }
        }
    }
}

#[derive(Debug, Clone)]
struct StructRepr {
    name: Ident,
    fields: Vec<Field>,
}

impl TryFrom<DeriveInput> for StructRepr {
    type Error = syn::Error;

    fn try_from(value: DeriveInput) -> std::result::Result<Self, Self::Error> {
        let name = value.ident.clone();
        match value.data.clone() {
            syn::Data::Struct(data_struct) => {
                let fields: Vec<syn::Result<Field>> = data_struct
                    .fields
                    .into_iter()
                    .map(|f| Field::from_raw(&f))
                    .collect();
                let errs: Vec<syn::Error> = fields
                    .clone()
                    .into_iter()
                    .filter(|e| e.is_err())
                    .filter_map(|e| e.err())
                    .collect();
                if !errs.is_empty()
                    && let Some(e) = errs.first()
                {
                    return std::result::Result::Err(e.clone());
                }
                let fields: Vec<Field> = fields.into_iter().filter_map(|f| f.ok()).collect();
                Ok(Self { name, fields })
            }
            _ => unimplemented!(),
        }
    }
}

fn is_optional(ty: &Type) -> bool {
    has_outer_type("Option", ty)
}

fn is_repeated(ty: &Type) -> bool {
    has_outer_type("Vec", ty)
}

#[derive(Debug, Clone)]
struct BuilderAttrs {
    each: Ident,
    _equals: Token![=],
    name: Lit,
}

impl syn::parse::Parse for BuilderAttrs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self {
            each: input.parse()?,
            _equals: input.parse()?,
            name: input.parse()?,
        })
    }
}

fn get_builder_attr(a: &Attribute) -> syn::Result<String> {
    if a.style != AttrStyle::Outer {
        return Err(Error::new(a.span(), ""));
    }
    match &a.meta {
        syn::Meta::List(meta_list)
            if !meta_list.path.segments.is_empty() && meta_list.path.is_ident("builder") =>
        {
            let ba: syn::Result<BuilderAttrs> = meta_list.parse_args();
            match ba {
                Ok(ba) => {
                    if ba.each != "each" || !matches!(ba.name, Lit::Str(_)) {
                        return Err(syn::Error::new(
                            meta_list.span(),
                            "expected `builder(each = \"...\")`",
                        ));
                    }
                    if let Lit::Str(s) = ba.name {
                        return Ok(s.value());
                    }
                }
                Err(e) => {
                    return Err(e);
                }
            }
            Err(Error::new(a.span(), ""))
        }
        _ => Err(Error::new(a.span(), "")),
    }
}

fn has_outer_type(outer: &str, ty: &Type) -> bool {
    matches!(ty, syn::Type::Path(TypePath { qself: _, path }) if !path.segments.is_empty() && path.segments[0].ident == outer)
}

fn unwrap_option(ty: &Type) -> Option<Type> {
    if !is_optional(ty) {
        return None;
    }

    match ty {
        Type::Path(TypePath { qself: _, path }) => {
            if path.segments.is_empty() || path.segments[0].ident != "Option" {
                return None;
            }
            let segment = path.segments[0].clone();
            if let syn::PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                args,
                colon2_token: _,
                lt_token: _,
                gt_token: _,
            }) = segment.arguments
            {
                if args.is_empty() {
                    return None;
                }
                if let Some(GenericArgument::Type(ty)) = args.first() {
                    return Some(ty.clone());
                }
                None
            } else {
                None
            }
        }
        _ => None,
    }
}

fn unwrap_vec(ty: &Type) -> Option<Type> {
    if !is_repeated(ty) {
        return None;
    }

    match ty {
        Type::Path(TypePath { qself: _, path }) => {
            if path.segments.is_empty() || path.segments[0].ident != "Vec" {
                return None;
            }
            let segment = path.segments[0].clone();
            if let syn::PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                args,
                colon2_token: _,
                lt_token: _,
                gt_token: _,
            }) = segment.arguments
            {
                if args.is_empty() {
                    return None;
                }
                if let Some(GenericArgument::Type(ty)) = args.first() {
                    return Some(ty.clone());
                }
                None
            } else {
                None
            }
        }
        _ => None,
    }
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let structrep = StructRepr::try_from(input);
    if let std::result::Result::Err(e) = structrep {
        return e.into_compile_error().into();
    }
    let structrep = structrep.clone().unwrap();
    let struct_ident = structrep.name;
    let builder_type_name = format! {"{struct_ident}Builder"};
    let builder_ident = Ident::new(&builder_type_name, Span::call_site());

    let builder_fields: Vec<proc_macro2::TokenStream> =
        structrep.fields.iter().map(|f| f.builder()).collect();

    let setters: Vec<proc_macro2::TokenStream> =
        structrep.fields.iter().map(|f| f.setter()).collect();

    let initialisers: Vec<proc_macro2::TokenStream> =
        structrep.fields.iter().map(|f| f.init()).collect();

    let finalisers: Vec<proc_macro2::TokenStream> =
        structrep.fields.iter().map(|f| f.finaliser()).collect();

    quote! {
        pub struct #builder_ident{
            #(#builder_fields)*
        }

        impl #builder_ident{
            #(#setters)*

            fn build(&mut self) -> Result<#struct_ident, Box<dyn std::error::Error>>{
                Ok(#struct_ident{
                    #(#finalisers)*
                })
            }
        }

        impl #struct_ident{
            pub fn builder() -> #builder_ident{
                #builder_ident{
                    #(#initialisers)*
                }
            }
        }
    }
    .into()
}
