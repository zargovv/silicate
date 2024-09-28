extern crate proc_macro;

use std::{
  iter::{self, Peekable},
  ops::Deref,
  slice,
};

use proc_macro::{token_stream, Delimiter, Literal, TokenStream, TokenTree};

pub struct Attr(TokenStream);

impl Deref for Attr {
  type Target = TokenStream;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl IntoIterator for Attr {
  type Item = TokenTree;

  type IntoIter = token_stream::IntoIter;

  fn into_iter(self) -> Self::IntoIter {
    self.0.into_iter()
  }
}

impl<T: Iterator<Item = TokenTree>> From<&mut Peekable<T>> for Attr {
  fn from(value: &mut Peekable<T>) -> Self {
    let mut attr = TokenStream::new();
    while matches!(value.peek(), Some(TokenTree::Punct(p)) if p.as_char() == '#') {
      let Some(token) = value.next() else {
        unreachable!();
      };
      attr.extend(iter::once(token));

      let Some(TokenTree::Group(g)) = value.next() else {
        unreachable!("Attr group expected");
      };
      assert_eq!(g.delimiter(), Delimiter::Bracket);
      attr.extend(iter::once(TokenTree::Group(g)));
    }
    Self(attr)
  }
}

pub struct Vis(TokenStream);

impl Deref for Vis {
  type Target = TokenStream;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}

impl IntoIterator for Vis {
  type Item = TokenTree;

  type IntoIter = token_stream::IntoIter;

  fn into_iter(self) -> Self::IntoIter {
    self.0.into_iter()
  }
}

impl<T: Iterator<Item = TokenTree>> From<&mut Peekable<T>> for Vis {
  fn from(value: &mut Peekable<T>) -> Self {
    let mut vis = TokenStream::new();
    if matches!(value.peek(), Some(TokenTree::Ident(id)) if id.to_string() == "pub") {
      let Some(token) = value.next() else {
        unreachable!();
      };
      vis.extend(iter::once(token));
      if let Some(TokenTree::Group(g)) = value.peek() {
        assert_eq!(g.delimiter(), Delimiter::Parenthesis);
        let Some(t) = value.next() else {
          unreachable!();
        };
        vis.extend(iter::once(t));
      }
    }
    Self(vis)
  }
}

pub struct Field {
  pub attr: Attr,
  pub vis: Vis,
  pub accessor: TokenTree,
  pub ty: TokenStream,
}

impl Field {
  fn from_iter(
    stream_iter: &mut Peekable<impl Iterator<Item = TokenTree>>,
    tuple: bool,
  ) -> Vec<Self> {
    let mut fields = Vec::new();
    let mut i = 0;
    while stream_iter.peek().is_some() {
      let attr = Attr::from(&mut *stream_iter);
      let vis = Vis::from(&mut *stream_iter);

      let accessor = if tuple {
        let lit = TokenTree::Literal(Literal::i32_unsuffixed(i));
        i += 1;
        lit
      } else {
        let Some(TokenTree::Ident(ident)) = stream_iter.next() else {
          unreachable!("Struct field ident expected");
        };

        let Some(TokenTree::Punct(token)) = stream_iter.next() else {
          unreachable!("`:` expected: {ident}");
        };
        assert_eq!(token.as_char(), ':');

        TokenTree::Ident(ident)
      };

      let mut ty = TokenStream::new();
      for token in stream_iter.by_ref() {
        match token {
          TokenTree::Punct(p) if p.as_char() == ',' => break,
          token => ty.extend(iter::once(token)),
        }
      }

      fields.push(Self {
        attr,
        vis,
        accessor,
        ty,
      });
    }
    fields
  }
}

pub enum FieldIter<'a> {
  None,
  Tuple(slice::Iter<'a, Field>),
  Block(slice::Iter<'a, Field>),
}

impl<'a> Iterator for FieldIter<'a> {
  type Item = &'a Field;

  fn next(&mut self) -> Option<Self::Item> {
    match self {
      Self::None => None,
      Self::Tuple(iter) | Self::Block(iter) => iter.next(),
    }
  }
}

pub enum FieldDescriptor {
  None,
  Tuple(Vec<Field>),
  Block(Vec<Field>),
}

impl<'a> IntoIterator for &'a FieldDescriptor {
  type Item = &'a Field;

  type IntoIter = FieldIter<'a>;

  fn into_iter(self) -> Self::IntoIter {
    self.iter()
  }
}

impl FieldDescriptor {
  #[must_use]
  pub fn iter(&self) -> FieldIter {
    match self {
      Self::None => FieldIter::None,
      Self::Tuple(fields) => FieldIter::Tuple(fields.iter()),
      Self::Block(fields) => FieldIter::Block(fields.iter()),
    }
  }
}

impl From<Option<TokenTree>> for FieldDescriptor {
  fn from(value: Option<TokenTree>) -> Self {
    let Some(token) = value else {
      return Self::None;
    };

    let TokenTree::Group(group) = token else {
      unreachable!("Definition body expected");
    };

    let tuple = match group.delimiter() {
      Delimiter::Brace => false,
      Delimiter::Parenthesis => true,
      _ => unreachable!("Definition body expected"),
    };

    let fields = Field::from_iter(&mut group.stream().into_iter().peekable(), tuple);
    if tuple {
      Self::Tuple(fields)
    } else {
      Self::Block(fields)
    }
  }
}

pub struct Struct {
  pub attr: Attr,
  pub vis: Vis,
  pub ident: TokenTree,
  pub fields: FieldDescriptor,
}

impl From<TokenStream> for Struct {
  fn from(value: TokenStream) -> Self {
    let mut stream_iter = value.into_iter().peekable();

    let attr = Attr::from(&mut stream_iter);
    let vis = Vis::from(&mut stream_iter);

    let Some(TokenTree::Ident(token)) = stream_iter.next() else {
      unreachable!("Keyword `struct` expected");
    };
    assert_eq!(token.to_string(), "struct");

    let Some(TokenTree::Ident(ident)) = stream_iter.next() else {
      unreachable!("Struct ident expected");
    };

    let fields = FieldDescriptor::from(stream_iter.next());

    Self {
      attr,
      vis,
      ident: TokenTree::Ident(ident),
      fields,
    }
  }
}

pub struct Variant {
  pub attr: Attr,
  pub ident: TokenTree,
  pub fields: FieldDescriptor,
}

pub struct Enum {
  pub attr: Attr,
  pub vis: Vis,
  pub ident: TokenTree,
  pub variants: Vec<Variant>,
}

impl From<TokenStream> for Enum {
  fn from(value: TokenStream) -> Self {
    let mut stream_iter = value.into_iter().peekable();

    let attr = Attr::from(&mut stream_iter);
    let vis = Vis::from(&mut stream_iter);

    let Some(TokenTree::Ident(token)) = stream_iter.next() else {
      unreachable!("Keyword `enum` expected");
    };
    assert_eq!(token.to_string(), "enum");

    let Some(TokenTree::Ident(ident)) = stream_iter.next() else {
      unreachable!("Enum ident expected");
    };
    let Some(TokenTree::Group(group)) = stream_iter.next() else {
      unreachable!("Enum body expected");
    };
    assert_eq!(group.delimiter(), Delimiter::Brace);

    let mut stream_iter = group.stream().into_iter().peekable();

    let mut variants = Vec::new();
    while stream_iter.peek().is_some() {
      let attr = Attr::from(&mut stream_iter);

      let Some(TokenTree::Ident(ident)) = stream_iter.next() else {
        unreachable!("Enum variant ident expected");
      };
      let fields = FieldDescriptor::from(stream_iter.next());

      if matches!(stream_iter.peek(), Some(TokenTree::Punct(p)) if p.as_char() == ',') {
        stream_iter.next();
      }

      variants.push(Variant {
        attr,
        ident: TokenTree::Ident(ident),
        fields,
      });
    }

    Self {
      attr,
      vis,
      ident: TokenTree::Ident(ident),
      variants,
    }
  }
}
