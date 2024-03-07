mod parse;
pub use parse::*;

#[derive(Clone, Debug, Default, serde::Deserialize, serde::Serialize)]
enum TokenKind {
    BuiltIn,
    StringLiteral(String),
    #[default]
    UserDefined,
}

#[derive(Clone, Debug, Default, serde::Deserialize, serde::Serialize)]
struct Token {
    kind: TokenKind,
}

#[derive(Clone, Debug, Default, serde::Deserialize, serde::Serialize)]
pub struct TokenDefinition {
    pub(crate) tokens: std::collections::HashMap<String, Token>,
}

impl TokenDefinition {
    pub fn new() -> Self {
        Self {
            tokens: std::collections::HashMap::new(),
        }
    }

    pub fn add_token(&mut self, token_name: impl ToString, token_description: ()) -> &mut Self {
        let _ = token_description;
        self.tokens.insert(
            token_name.to_string(),
            Token {
                kind: TokenKind::UserDefined,
            },
        );
        self
    }

    fn token_names(&self) -> impl Iterator<Item = &'_ str> {
        self.tokens.keys().map(|s| s.as_str())
    }
}
