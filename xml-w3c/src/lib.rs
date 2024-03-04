mod parse;
pub use parse::*;

#[derive(Clone, Debug, Default)]
pub struct TokenDefinition {
    tokens: std::collections::HashMap<String, ()>,
}

impl TokenDefinition {
    pub fn new() -> Self {
        Self {
            tokens: std::collections::HashMap::new(),
        }
    }
    pub fn add_token(&mut self, token_name: impl ToString, token_description: ()) -> &mut Self {
        self.tokens
            .insert(token_name.to_string(), token_description);
        self
    }
}

impl TokenDefinition {
    fn token_names(&self) -> impl Iterator<Item = &'_ str> {
        self.tokens.keys().map(|s| s.as_str())
    }
}
