use super::{Token, specification::SPECIFICATIONS};

pub struct Tokenizer {
  content: String,
  row: usize,
  column: usize,
}

impl Tokenizer {
  pub fn new(content: String) -> Tokenizer {
    Tokenizer {
      content: content,
      row: 0,
      column: 0,
    }
  }

  fn get_next_token_from_spec(&mut self, end: usize) -> Option<Token> {
    let text = match end {
      0 => &self.content[self.column..],
      _ => &self.content[self.column..end],
    };
    for spec in SPECIFICATIONS.as_ref() {
      for capture in spec.regex.captures_iter(text) {
        let m = capture.get(0).expect("Match should exist");
        let start = m.start();
        if start != 0 { continue; }
  
        let value = m.as_str().to_string();
        let len = value.len();
  
        self.column += len;
  
        let token = (spec.create)(value);

        if let Token::Comment(_) = token {
          self.column -= len;
          let nl = self.content[self.column..].find("\n");
          match nl {
            None => {
              let string = self.content[self.column..].to_string();
              self.column += string.len();

              return Some(Token::Comment(string));
            },
            Some(end) => {
              let string = self.content[self.column..self.column + end].to_string();
              self.column += string.len();

              return Some(Token::Comment(string));
            },
          }
        }

        return match token {
          Token::Space => self.get_next_token(),
          _ => Some(token),
        }
      }
    }
  
    None
  }

  fn get_next_identifier_token_from_spec(&mut self, end: usize) -> Option<Token> {
    let text = match end {
      0 => &self.content[self.column..],
      _ => &self.content[self.column..end],
    };
    let mut smallest_start: Option<usize> = None;
    for spec in SPECIFICATIONS.as_ref() {
      for capture in spec.regex.captures_iter(text) {
        let m = capture.get(0).expect("Match should exist");
        let start = m.start();
        if start == 0 { unreachable!("Shoul not happen, as we already called a function to check all the tokens from the start!"); }

        if let None = smallest_start {
          smallest_start = Some(start);
          continue;
        }
        if let Some(value) = smallest_start {
          if start < value {
            smallest_start = Some(start);
            continue;
          }
        }
      }
    }

    return match smallest_start {
      None => None,
      Some(start) => {
        self.column += start;

        Some(Token::Identifier(text[0..start].to_string()))
      }
    };
  }

  pub fn get_next_token(&mut self) -> Option<Token> {
    if self.column >= self.content.len() - 1 {
      return None;
    }

    let end = match self.content[self.column..].find(" ") {
      Some(0) => 0,
      Some(end) => self.column + end,
      None => 0,
    };

    let mut token = self.get_next_token_from_spec(end);
    if let Some(_) = token {
      return token;
    }
    token = self.get_next_identifier_token_from_spec(end);
    if let Some(_) = token {
      return token;
    }

    let text = match end {
      0 => &self.content[self.column..],
      _ => &self.content[self.column..end],
    };
    self.column += text.len();
    return Some(Token::Identifier(text.to_string()));
  }
}
