use super::*;
use std::fmt::Display;

pub struct EbnfDisplay<'a, T>(pub &'a T);
use EbnfDisplay as ED;

impl<'a> Display for ED<'a, Rule> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn print_char_class<'b>(
            f: &mut std::fmt::Formatter,
            iter: impl IntoIterator<Item = &'b CharClass>,
        ) -> std::fmt::Result {
            for r in iter {
                match r {
                    CharClass::Char { chr } => write!(f, "{chr}")?,
                    CharClass::CharRange { range } => {
                        write!(f, "{}-{}", range.start(), range.end())?
                    }
                }
            }
            Ok(())
        }

        match self.0 {
            Rule::Ref { ref_name } => write!(f, "{ref_name}")?,
            Rule::String { val } => write!(
                f,
                "{quote}{val}{quote}",
                quote = {
                    let single = val.contains('\'');
                    let double = val.contains('\"');
                    match (single, double) {
                        (false, true | false) => '\'',
                        (true, false) => '"',
                        (true, true) => return Err(std::fmt::Error),
                    }
                }
            )?,
            Rule::Choice { rules } => {
                if rules.len() > 1 {
                    write!(f, "(")?;
                }
                let mut iter = rules.iter();
                if let Some(r) = iter.next() {
                    write!(f, "{}", ED(r))?;
                }
                for r in iter {
                    write!(f, " | {}", ED(r))?;
                }
                if rules.len() > 1 {
                    write!(f, ")")?;
                }
                write!(f, "")?;
            }
            Rule::Sequence { rules } => {
                if rules.len() > 1 {
                    write!(f, "(")?;
                }
                let mut iter = rules.iter();
                if let Some(r) = iter.next() {
                    write!(f, "{}", ED(r))?;
                }
                for r in iter {
                    write!(f, " {}", ED(r))?;
                }
                if rules.len() > 1 {
                    write!(f, ")")?;
                }
            }
            Rule::Repeat { kind, rule } => {
                write!(
                    f,
                    "{}{}",
                    ED(&**rule),
                    match *kind {
                        RepeatKind::OneOrMore => "+",
                        RepeatKind::ZeroOrMore => "*",
                        RepeatKind::ZeroOrOnce => "?",
                    }
                )?;
            }
            Rule::CharClass { inverse, classes } => {
                write!(f, "[{}", if *inverse { "^" } else { "" })?;
                print_char_class(f, classes)?;
                write!(f, "]")?;
            }
        }
        Ok(())
    }
}

impl<'a> Display for ED<'a, Production> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{dif}::={dif}",
            self.0.name,
            dif = if f.alternate() { '\t' } else { ' ' }
        )?;
        for r in &self.0.rules {
            write!(f, "{}' '", ED(r))?;
        }
        Ok(())
    }
}

impl<'a> Display for ED<'a, Grammar> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.0.rules.values();
        let mut print = |nl, r| {
            if f.alternate() {
                write!(f, "{nl}{:#}", ED(r))
            } else {
                write!(f, "{nl}{:}", ED(r))
            }
        };
        if let Some(r) = iter.next() {
            print("", r)?;
        }
        for r in iter {
            print("\n", r)?;
        }
        Ok(())
    }
}
