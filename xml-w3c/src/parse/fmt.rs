use super::*;
use std::fmt::Display;

impl Display for Rule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn print_char_class<'a>(
            f: &mut std::fmt::Formatter,
            iter: impl IntoIterator<Item = &'a CharClass>,
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

        match self {
            Self::Ref { ref_name } => write!(f, "{ref_name}")?,
            Self::String { val } => write!(
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
            Self::Choice { rules } => {
                if rules.len() > 1 {
                    write!(f, "(")?;
                }
                let mut iter = rules.iter();
                if let Some(r) = iter.next() {
                    write!(f, "{r}")?;
                }
                for r in iter {
                    write!(f, " | {r}")?;
                }
                if rules.len() > 1 {
                    write!(f, ")")?;
                }
                write!(f, "")?;
            }
            Self::Sequence { rules } => {
                if rules.len() > 1 {
                    write!(f, "(")?;
                }
                let mut iter = rules.iter();
                if let Some(r) = iter.next() {
                    write!(f, "{r}")?;
                }
                for r in iter {
                    write!(f, " {r}")?;
                }
                if rules.len() > 1 {
                    write!(f, ")")?;
                }
            }
            Self::OneOrMore { rule } => {
                write!(f, "{rule}+")?;
            }
            Self::ZeroOrMore { rule } => {
                write!(f, "{rule}*")?;
            }
            Self::Optional { rule } => {
                write!(f, "{rule}?")?;
            }
            Self::CharClass { classes } => {
                write!(f, "[")?;
                print_char_class(f, classes)?;
                write!(f, "]")?;
            }
            Self::Complement { classes } => {
                write!(f, "[^")?;
                print_char_class(f, classes)?;
                write!(f, "]")?;
            }
        }
        Ok(())
    }
}

impl Display for Production {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ::= ", self.name)?;
        for r in &self.rules {
            write!(f, "{} ", r)?;
        }
        Ok(())
    }
}

impl Display for Grammar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for r in self.rules.values() {
            writeln!(f, "{}", r)?;
        }
        Ok(())
    }
}
