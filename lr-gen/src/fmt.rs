#[cfg(feature = "table")]
mod table_display;

struct MAction<'a>(&'a crate::Action);
const ENTRY_POINT: &str = "⊤";

macro_rules! colors {
    {$($name:ident => $val:literal),*$(,)?} => {
        $(pub const $name: &str = concat!("\x1B[", stringify!($val),"m");
        #[allow(unused_macros)]
        macro_rules! $name {
        () => (concat!("\x1B[", stringify!($val),"m"))
        }
        )*
    };
}

colors! {
    RESET           => 0,
    RED             => 31,
    GREEN           => 32,
    YELLOW          => 33,
    BLUE            => 34,
    MAGENTA         => 35,
    CYAN            => 36,
    WHITE           => 37,
    BRIGHT_RED      => 91,
    BRIGHT_GREEN    => 92,
    BRIGHT_YELLOW   => 93,
    BRIGHT_BLUE     => 94,
    BRIGHT_MAGENTA  => 95,
    BRIGHT_CYAN     => 96,
    BRIGHT_WHITE    => 97,
}

impl<'a> std::fmt::Display for MAction<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            crate::Action::Other { ty, goto, mode } => match ty {
                0 => write!(f, "Goto {GREEN}{goto}{RESET} ({CYAN}{mode:b}{RESET})"),
                _ => write!(
                    f,
                    "Unknown ty={RED}{ty}{RESET}, goto={GREEN}{goto}{RESET} ({CYAN}{mode:b}{RESET})"
                ),
            },
            crate::Action::Reduce { name, len, goto } => {
                write!(
                    f,
                    "Reduce to '{MAGENTA}{}{RESET}' with {CYAN}{len}{RESET} tokens and goto {GREEN}{goto}{RESET}",
                    match name {
                        crate::RuleName::EntryPoint => ENTRY_POINT,
                        crate::RuleName::Named(r) => &r,
                    }
                )
            }
        }
    }
}

trait AsciiCharToStr {
    fn ascii_str(&self) -> Option<&str>;
}

macro_rules! lit {
    [$($s:literal),* $(,)?] => {
        [
            $(
                $s,
            )*
        ]
    };
}

impl AsciiCharToStr for char {
    fn ascii_str(&self) -> Option<&str> {
        const STRS: [&str; 128] = lit![
            "'\\0'", "'\\1'", "'\\2'", "'\\3'", "'\\4'", "'\\5'", "'\\6'", "'\\a'", "'\\b'",
            "'\\t'", "'\\n'", "'\\v'", "'\\f'", "'\\r'", "'\\E'", "'\\F'", "'\\10'", "'\\11'",
            "'\\12'", "'\\13'", "'\\14'", "'\\15'", "'\\16'", "'\\17'", "'\\18'", "'\\19'",
            "'\\1A'", "'\\1B'", "'\\1C'", "'\\1D'", "'\\1E'", "'\\1F'", "'\x20'", "'\x21'",
            "'\x22'", "'\x23'", "'\x24'", "'\x25'", "'\x26'", "'\x27'", "'\x28'", "'\x29'",
            "'\x2A'", "'\x2B'", "'\x2C'", "'\x2D'", "'\x2E'", "'\x2F'", "'\x30'", "'\x31'",
            "'\x32'", "'\x33'", "'\x34'", "'\x35'", "'\x36'", "'\x37'", "'\x38'", "'\x39'",
            "'\x3A'", "'\x3B'", "'\x3C'", "'\x3D'", "'\x3E'", "'\x3F'", "'\x40'", "'\x41'",
            "'\x42'", "'\x43'", "'\x44'", "'\x45'", "'\x46'", "'\x47'", "'\x48'", "'\x49'",
            "'\x4A'", "'\x4B'", "'\x4C'", "'\x4D'", "'\x4E'", "'\x4F'", "'\x50'", "'\x51'",
            "'\x52'", "'\x53'", "'\x54'", "'\x55'", "'\x56'", "'\x57'", "'\x58'", "'\x59'",
            "'\x5A'", "'\x5B'", "'\x5C'", "'\x5D'", "'\x5E'", "'\x5F'", "'\x60'", "'\x61'",
            "'\x62'", "'\x63'", "'\x64'", "'\x65'", "'\x66'", "'\x67'", "'\x68'", "'\x69'",
            "'\x6A'", "'\x6B'", "'\x6C'", "'\x6D'", "'\x6E'", "'\x6F'", "'\x70'", "'\x71'",
            "'\x72'", "'\x73'", "'\x74'", "'\x75'", "'\x76'", "'\x77'", "'\x78'", "'\x79'",
            "'\x7A'", "'\x7B'", "'\x7C'", "'\x7D'", "'\x7E'", "'\\7F'",
        ];

        if self.is_ascii() {
            Some(self)
                .map(|c| unsafe { &*(c as *const char as *const [u8; 4]) })
                .map(|s| s[0])
                .map(|s| STRS[s as usize])
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TextDisplay<'a>(pub &'a crate::DecisionTable);

impl<'a> std::fmt::Display for TextDisplay<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        display_as_text(self.0, f)
    }
}

fn display_as_text(
    table: &crate::DecisionTable,
    fmt: &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result {
    let max_len = {
        let val = table
            .iter()
            .map(|s| {
                s.iter()
                    .map(|(lhs, _rhs)| {
                        match lhs {
                            crate::Token::Terminal(c) => c.ascii_str().expect("Char isn't ascii"),
                            crate::Token::NonTerminal(r) if &**r == "__entry_point__" => {
                                ENTRY_POINT
                            }
                            crate::Token::NonTerminal(r) => &r,
                        }
                        .len()
                    })
                    .max()
                    .unwrap_or(0)
            })
            .max()
            .unwrap_or(0)
            + 1;
        val + 4 - val.rem_euclid(4)
    };

    for (index, states) in table.iter().enumerate() {
        for (lhs, rhs) in states {
            writeln!(
                fmt,
                "{index}:\t{}",
                format_args!(
                    "{}{:^max_len$}{RESET}: {}",
                    match lhs {
                        crate::Token::Terminal(_) => BRIGHT_YELLOW,
                        _ => MAGENTA,
                    },
                    match lhs {
                        crate::Token::Terminal(c) => c.ascii_str().expect("Char isn't ascii"),
                        crate::Token::NonTerminal(r) if &**r == "__entry_point__" => ENTRY_POINT,
                        crate::Token::NonTerminal(r) => r,
                    },
                    MAction(rhs)
                )
            )?;
        }
        if !states.is_empty() {
            writeln!(fmt)?;
        } else {
            writeln!(fmt, "\n{index}:\n")?;
        }
    }
    Ok(())
}
