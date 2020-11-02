//! A library for pretty-printing lint errors with a given source text.
//! 
//! The API is fairly minimal, and the output closely resembles rustc's.
//! 

use colored::Colorize;

// Reexport for color control.
pub use colored;

/// Position in a source document 1-based.
#[derive(Debug, Default)]
pub struct Position {
    pub line: usize,
    pub col: usize,
}

/// The range the lint spans.
#[derive(Debug, Default)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

/// Lint severity.
#[derive(Debug)]
pub enum Severity {
    Error,
    Warning,
    Info,
    Success,
}

/// Show a pretty lint message similar to rustc's messages.
#[derive(Debug)]
pub struct PrettyLint<'l> {
    source: &'l str,
    severity: Severity,
    lint_code: Option<&'l str>,
    file_path: Option<&'l str>,
    span: Span,
    message: Option<&'l str>,
    inline_message: Option<&'l str>,
    notes: &'l [&'l str],

    is_additional: bool,

    additional_lints: Vec<PrettyLint<'l>>,
}

impl<'l> PrettyLint<'l> {
    pub fn new(source: &'l str) -> Self {
        PrettyLint {
            lint_code: None,
            file_path: None,
            inline_message: None,
            message: None,
            severity: Severity::Error,
            span: Span::default(),
            source,
            notes: &[],
            is_additional: false,
            additional_lints: Vec::new(),
        }
    }

    pub fn with_message(mut self, message: &'l str) -> Self {
        self.message = Some(message);
        self
    }

    pub fn with_code(mut self, code: &'l str) -> Self {
        self.lint_code = Some(code);
        self
    }

    pub fn with_severity(mut self, severity: Severity) -> Self {
        self.severity = severity;
        self
    }

    pub fn error(source: &'l str) -> Self {
        PrettyLint::new(source).with_severity(Severity::Error)
    }

    pub fn warning(source: &'l str) -> Self {
        PrettyLint::new(source).with_severity(Severity::Warning)
    }

    pub fn info(source: &'l str) -> Self {
        PrettyLint::new(source).with_severity(Severity::Info)
    }

    pub fn with_file_path(mut self, file_path: &'l str) -> Self {
        self.file_path = Some(file_path);
        self
    }

    pub fn with_inline_message(mut self, msg: &'l str) -> Self {
        self.inline_message = Some(msg);
        self
    }

    pub fn with_notes(mut self, notes: &'l [&'l str]) -> Self {
        self.notes = notes;
        self
    }

    pub fn at(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn and(mut self, mut lint: PrettyLint<'l>) -> Self {
        lint.is_additional = true;
        self.additional_lints.push(lint);
        self
    }
}

impl core::fmt::Display for PrettyLint<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let multiline = self.span.start.line != self.span.end.line;

        if let (Some(message), false) = (self.message, self.is_additional) {
            match self.severity {
                Severity::Error => "error".red().bold().fmt(f)?,
                Severity::Warning => "warning".yellow().bold().fmt(f)?,
                Severity::Info => "info".cyan().bold().fmt(f)?,
                Severity::Success => "success".green().bold().fmt(f)?,
            };

            if let Some(code) = self.lint_code {
                if !code.is_empty() {
                    match self.severity {
                        Severity::Error => {
                            "(".red().bold().fmt(f)?;
                            code.red().bold().fmt(f)?;
                            ")".red().bold().fmt(f)?;
                        }
                        Severity::Warning => {
                            "(".yellow().bold().fmt(f)?;
                            code.yellow().bold().fmt(f)?;
                            ")".yellow().bold().fmt(f)?;
                        }
                        Severity::Info => {
                            "(".cyan().bold().fmt(f)?;
                            code.cyan().bold().fmt(f)?;
                            ")".cyan().bold().fmt(f)?;
                        }
                        Severity::Success => {
                            "(".green().bold().fmt(f)?;
                            code.green().bold().fmt(f)?;
                            ")".green().bold().fmt(f)?;
                        }
                    };
                }
            }

            ": ".bold().fmt(f)?;
            message.bold().fmt(f)?;
            f.write_str("\n")?;
        }

        let mut line_digits = digits(usize::max(self.span.start.line, self.span.end.line));

        if multiline {
            line_digits = usize::max(3, line_digits);
        }

        if !self.additional_lints.is_empty() {
            for add in &self.additional_lints {
                line_digits = usize::max(line_digits, digits(add.span.start.line));
                line_digits = usize::max(line_digits, digits(add.span.end.line));
            }
        }

        let num_padding = " ".repeat(line_digits);
        let padding_sep = " | ".blue().bold();

        let padding_sep_span = match self.severity {
            Severity::Error => format!("{}{}", " |".blue().bold(), "|".red()),
            Severity::Warning => format!("{}{}", " |".blue().bold(), "|".yellow()),
            Severity::Info => format!("{}{}", " |".blue().bold(), "|".cyan()),
            Severity::Success => format!("{}{}", " |".blue().bold(), "|".green()),
        };

        f.write_str(&num_padding)?;
        if self.is_additional {
            "... ".blue().bold().fmt(f)?;
        } else {
            "--> ".blue().bold().fmt(f)?;
        }
        if let Some(fpath) = self.file_path {
            if !fpath.is_empty() {
                f.write_str(fpath)?;
                f.write_str(":")?;
            }
        }
        f.write_str(&format!("{}:{}", self.span.start.line, self.span.start.col))?;
        f.write_str("\n")?;

        f.write_str(&num_padding)?;
        padding_sep.fmt(f)?;
        f.write_str("\n")?;

        for (i, line) in self.source.split('\n').enumerate() {
            let line_number = i + 1;

            if line_number == self.span.start.line {
                write!(
                    f,
                    "{: ^size$}",
                    line_number.to_string().blue().bold(),
                    size = line_digits
                )?;

                padding_sep.fmt(f)?;
                f.write_str(line)?;
                f.write_str("\n")?;
            }

            if line_number > self.span.end.line {
                break;
            }

            if !multiline {
                continue;
            }

            if line_number == self.span.start.line + 1 {
                num_padding.fmt(f)?;
                padding_sep.fmt(f)?;
                for _ in 0..self.span.start.col.checked_sub(1).unwrap_or(0) {
                    match self.severity {
                        Severity::Error => {
                            "_".red().fmt(f)?;
                        }
                        Severity::Warning => {
                            "_".yellow().fmt(f)?;
                        }
                        Severity::Info => {
                            "_".cyan().fmt(f)?;
                        }
                        Severity::Success => {
                            "_".green().fmt(f)?;
                        }
                    }
                }

                match self.severity {
                    Severity::Error => {
                        "^".red().bold().fmt(f)?;
                    }
                    Severity::Warning => {
                        "^".yellow().bold().fmt(f)?;
                    }
                    Severity::Info => {
                        "^".cyan().bold().fmt(f)?;
                    }
                    Severity::Success => {
                        "^".green().bold().fmt(f)?;
                    }
                }
            }

            if line_number == self.span.end.line {
                if line_number - self.span.start.line > 1 {
                    f.write_str("\n")?;
                    ".".repeat(line_digits).bold().blue().fmt(f)?;
                    padding_sep_span.fmt(f)?;
                    f.write_str("\n")?;

                    num_padding.fmt(f)?;
                    padding_sep_span.fmt(f)?;
                }
                f.write_str("\n")?;

                write!(
                    f,
                    "{: ^size$}",
                    line_number.to_string().blue().bold(),
                    size = line_digits
                )?;
                padding_sep_span.fmt(f)?;
                f.write_str(line)?;
                f.write_str("\n")?;

                num_padding.fmt(f)?;
                padding_sep_span.fmt(f)?;

                for _ in 0..self.span.end.col.checked_sub(1).unwrap_or(0) {
                    match self.severity {
                        Severity::Error => {
                            "_".red().fmt(f)?;
                        }
                        Severity::Warning => {
                            "_".yellow().fmt(f)?;
                        }
                        Severity::Info => {
                            "_".cyan().fmt(f)?;
                        }
                        Severity::Success => {
                            "_".green().fmt(f)?;
                        }
                    }
                }

                match self.severity {
                    Severity::Error => {
                        "^".red().bold().fmt(f)?;
                    }
                    Severity::Warning => {
                        "^".yellow().bold().fmt(f)?;
                    }
                    Severity::Info => {
                        "^".cyan().bold().fmt(f)?;
                    }
                    Severity::Success => {
                        "^".green().bold().fmt(f)?;
                    }
                }
            }
        }

        if !multiline {
            let error_len = self
                .span
                .end
                .col
                .checked_sub(self.span.start.col)
                .expect("end position must be after the start position");

            f.write_str(&num_padding)?;
            padding_sep.fmt(f)?;

            for _ in 0..self.span.start.col.checked_sub(1).unwrap_or(0) {
                f.write_str(" ")?;
            }

            for _ in 0..=error_len {
                match self.severity {
                    Severity::Error => {
                        "^".red().bold().fmt(f)?;
                    }
                    Severity::Warning => {
                        "^".yellow().bold().fmt(f)?;
                    }
                    Severity::Info => {
                        "^".cyan().bold().fmt(f)?;
                    }
                    Severity::Success => {
                        "^".green().bold().fmt(f)?;
                    }
                }
            }
        }

        if let Some(msg) = self.inline_message {
            f.write_str(" ")?;

            match self.severity {
                Severity::Error => {
                    msg.bold().red().fmt(f)?;
                }
                Severity::Warning => {
                    msg.bold().yellow().fmt(f)?;
                }
                Severity::Info => {
                    msg.bold().cyan().fmt(f)?;
                }
                Severity::Success => {
                    msg.bold().green().fmt(f)?;
                }
            }
        }

        if !self.additional_lints.is_empty() {
            f.write_str("\n")?;
            f.write_str(&num_padding)?;
            padding_sep.fmt(f)?;
            f.write_str("\n")?;
            for add in &self.additional_lints {
                add.fmt(f)?;
            }
        }

        if !self.notes.is_empty() && !self.is_additional {
            f.write_str("\n")?;
            f.write_str(&num_padding)?;
            padding_sep.fmt(f)?;
            for note in self.notes {
                f.write_str("\n")?;
                num_padding.fmt(f)?;
                " - ".bold().blue().fmt(f)?;
                f.write_str(note)?;
            }

            for add in &self.additional_lints {
                for note in add.notes {
                    f.write_str("\n")?;
                    num_padding.fmt(f)?;
                    " - ".bold().blue().fmt(f)?;
                    f.write_str(note)?;
                }
            }
        }

        Ok(())
    }
}

/// Get the digit count of a number.
/// Simplest way.
fn digits(mut val: usize) -> usize {
    if val == 0 {
        return 1;
    }

    let mut count = 0;
    while val != 0 {
        val /= 10;
        count += 1;
    }

    count
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_print() {
        let src = r#"
this is
some text
that is wrong
"#;

        let lint = PrettyLint::error(src)
            .with_message("This is an error")
            .with_file_path(file!())
            .with_code("syntax")
            .at(Span {
                start: Position { line: 3, col: 1 },
                end: Position { line: 3, col: 4 },
            })
            .with_inline_message("this is wrong")
            .with_notes(&["This is a note", "This is another note"])
            .and(
                PrettyLint::info(src)
                    .with_inline_message("stuff is here")
                    .with_file_path(file!())
                    .at(Span {
                        start: Position { line: 2, col: 1 },
                        end: Position { line: 2, col: 4 },
                    }),
            );

        println!("{}", lint);
    }
}
