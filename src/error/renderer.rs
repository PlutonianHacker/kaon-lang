use std::io::{self};
use termcolor::{Color, ColorSpec, WriteColor};

use crate::common::Span;
use crate::error::{Diagnostic, Label, LabelStyle, Severity};

pub struct Styles {
    header_error: ColorSpec,
    header_warning: ColorSpec,
    header_note: ColorSpec,
    header_message: ColorSpec,
    header_help: ColorSpec,
    source_border: ColorSpec,
    primary_label: ColorSpec,
    secondary_label: ColorSpec,
}

impl Styles {
    pub fn header(&self, severity: &Severity) -> &ColorSpec {
        match severity {
            Severity::Error => &self.header_error,
            Severity::Warning => &self.header_warning,
            Severity::Note => &self.header_note,
            Severity::Help => &self.header_help,
        }
    }

    pub fn new() -> Styles {
        let mut header = ColorSpec::new().set_bold(true).set_intense(true).clone();

        Styles {
            header_error: header.set_fg(Some(Color::Red)).clone(),
            header_warning: header.set_fg(Some(Color::Yellow)).clone(),
            header_note: header.set_fg(Some(Color::Green)).clone(),
            header_message: header.set_fg(Some(Color::White)).clone(),
            header_help: header.set_fg(Some(Color::Cyan)).clone(),
            source_border: ColorSpec::new()
                .set_fg(Some(Color::Blue))
                .set_bold(true)
                .clone(),
            primary_label: header.set_fg(Some(Color::Red)).clone(),
            secondary_label: header.set_fg(Some(Color::Yellow)).clone(),
        }
    }
}

pub struct Renderer<'writer> {
    pub writer: &'writer mut dyn WriteColor,
    styles: Styles,
}

impl<'writer> Renderer<'writer> {
    pub fn new(writer: &'writer mut dyn WriteColor) -> Self {
        Renderer {
            writer: writer,
            styles: Styles::new(),
        }
    }

    pub fn set_color(&mut self, spec: &ColorSpec) -> io::Result<()> {
        self.writer.set_color(spec)
    }

    pub fn reset(&mut self) -> io::Result<()> {
        self.writer.reset()
    }

    pub fn styles(&self) -> &Styles {
        &self.styles
    }

    // error[E0365]: expected type after `:`
    pub fn render_header(
        &mut self,
        severity: &Severity,
        code: &Option<String>,
        msg: &String,
    ) -> io::Result<()> {
        self.set_color(&self.styles().header(&severity).clone())?;

        match severity {
            &Severity::Error => write!(self.writer, "error")?,
            &Severity::Warning => write!(self.writer, "warning")?,
            &Severity::Help => write!(self.writer, "help")?,
            _ => write!(self.writer, "bug")?,
        };

        if let Some(code) = code {
            write!(self.writer, "[{}]", code)?;
        }

        self.set_color(&self.styles.header_message.clone())?;
        writeln!(self.writer, ": {}", msg)
    }

    pub fn render_location(&mut self, location: String, padding: usize) -> io::Result<()> {
        self.render_padding(padding)?;

        self.set_color(&self.styles.source_border.clone())?;
        write!(self.writer, " ╭")?;

        self.writer.reset()?;
        writeln!(self.writer, " {}", location)
    }

    pub fn render_break(&mut self, padding: usize) -> io::Result<()> {
        self.render_padding(padding)?;
        self.set_color(&self.styles().source_border.clone())?;
        writeln!(self.writer, " . ")?;
        self.writer.reset()
    }

    pub fn snippet_empty(&mut self) -> io::Result<()> {
        self.set_color(&self.styles.source_border.clone())?;
        writeln!(self.writer, " │ ")?;
        self.writer.reset()
    }

    pub fn inner_gutter(&mut self) -> io::Result<()> {
        self.set_color(&self.styles.source_border.clone())?;
        write!(self.writer, " │ ")?;
        self.writer.reset()
    }

    pub fn render_padding(&mut self, padding: usize) -> io::Result<()> {
        write!(self.writer, "{}", " ".repeat(padding))
    }

    pub fn render_line_number(&mut self, line_number: usize) -> io::Result<()> {
        self.set_color(&self.styles.source_border.clone())?;
        write!(self.writer, "{}", line_number)?;
        self.writer.reset()
    }

    pub fn render_span(
        &mut self,
        offset: usize,
        label: Label,
        caret: &str,
        severity: &Severity,
    ) -> io::Result<()> {
        match (label.style, severity) {
            (LabelStyle::Primary, &Severity::Error) => {
                self.set_color(&self.styles.primary_label.clone())?
            }
            (LabelStyle::Secondary, &Severity::Error) => {
                self.set_color(&self.styles.source_border.clone())?
            }
            (LabelStyle::Primary, &Severity::Warning) => {
                self.set_color(&self.styles.secondary_label.clone())?
            }
            _ => self.set_color(&self.styles.secondary_label.clone())?,
        }

        write!(
            self.writer,
            "{}{} {}",
            " ".repeat(offset),
            caret.repeat(label.span.length),
            label.message
        )?;

        self.writer.reset()
    }

    pub fn render_notes(&mut self, notes: Vec<String>, padding: usize) -> io::Result<()> {
        for note in notes {
            self.render_padding(padding)?;

            self.set_color(&self.styles.source_border.clone())?;
            write!(self.writer, " =")?;

            self.set_color(&self.styles.header_message.clone())?;
            write!(self.writer, " note:")?;

            self.writer.reset()?;
            let lines = note.split("\n").collect::<Vec<&str>>();

            writeln!(self.writer, " {}", lines[0])?;

            for line in lines[1..].iter() {
                writeln!(
                    self.writer,
                    "{} {} {}",
                    " ".repeat(padding),
                    " ".repeat(7),
                    line.trim()
                )?;
            }
        }

        Ok(())
    }

    pub fn render_snippet(
        &mut self,
        padding: usize,
        source: &String,
        labels: &Vec<Label>,
        severity: &Severity,
    ) -> io::Result<()> {
        let lines = Span::lines(source);

        let mut sorted_labels = vec![];
        for label in labels
            .iter()
            .map(|label| (label, Span::line_index(source, label.span.start)))
        {
            sorted_labels.push(label);
        }

        sorted_labels.sort_by(|a, b| a.1 .0.cmp(&b.1 .0));

        let mut _message_count = 0;

        let mut max_label_start = 0;
        let mut max_label_end = 0;

        for label in sorted_labels.iter() {
            let (label_span, _) = label;

            if !label_span.message.is_empty() {
                _message_count += 1;
            }

            max_label_start = std::cmp::max(max_label_start, label_span.span.start);
            max_label_end = std::cmp::max(max_label_end, label_span.span.end());
        }

        let mut previous_line = 0;
        let mut current_line: isize = -1;

        for label in sorted_labels.iter() {
            let (label_span, (line, _)) = label;

            let _same_line_labels: Vec<&(&Label, (usize, usize))> = sorted_labels
                .iter()
                .filter(|label| label.1 .0 == current_line as usize)
                .collect();

            if previous_line == -1
                && *line as isize != current_line
                && *line as isize - 1 != current_line
            {
                self.render_break(padding)?;
            }

            if *line as isize != current_line {
                self.render_line_number(*line + 1)?;
                self.inner_gutter()?;

                writeln!(self.writer, "{}", &lines[*line])?;
                previous_line = current_line;
                current_line = *line as isize;
            }

            // 5 | test
            //   | |  ^^^^^ text
            //   | ---- help: text

            let caret = match label_span.style {
                LabelStyle::Primary => "^",
                LabelStyle::Secondary => "-",
            };
            self.render_padding(padding)?;
            self.inner_gutter()?;

            self.render_span(label.1 .1, label_span.clone().clone(), caret, &severity)?;

            writeln!(self.writer, "")?;
        }

        Ok(())
    }

    pub fn render_source(&mut self, diagnostic: Diagnostic) -> io::Result<()> {
        let source = &diagnostic.labels[0].span.source.as_ref().contents;

        let (start_line, start_col) = Span::line_index(&source, diagnostic.labels[0].span.start);
        let (end_line, _) = Span::line_index(&source, diagnostic.labels[0].span.start);

        let readable_start_line = (start_line + 1).to_string();
        let readable_start_col = (start_col + 1).to_string();
        let readable_end_line = (end_line + 1).to_string();
        let padding = readable_end_line.len();

        let location = format!(
            "{}:{}:{}",
            &diagnostic.labels[0].span.source.path.to_string_lossy(),
            readable_start_line,
            readable_start_col,
        );

        self.render_location(location, padding)?;

        self.render_padding(padding)?;
        self.snippet_empty()?;

        let labels = &diagnostic.labels;
        self.render_snippet(padding, &source, labels, &diagnostic.severity)?;

        self.render_padding(padding)?;
        self.snippet_empty()?;

        if !diagnostic.notes.is_empty() {
            self.render_notes(diagnostic.notes, padding)?;
        }

        writeln!(self.writer, "")
    }

    pub fn render(&mut self, diagnostic: Diagnostic) -> io::Result<()> {
        self.render_header(&diagnostic.severity, &diagnostic.code, &diagnostic.message)?;

        if !&diagnostic.labels.is_empty() {
            return self.render_source(diagnostic);
        }

        write!(self.writer, "")
    }
}
