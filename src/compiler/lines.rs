use core::fmt;

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct LineAttr<'a> {
    pub file: Option<&'a str>,
    pub line: usize,
    pub start: usize,
}

impl fmt::Display for LineAttr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.file.unwrap_or("<unknown>"), self.line)
    }
}

pub fn find_lines(src: &str) -> Vec<LineAttr<'_>> {
    let mut lines = Vec::new();

    let mut file = None;
    let mut lino = 1;
    let mut bytes = 0;

    for line in src.split('\n') {
        let mut ws = line.split_whitespace();
        let first = ws.next();
        let secnd = ws.next();
        if first == Some("#") && secnd == Some("line") {
            lino = ws.next().and_then(|n| n.parse().ok()).unwrap_or(0_usize).saturating_sub(1);
            file = ws.next().and_then(|f| f.get(1..f.len() - 1));
        } else if first == Some("#line") || (first == Some("#") && secnd.map_or(false, |e| !e.chars().any(|e| !e.is_ascii_digit()))) {
            lino = secnd.and_then(|n| n.parse().ok()).unwrap_or(0_usize).saturating_sub(1);
            file = ws.next().and_then(|f| f.get(1..f.len() - 1));
        } else {
            lino += 1;
        }

        lines.push(LineAttr { file, line: lino, start: bytes });
        bytes += line.len() + 1;
    }

    lines
}
