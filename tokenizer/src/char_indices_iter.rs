use std::io::{BufRead, BufReader, Cursor, Result};

pub struct CharIndicesIter<R: BufRead> {
    reader: R,
    /// global byte offset from the start of the stream
    global_off: usize,
}

impl<R: BufRead> CharIndicesIter<R> {
    pub fn new(reader: R) -> Self {
        Self {
            reader,
            global_off: 0,
        }
    }

    #[inline]
    fn read_one(&mut self) -> Result<Option<u8>> {
        let mut b = [0u8; 1];
        match self.reader.read(&mut b)? {
            0 => Ok(None),
            1 => Ok(Some(b[0])),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn read_more(&mut self, buf: &mut [u8], have: usize, need: usize) -> Result<bool> {
        debug_assert!(need <= 3);
        let mut filled = 0usize;
        while filled < need {
            let n = self.reader.read(&mut buf[have + filled..have + need])?;
            if n == 0 {
                return Ok(false);
            }
            filled += n;
        }
        Ok(true)
    }
}

impl<R: BufRead> Iterator for CharIndicesIter<R> {
    type Item = (usize, char);

    fn next(&mut self) -> Option<Self::Item> {
        // read first byte of the char
        let b0 = match self.read_one() {
            Ok(Some(b)) => b,
            Ok(None) => return None, // EOF
            Err(_) => return None,
        };

        // ASCII fast path
        if b0 < 0x80 {
            let idx = self.global_off;
            self.global_off += 1;
            return Some((idx, b0 as char));
        }

        // find expected UTF-8 length
        let (need_cont, total_len) = if (0xC2..=0xDF).contains(&b0) {
            (1, 2)
        } else if (0xE0..=0xEF).contains(&b0) {
            (2, 3)
        } else if (0xF0..=0xF4).contains(&b0) {
            (3, 4)
        } else {
            // Invalid start byte (C0/C1, F5..FF, or lone 10xxxxxx)
            return None;
        };

        // collect up to 4 bytes in a small local buffer
        let mut code = [0u8; 4];
        code[0] = b0;

        // fill the rest of the character
        match self.read_more(&mut code, 1, need_cont) {
            Ok(true) => {}
            _ => return None, // early EOF
        }

        // basic check of continuation bytes (10xxxxxx). Other prohibitions (overlongs, surrogates) will be checked by from_utf8.
        for item in code.iter().take(need_cont + 1).skip(1) {
            if item & 0b1100_0000 != 0b1000_0000 {
                return None;
            }
        }

        // check correctness and extract char
        let slice = &code[..total_len];
        match std::str::from_utf8(slice) {
            Ok(s) => {
                let ch = s.chars().next().unwrap();
                let idx = self.global_off;
                self.global_off += total_len;
                Some((idx, ch))
            }
            Err(_) => {
                // invalid sequence, stop iteration
                // (could return '\u{FFFD}' and continue, but here we stop)
                None
            }
        }
    }
}

impl<'a> From<&'a str> for CharIndicesIter<BufReader<Cursor<&'a [u8]>>> {
    fn from(s: &'a str) -> Self {
        let cur = Cursor::new(s.as_bytes());
        CharIndicesIter::new(BufReader::new(cur))
    }
}

impl From<String> for CharIndicesIter<BufReader<Cursor<Vec<u8>>>> {
    fn from(s: String) -> Self {
        let cur = Cursor::new(s.into_bytes());
        CharIndicesIter::new(BufReader::new(cur))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Etalon check: streamed sequence (i, ch)
    /// matches s.char_indices().
    fn assert_matches_char_indices(s: &str, got: &[(usize, char)]) {
        let expect: Vec<(usize, char)> = s.char_indices().collect();
        assert_eq!(
            got, &expect,
            "streamed char_indices must match str::char_indices"
        );

        if let Some((i_last, ch_last)) = got.last().copied() {
            assert_eq!(i_last + ch_last.len_utf8(), s.len());
        } else {
            assert_eq!(s.len(), 0);
        }
    }

    #[test]
    fn empty_string() {
        let s = "";
        let v: Vec<_> = CharIndicesIter::from(s).collect();
        assert!(v.is_empty());
        assert_matches_char_indices(s, &v);
    }

    #[test]
    fn ascii_basic() {
        let s = "hello, world!";
        let v: Vec<_> = CharIndicesIter::from(s).collect();
        assert_matches_char_indices(s, &v);
    }

    #[test]
    fn only_newlines() {
        let s = "\n\n\n";
        let v: Vec<_> = CharIndicesIter::from(s).collect();
        assert_matches_char_indices(s, &v);
        assert!(v.iter().all(|&(_, ch)| ch == '\n'));
    }

    #[test]
    fn crlf_sequence() {
        let s = "\r\n\r\n\r\n";
        let v: Vec<_> = CharIndicesIter::from(s).collect();
        assert_matches_char_indices(s, &v);
        for (k, &(_, ch)) in v.iter().enumerate() {
            if k % 2 == 0 {
                assert_eq!(ch, '\r');
            } else {
                assert_eq!(ch, '\n');
            }
        }
    }

    #[test]
    fn multibyte_mixed_2_3_4_bytes() {
        let s = "aéb€c🦀d";
        let v: Vec<_> = CharIndicesIter::from(s).collect();
        assert_matches_char_indices(s, &v);
    }

    #[test]
    fn unicode_boundaries_and_newlines() {
        let s = "A\né\n€\n🦀\nB";
        let v: Vec<_> = CharIndicesIter::from(s).collect();
        assert_matches_char_indices(s, &v);

        let mut prev = 0usize;
        for (i, _) in &v {
            assert!(*i >= prev);
            prev = *i;
        }
    }

    #[test]
    fn emoji_and_neighbors() {
        let s = "x🦀y";
        let v: Vec<_> = CharIndicesIter::from(s).collect();
        assert_matches_char_indices(s, &v);

        let crabs: Vec<_> = v.iter().filter(|&&(_, ch)| ch == '🦀').collect();
        assert_eq!(crabs.len(), 1);
        let (i_crab, _) = crabs[0];
        assert_eq!(&s[*i_crab..*i_crab + '🦀'.len_utf8()], "🦀");
    }

    #[test]
    fn long_repeated_multibyte() {
        let unit = "é€🦀";
        let s = unit.repeat(1000);
        let v: Vec<_> = CharIndicesIter::from(s.as_str()).collect();
        assert_eq!(v.len(), 3_000);
        assert_matches_char_indices(&s, &v);
    }

    #[test]
    fn reconstruct_string_from_indices() {
        let s = "0é1€2🦀3";
        let v: Vec<_> = CharIndicesIter::from(s).collect();

        let recon: String = v.iter().map(|&(_, ch)| ch).collect();
        assert_eq!(recon, s);

        for &(i, ch) in &v {
            let end = i + ch.len_utf8();
            assert_eq!(&s[i..end], ch.to_string());
        }
    }

    #[test]
    fn various_separators_and_whitespace() {
        let s = " \t\r\n\u{00A0}\u{2003}é€🦀";
        let v: Vec<_> = CharIndicesIter::from(s).collect();
        assert_matches_char_indices(s, &v);
        let tail: String = v
            .iter()
            .rev()
            .take(3)
            .map(|&(_, ch)| ch)
            .collect::<Vec<_>>()
            .into_iter()
            .rev()
            .collect();
        assert_eq!(tail, "é€🦀");
    }

    #[test]
    fn utf8_bom_is_valid() {
        let s = "\u{FEFF}fn main() {}";
        let v: Vec<_> = CharIndicesIter::from(s).collect();
        assert_matches_char_indices(s, &v);
        assert_eq!(v.first().unwrap().1, '\u{FEFF}');
    }

    #[test]
    fn large_ascii_run() {
        let s = "a".repeat(100_000);
        let v: Vec<_> = CharIndicesIter::from(s.as_str()).collect();
        assert_eq!(v.len(), 100_000);
        assert_matches_char_indices(&s, &v);
    }

    #[test]
    fn truncated_multibyte_at_end() {
        let bytes = [b'a', 0xC3];
        let cur = Cursor::new(bytes);
        let mut it = CharIndicesIter::new(BufReader::new(cur));
        assert_eq!(it.next(), Some((0, 'a')));
        assert_eq!(it.next(), None);
    }

    #[test]
    fn lone_continuation_byte_midstream() {
        let bytes = [b'a', 0x80, b'b'];
        let cur = Cursor::new(bytes);
        let mut it = CharIndicesIter::new(BufReader::new(cur));
        assert_eq!(it.next(), Some((0, 'a')));
        assert_eq!(it.next(), None);
    }

    #[test]
    fn overlong_ascii_c0_80() {
        let bytes = [0xC0, 0x80, b'X'];
        let cur = Cursor::new(bytes);
        let mut it = CharIndicesIter::new(BufReader::new(cur));
        assert_eq!(it.next(), None);
    }

    #[test]
    fn invalid_start_byte_ff() {
        let bytes = [0xFF, b'x'];
        let cur = Cursor::new(bytes);
        let mut it = CharIndicesIter::new(BufReader::new(cur));
        assert_eq!(it.next(), None);
    }

    #[test]
    fn surrogate_should_fail_if_encoded() {
        // Surrogate is not allowed in Unicode scalar values, valid UTF-8 does not encode it.
        // Synthetic: try "D800" in three-byte form (often found in broken streams).
        // This is a sequence that from_utf8 will reject.
        let bytes = [0xED, 0xA0, 0x80]; // U+D800 (surrogate)
        let cur = Cursor::new(bytes);
        let mut it = CharIndicesIter::new(BufReader::new(cur));
        assert_eq!(it.next(), None);
    }
}
