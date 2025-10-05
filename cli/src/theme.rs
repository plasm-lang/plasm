use anstyle::{Color, RgbColor, Style};

pub struct Theme {
    pub error: Style,
    pub warning: Style,
    pub info: Style,
    pub success: Style,
    pub hint: Style,

    pub path: Style,
    pub line_number: Style,
    pub code: Style,
    pub keyword: Style,
    pub bracket: Style,
    pub literal: Style,
    pub built_in_type: Style,
    pub comment: Style,
}

pub const fn hex(x: u32) -> Color {
    Color::Rgb(RgbColor(
        ((x >> 16) & 0xFF) as u8,
        ((x >> 8) & 0xFF) as u8,
        (x & 0xFF) as u8,
    ))
}

#[allow(dead_code)]
impl Theme {
    /// https://lospec.com/palette-list/nebulaspace
    pub const NEBULASPACE: Self = Self {
        error: Style::new().fg_color(Some(Color::Rgb(RgbColor(166, 99, 114)))),
        warning: Style::new().fg_color(Some(Color::Rgb(RgbColor(230, 235, 106)))),
        info: Style::new().fg_color(Some(Color::Rgb(RgbColor(61, 71, 110)))),
        success: Style::new().fg_color(Some(Color::Rgb(RgbColor(109, 133, 44)))),
        hint: Style::new().fg_color(Some(Color::Rgb(RgbColor(61, 71, 110)))),

        path: Style::new(),
        line_number: Style::new().fg_color(Some(Color::Rgb(RgbColor(50, 36, 77)))),
        code: Style::new().fg_color(Some(Color::Rgb(RgbColor(237, 232, 225)))),
        keyword: Style::new().fg_color(Some(Color::Rgb(RgbColor(166, 99, 114)))),
        bracket: Style::new().fg_color(Some(Color::Rgb(RgbColor(230, 235, 106)))),
        literal: Style::new().fg_color(Some(Color::Rgb(RgbColor(93, 133, 140)))),
        built_in_type: Style::new().fg_color(Some(Color::Rgb(RgbColor(166, 99, 114)))),
        comment: Style::new()
            .fg_color(Some(Color::Rgb(RgbColor(50, 36, 77))))
            .italic(),
    };

    /// https://lospec.com/palette-list/dracula-standard
    pub const DRACULA: Self = Self {
        error: Style::new().fg_color(Some(Color::Rgb(RgbColor(255, 85, 85)))),
        warning: Style::new().fg_color(Some(Color::Rgb(RgbColor(241, 250, 140)))),
        info: Style::new().fg_color(Some(Color::Rgb(RgbColor(98, 114, 164)))),
        success: Style::new().fg_color(Some(Color::Rgb(RgbColor(80, 250, 123)))),
        hint: Style::new().fg_color(Some(Color::Rgb(RgbColor(80, 250, 123)))),

        path: Style::new(),
        line_number: Style::new().fg_color(Some(Color::Rgb(RgbColor(68, 71, 90)))),
        code: Style::new().fg_color(Some(Color::Rgb(RgbColor(248, 248, 242)))),
        keyword: Style::new().fg_color(Some(Color::Rgb(RgbColor(139, 233, 253)))),
        bracket: Style::new().fg_color(Some(Color::Rgb(RgbColor(255, 184, 108)))),
        literal: Style::new().fg_color(Some(Color::Rgb(RgbColor(189, 147, 249)))),
        built_in_type: Style::new().fg_color(Some(Color::Rgb(RgbColor(139, 233, 253)))),
        comment: Style::new()
            .fg_color(Some(Color::Rgb(RgbColor(68, 71, 90))))
            .italic(),
    };

    /// https://lospec.com/palette-list/colodore
    pub const COLODORE: Self = Self {
        error: Style::new().fg_color(Some(hex(0xc46c71))),
        warning: Style::new().fg_color(Some(hex(0xedf171))),
        info: Style::new().fg_color(Some(hex(0x706deb))),
        success: Style::new().fg_color(Some(hex(0x56ac4d))),
        hint: Style::new().fg_color(Some(hex(0x706deb))),

        path: Style::new(),
        line_number: Style::new().fg_color(Some(hex(0x4a4a4a))),
        code: Style::new(),
        keyword: Style::new().fg_color(Some(hex(0x8e3c97))),
        bracket: Style::new().fg_color(Some(hex(0x813338))),
        literal: Style::new().fg_color(Some(hex(0x706deb))),
        built_in_type: Style::new().fg_color(Some(hex(0x8e3c97))),
        comment: Style::new().fg_color(Some(hex(0x4a4a4a))).italic(),
    };

    /// https://lospec.com/palette-list/akc12
    pub const AKC12: Self = Self {
        error: Style::new().fg_color(Some(hex(0xd9626b))),
        warning: Style::new().fg_color(Some(hex(0xffeb99))),
        info: Style::new().fg_color(Some(hex(0x355d68))),
        success: Style::new().fg_color(Some(hex(0x6aaf9d))),
        hint: Style::new().fg_color(Some(hex(0x355d68))),

        path: Style::new(),
        line_number: Style::new().fg_color(Some(hex(0x1b1e34))),
        code: Style::new(),
        keyword: Style::new().fg_color(Some(hex(0xa73169))),
        bracket: Style::new().fg_color(Some(hex(0xffc27a))),
        literal: Style::new().fg_color(Some(hex(0x355d68))),
        built_in_type: Style::new().fg_color(Some(hex(0xa73169))),
        comment: Style::new().fg_color(Some(hex(0x1b1e34))).italic(),
    };

    /// https://lospec.com/palette-list/commodore64
    pub const COMMODORE64: Self = Self {
        error: Style::new().fg_color(Some(hex(0x9f4e44))),
        warning: Style::new().fg_color(Some(hex(0xc9d487))),
        info: Style::new().fg_color(Some(hex(0x887ecb))),
        success: Style::new().fg_color(Some(hex(0x5cab5e))),
        hint: Style::new().fg_color(Some(hex(0x887ecb))),

        path: Style::new(),
        line_number: Style::new().fg_color(Some(hex(0x626262))),
        code: Style::new(),
        keyword: Style::new().fg_color(Some(hex(0xa057a3))),
        bracket: Style::new().fg_color(Some(hex(0xc9d487))),
        literal: Style::new().fg_color(Some(hex(0x887ecb))),
        built_in_type: Style::new().fg_color(Some(hex(0xa057a3))),
        comment: Style::new().fg_color(Some(hex(0x626262))).italic(),
    };
}
