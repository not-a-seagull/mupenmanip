// MIT/Apache2 License

use bytes::{BufMut, BytesMut};
use core::{convert::TryFrom, fmt, str};
use memchr::memrchr;
use std::{
    io::{self, prelude::*, IoSlice},
    panic::catch_unwind,
    ptr,
};

/// From where the movie starts.
#[derive(Debug, Clone, Copy)]
#[repr(u16)]
pub enum MovieStart {
    FromStart = 2,
    FromSnapshot = 1,
}

impl fmt::Display for MovieStart {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::FromStart => f.write_str("From start"),
            Self::FromSnapshot => f.write_str("From snapshot"),
        }
    }
}

impl Default for MovieStart {
    #[inline]
    fn default() -> Self {
        Self::FromStart
    }
}

impl TryFrom<u16> for MovieStart {
    type Error = &'static str;

    #[inline]
    fn try_from(value: u16) -> Result<MovieStart, &'static str> {
        Ok(match value {
            1 => Self::FromSnapshot,
            2 => Self::FromStart,
            _ => return Err("Invalid Movie: MovieStart type is incorrect"),
        })
    }
}

/// Controller flags.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct ControllerFlags {
    pub is_present: bool,
    pub has_mempak: bool,
    pub has_rumblepack: bool,
}

impl fmt::Display for ControllerFlags {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            " - Is Present: {}
 - Has Mempak: {}
 - Has Rumblepak: {}
",
            self.is_present, self.has_mempak, self.has_rumblepack
        )
    }
}

/// The header for the M64 file.
#[derive(Debug, Clone)]
pub struct MHeader {
    /// 00C 4-byte little-endian unsigned int: number of frames (vertical interrupts)
    pub frame_count: u32,
    /// 010 4-byte little-endian unsigned int: rerecord count
    pub rerecord_count: u32,
    /// 014 1-byte unsigned int: frames (vertical interrupts) per second
    pub fps: u8,
    /// 015 1-byte unsigned int: number of controllers
    pub num_controllers: u8,
    /// 018 4-byte little-endian unsigned int: number of input samples for any controllers
    pub input_samples: u32,
    /// 01C 2-byte unsigned int: movie start type
    pub movie_start_type: MovieStart,
    /// 020 4-byte unsigned int: controller flags
    pub controller_flags: [ControllerFlags; 4],
    /// 0C4 32-byte ASCII string: internal name of ROM used when recording, directly from ROM
    pub rom_internal_name_ascii: [u8; 32],
    /// 0E4 4-byte unsigned int: CRC32 of ROM used when recording, directly from ROM
    pub crc32: u32,
    /// 0E8 2-byte unsigned int: country code of ROM used when recording, directly from ROM
    pub country_code: u16,
    /// 122 64-byte ASCII string: name of video plugin used when recording, directly from plugin
    pub video_plugin_name_ascii: [u8; 64],
    /// 162 64-byte ASCII string: name of sound plugin used when recording, directly from plugin
    pub sound_plugin_name_ascii: [u8; 64],
    /// 1A2 64-byte ASCII string: name of input plugin used when recording, directly from plugin
    pub input_plugin_name_ascii: [u8; 64],
    /// 1E2 64-byte ASCII string: name of rsp plugin used when recording, directly from plugin
    pub rsp_plugin_name_ascii: [u8; 64],
    /// 222 222-byte UTF-8 string: author name info
    pub author_name_ascii: [u8; 222],
    /// 300 256-byte UTF-8 string: author movie description info
    pub author_desc_ascii: [u8; 256],
}

impl MHeader {
    #[inline]
    pub fn rom_internal_name(&self) -> &str {
        let zero_pos = memrchr(0, &self.rom_internal_name_ascii).unwrap_or(32);
        str::from_utf8(&self.rom_internal_name_ascii[0..zero_pos])
            .expect("Invalid ROM internal name")
    }

    #[inline]
    pub fn video_plugin_name(&self) -> &str {
        let zero_pos = memrchr(0, &self.video_plugin_name_ascii).unwrap_or(64);
        str::from_utf8(&self.video_plugin_name_ascii[0..zero_pos])
            .expect("Invalid video plugin name")
    }

    #[inline]
    pub fn sound_plugin_name(&self) -> &str {
        let zero_pos = memrchr(0, &self.sound_plugin_name_ascii).unwrap_or(64);
        str::from_utf8(&self.sound_plugin_name_ascii[0..zero_pos])
            .expect("Invalid sound plugin name")
    }

    #[inline]
    pub fn input_plugin_name(&self) -> &str {
        let zero_pos = memrchr(0, &self.input_plugin_name_ascii).unwrap_or(64);
        str::from_utf8(&self.input_plugin_name_ascii[0..zero_pos])
            .expect("Invalid input plugin name")
    }

    #[inline]
    pub fn rsp_plugin_name(&self) -> &str {
        let zero_pos = memrchr(0, &self.rsp_plugin_name_ascii).unwrap_or(64);
        str::from_utf8(&self.rsp_plugin_name_ascii[0..zero_pos]).expect("Invalid RSP plugin name")
    }

    #[inline]
    pub fn author_name(&self) -> &str {
        let zero_pos = memrchr(0, &self.author_name_ascii).unwrap_or(222);
        str::from_utf8(&self.author_name_ascii[0..zero_pos]).expect("Invalid author name")
    }

    #[inline]
    pub fn author_desc(&self) -> &str {
        let zero_pos = memrchr(0, &self.author_desc_ascii).unwrap_or(256);
        str::from_utf8(&self.author_desc_ascii[0..zero_pos]).expect("Invalid author description")
    }

    #[inline]
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, &'static str> {
        if bytes.len() != 1024 {
            return Err("Header bytes must be 1024 in length");
        }

        // check the signature
        match &bytes[0..4] {
            &[0x4D, 0x36, 0x34, 0x1A] => (),
            _ => return Err("Header did not have signature [4D, 36, 34, 1A]"),
        }

        // version number, should be 3
        match bytes[4] {
            3 => (),
            _ => return Err("Program does not support versions aside from 3"),
        }

        // number of frames
        let mut frame_count = [0; 4];
        frame_count.copy_from_slice(&bytes[0xC..0xC + 4]);
        let frame_count = u32::from_le_bytes(frame_count);

        // rerecord counts
        let mut rerecord_count = [0; 4];
        rerecord_count.copy_from_slice(&bytes[0x10..0x10 + 4]);
        let rerecord_count = u32::from_le_bytes(rerecord_count);

        // frames per second
        let fps = bytes[0x14];

        // number of controllers
        let num_controllers = bytes[0x15];

        // number of input samples for controllers
        let mut input_samples = [0; 4];
        input_samples.copy_from_slice(&bytes[0x18..0x18 + 4]);
        let input_samples = u32::from_le_bytes(input_samples);

        // movie start type
        let mut movie_start_type = [0; 2];
        movie_start_type.copy_from_slice(&bytes[0x1C..0x1C + 2]);
        let movie_start_type = u16::from_le_bytes(movie_start_type);
        let movie_start_type = MovieStart::try_from(movie_start_type)?;

        let mut ctrl_flag = [0; 4];
        ctrl_flag.copy_from_slice(&bytes[0x20..0x20 + 4]);
        let ctrl_flag = u32::from_le_bytes(ctrl_flag);

        let mut controller_flags: [ControllerFlags; 4] = Default::default();
        for offset in 0..4 {
            controller_flags[offset].is_present = (ctrl_flag & (1 << offset)) != 0;
            controller_flags[offset].has_mempak = (ctrl_flag & (1 << (offset + 4))) != 0;
            controller_flags[offset].has_rumblepack = (ctrl_flag & (1 << (offset + 8))) != 0;
        }

        let mut rom_internal_name_ascii = [0; 32];
        rom_internal_name_ascii.copy_from_slice(&bytes[0xC4..0xC4 + 32]);

        let mut crc32 = [0; 4];
        crc32.copy_from_slice(&bytes[0xE4..0xE4 + 4]);
        let crc32 = u32::from_le_bytes(crc32);

        let mut country_code = [0; 2];
        country_code.copy_from_slice(&bytes[0xE8..0xE8 + 2]);
        let country_code = u16::from_le_bytes(country_code);

        let mut video_plugin_name_ascii = [0; 64];
        video_plugin_name_ascii.copy_from_slice(&bytes[0x122..0x122 + 64]);

        let mut sound_plugin_name_ascii = [0; 64];
        sound_plugin_name_ascii.copy_from_slice(&bytes[0x162..0x162 + 64]);

        let mut input_plugin_name_ascii = [0; 64];
        input_plugin_name_ascii.copy_from_slice(&bytes[0x1A2..0x1A2 + 64]);

        let mut rsp_plugin_name_ascii = [0; 64];
        rsp_plugin_name_ascii.copy_from_slice(&bytes[0x1E2..0x1E2 + 64]);

        let mut author_name_ascii = [0; 222];
        author_name_ascii.copy_from_slice(&bytes[0x222..0x222 + 222]);

        let mut author_desc_ascii = [0; 256];
        author_desc_ascii.copy_from_slice(&bytes[0x300..0x300 + 256]);

        Ok(Self {
            frame_count,
            rerecord_count,
            fps,
            num_controllers,
            input_samples,
            movie_start_type,
            controller_flags,
            rom_internal_name_ascii,
            crc32,
            country_code,
            video_plugin_name_ascii,
            sound_plugin_name_ascii,
            input_plugin_name_ascii,
            rsp_plugin_name_ascii,
            author_name_ascii,
            author_desc_ascii,
        })
    }

    #[inline]
    pub fn to_bytes(&self) -> BytesMut {
        let mut b = BytesMut::with_capacity(1024);
        b.put_slice(b"M64\x1A");
        b.put_u32_le(3);
        b.put_u32_le(0);
        b.put_u32_le(self.frame_count);
        b.put_u32_le(self.rerecord_count);
        b.put_slice(&[self.fps]);
        b.put_slice(&[self.num_controllers]);
        b.put_u16_le(0);
        b.put_u32_le(self.input_samples);
        b.put_u16_le(self.movie_start_type as u16);
        b.put_u16_le(0);
        b.put_u32_le({
            let mut res: u32 = 0;
            for offset in 0..4 {
                res |= if self.controller_flags[offset].is_present {
                    1 << offset
                } else {
                    0
                };
                res |= if self.controller_flags[offset].has_mempak {
                    1 << (offset + 4)
                } else {
                    0
                };
                res |= if self.controller_flags[offset].has_rumblepack {
                    1 << (offset + 8)
                } else {
                    0
                };
            }
            res
        });
        b.put_slice(&[0; 160]);
        b.put_slice(&self.rom_internal_name_ascii);
        b.put_u32_le(self.crc32);
        b.put_u16_le(self.country_code);
        b.put_slice(&[0; 56]);
        b.put_slice(&self.video_plugin_name_ascii);
        b.put_slice(&self.sound_plugin_name_ascii);
        b.put_slice(&self.input_plugin_name_ascii);
        b.put_slice(&self.rsp_plugin_name_ascii);
        b.put_slice(&self.author_name_ascii);
        b.put_slice(&self.author_desc_ascii);
        b
    }

    #[inline]
    pub fn is_compatible_with(&self, other: &MHeader) -> Result<(), &'static str> {
        if self.fps != other.fps {
            Err("FPS is not equivalent")
        } else if self.controller_flags != other.controller_flags {
            Err("Controller data is not equivalent")
        } else {
            Ok(())
        }
    }
}

impl fmt::Display for MHeader {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "# of Frames: {}
# of Rerecords: {}
FPS: {}
# of Controllers: {}
# of Input Samples: {}
Movie Start: {}

Controller #1:
{}

Controller #2:
{}

Controller #3:
{}

Controller #4:
{}

ROM Internal Name: {}
CRC32: {}
Country Code: {}
Video Plugin Name: {}
Audio Plugin Name: {}
Input Plugin Name: {}
RSP Plugin Name: {}
Author Name: {}
Author Movie Description: {}",
            self.frame_count,
            self.rerecord_count,
            self.fps,
            self.num_controllers,
            self.input_samples,
            self.movie_start_type,
            self.controller_flags[0],
            self.controller_flags[1],
            self.controller_flags[2],
            self.controller_flags[3],
            self.rom_internal_name(),
            self.crc32,
            self.country_code,
            self.video_plugin_name(),
            self.sound_plugin_name(),
            self.input_plugin_name(),
            self.rsp_plugin_name(),
            self.author_name(),
            self.author_desc()
        )
    }
}

/// A single frame of input.
#[derive(Debug, Copy, Clone, Default, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct MFrame {
    inner: u32,
}

impl From<u32> for MFrame {
    #[inline]
    fn from(inner: u32) -> MFrame {
        Self { inner }
    }
}

macro_rules! get_bit {
    ($bf: expr, $bit: expr) => {{
        ($bf) & (1 << ($bit)) != 0
    }};
}

macro_rules! set_bit {
    ($bf: expr, $bit: expr, $value: expr) => {{
        if ($value) {
            $bf |= (1 << ($bit));
        } else {
            $bf &= !(1 << ($bit));
        }
    }};
}

macro_rules! get_byte {
    ($bf: expr, $byte: expr) => {
        (($bf) >> (($byte) * 8)) as i8
    };
}

macro_rules! set_byte {
    ($bf: expr, $byte: expr, $value: expr) => {{
        let mask: u32 = !(0xFF << ($byte * 8));
        $bf = (($value as u32) << ($byte * 8)) | ($bf & mask);
    }};
}

impl MFrame {
    #[inline]
    pub fn d_right(&self) -> bool {
        get_bit!(self.inner, 0)
    }

    #[inline]
    pub fn set_d_right(&mut self, value: bool) {
        set_bit!(self.inner, 0, value);
    }

    #[inline]
    pub fn d_left(&self) -> bool {
        get_bit!(self.inner, 1)
    }

    #[inline]
    pub fn set_d_left(&mut self, value: bool) {
        set_bit!(self.inner, 1, value);
    }

    #[inline]
    pub fn d_down(&self) -> bool {
        get_bit!(self.inner, 2)
    }

    #[inline]
    pub fn set_d_down(&mut self, value: bool) {
        set_bit!(self.inner, 2, value);
    }

    #[inline]
    pub fn d_up(&self) -> bool {
        get_bit!(self.inner, 3)
    }

    #[inline]
    pub fn set_d_up(&mut self, value: bool) {
        set_bit!(self.inner, 3, value);
    }

    #[inline]
    pub fn s(&self) -> bool {
        get_bit!(self.inner, 4)
    }

    #[inline]
    pub fn set_s(&mut self, value: bool) {
        set_bit!(self.inner, 4, value);
    }

    #[inline]
    pub fn z(&self) -> bool {
        get_bit!(self.inner, 5)
    }

    #[inline]
    pub fn set_z(&mut self, value: bool) {
        set_bit!(self.inner, 5, value);
    }

    #[inline]
    pub fn b(&self) -> bool {
        get_bit!(self.inner, 6)
    }

    #[inline]
    pub fn set_b(&mut self, value: bool) {
        set_bit!(self.inner, 6, value);
    }

    #[inline]
    pub fn a(&self) -> bool {
        get_bit!(self.inner, 7)
    }

    #[inline]
    pub fn set_a(&mut self, value: bool) {
        set_bit!(self.inner, 7, value);
    }

    #[inline]
    pub fn c_right(&self) -> bool {
        get_bit!(self.inner, 8)
    }

    #[inline]
    pub fn set_c_right(&mut self, value: bool) {
        set_bit!(self.inner, 8, value);
    }

    #[inline]
    pub fn c_left(&self) -> bool {
        get_bit!(self.inner, 9)
    }

    #[inline]
    pub fn set_c_left(&mut self, value: bool) {
        set_bit!(self.inner, 9, value);
    }

    #[inline]
    pub fn c_down(&self) -> bool {
        get_bit!(self.inner, 10)
    }

    #[inline]
    pub fn set_c_down(&mut self, value: bool) {
        set_bit!(self.inner, 10, value);
    }

    #[inline]
    pub fn c_up(&self) -> bool {
        get_bit!(self.inner, 11)
    }

    #[inline]
    pub fn set_c_up(&mut self, value: bool) {
        set_bit!(self.inner, 11, value);
    }

    #[inline]
    pub fn r(&self) -> bool {
        get_bit!(self.inner, 12)
    }

    #[inline]
    pub fn set_r(&mut self, value: bool) {
        set_bit!(self.inner, 12, value);
    }

    #[inline]
    pub fn l(&self) -> bool {
        get_bit!(self.inner, 13)
    }

    #[inline]
    pub fn set_l(&mut self, value: bool) {
        set_bit!(self.inner, 13, value);
    }

    #[inline]
    pub fn x(&self) -> i8 {
        get_byte!(self.inner, 2)
    }

    #[inline]
    pub fn set_x(&mut self, val: i8) {
        set_byte!(self.inner, 2, val);
    }

    #[inline]
    pub fn y(&self) -> i8 {
        get_byte!(self.inner, 3)
    }

    #[inline]
    pub fn set_y(&mut self, val: i8) {
        set_byte!(self.inner, 3, val);
    }
}

impl fmt::Display for MFrame {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        macro_rules! write_wpi {
            ($wpi: expr, $f: ident) => {{
                if ($wpi) {
                    write!(f, ", ")?;
                } else {
                    $wpi = true;
                }
            }};
        }

        let mut wpi: bool = false;

        if self.c_left() {
            write!(f, "C🡄 ")?;
            wpi = true;
        }
        if self.c_up() {
            write_wpi!(wpi, f);
            write!(f, "C🡅 ")?;
        }
        if self.c_right() {
            write_wpi!(wpi, f);
            write!(f, "C🡆 ")?;
        }
        if self.c_down() {
            write_wpi!(wpi, f);
            write!(f, "C🡇 ")?;
        }
        if self.r() {
            write_wpi!(wpi, f);
            write!(f, "R")?;
        }
        if self.l() {
            write_wpi!(wpi, f);
            write!(f, "L")?;
        }
        if self.s() {
            write_wpi!(wpi, f);
            write!(f, "Start")?;
        }
        if self.z() {
            write_wpi!(wpi, f);
            write!(f, "Z")?;
        }
        if self.b() {
            write_wpi!(wpi, f);
            write!(f, "B")?;
        }
        if self.a() {
            write_wpi!(wpi, f);
            write!(f, "A")?;
        }
        if self.d_left() {
            write_wpi!(wpi, f);
            write!(f, "D🡄 ")?;
        }
        if self.d_up() {
            write_wpi!(wpi, f);
            write!(f, "D🡅 ")?;
        }
        if self.d_right() {
            write_wpi!(wpi, f);
            write!(f, "D🡆 ")?;
        }
        if self.d_down() {
            write_wpi!(wpi, f);
            write!(f, "D🡇 ")?;
        }
        write_wpi!(wpi, f);
        write!(f, "[{}, {}]", self.x(), self.y())
    }
}

/// An M64 recording.
#[derive(Debug, Clone)]
pub struct MRecord {
    pub header: MHeader,
    pub frames: Vec<MFrame>,
}

impl MRecord {
    #[inline]
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, &'static str> {
        let header = MHeader::from_bytes(&bytes[0..1024])?;
        let frames = (0..header.frame_count)
            .into_iter()
            .filter_map(|f| {
                let base_index = 0x400 + (f as usize * 4);

                let frame = match catch_unwind(|| {
                    let mut frame = [0; 4];
                    frame.copy_from_slice(&bytes[base_index..base_index + 4]);
                    frame
                }) {
                    Ok(frame) => frame,
                    Err(_) => {
                        eprintln!("Failed to read frame at index {}", base_index);
                        return None;
                    }
                };

                let frame = u32::from_le_bytes(frame);
                Some(frame.into())
            })
            .collect();

        Ok(Self { header, frames })
    }

    #[inline]
    pub fn write_to<Out: Write>(&self, out: &mut Out) -> io::Result<()> {
        // header
        let header = self.header.to_bytes();
        let header_slice = IoSlice::new(&header);
        // SAFETY: no
        let body = self.frames.as_ptr() as *const u8;
        let body = ptr::slice_from_raw_parts(body, self.frames.len() * 4);
        let body = IoSlice::new(unsafe { &*body });

        out.write_vectored(&[header_slice, body])?;
        Ok(())
    }
}
