//! These structs simply hold MIDI data in a convenient form.

use super::{error::Error::ValueError, utilities::key_number_to_key_name};
use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::fmt::{Debug, Display, Formatter};

/// A note event.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Note {
    /// Note velocity.
    pub velocity: u8,
    /// Note pitch, as a MIDI note number.
    pub pitch: u8,
    /// Note on time, absolute, in seconds.
    pub start: f64,
    /// Note off time, absolute, in seconds.
    pub end: f64,
}

impl Note {
    pub fn new(velocity: u8, pitch: u8, start: f64, end: f64) -> Self {
        Note {
            velocity,
            pitch,
            start,
            end,
        }
    }

    /// Get the duration of the note in seconds.
    pub fn get_duration(&self) -> f64 {
        self.end - self.start
    }
}

/// A pitch bend event.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct PitchBend {
    /// MIDI pitch bend amount, in the range ``[-8192, 8191]``.
    pub pitch: i16,
    /// Time where the pitch bend occurs.
    pub time: f64,
}

impl PitchBend {
    pub fn new(pitch: i16, time: f64) -> Self {
        PitchBend { pitch, time }
    }
}

/// A control change event.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ControlChange {
    /// The control change number, in ``[0, 127]``.
    pub number: u8,
    /// The value of the control change, in ``[0, 127]``.
    pub value: u8,
    /// Time where the control change occurs.
    pub time: f64,
}

impl ControlChange {
    pub fn new(number: u8, value: u8, time: f64) -> Self {
        ControlChange {
            number,
            value,
            time,
        }
    }
}

/// Container for a Time Signature event, which contains the time signature
/// numerator, denominator and the event time in seconds.
///
/// # Examples
///
/// * Instantiate a TimeSignature object with 6/8 time signature at 3.14 seconds:
/// ```
/// use pretty_midi::TimeSignature;
/// let ts = TimeSignature::new(6, 8, 3.14).unwrap();
/// assert_eq!("6/8 at 3.14 seconds".to_string(), ts.to_string());
/// ```
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TimeSignature {
    /// Numerator of time signature.
    pub numerator: u8,
    /// Denominator of time signature.
    pub denominator: u64,
    /// Time of event in seconds.
    pub time: f64,
}

impl TimeSignature {
    pub fn new(numerator: u8, denominator: u64, time: f64) -> Result<Self> {
        if numerator == 0 {
            return Err(
                ValueError(format!("{} is not a valid `numerator` value", numerator)).into(),
            );
        }
        if denominator == 0 {
            return Err(ValueError(format!(
                "{} is not a valid `denominator` value",
                denominator
            ))
            .into());
        }
        if time < 0.0 {
            return Err(ValueError(format!("{} is not a valid `time` value", time)).into());
        }

        Ok(TimeSignature {
            numerator,
            denominator,
            time,
        })
    }
}

impl Display for TimeSignature {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}/{} at {:.2} seconds",
            self.numerator, self.denominator, self.time
        )
    }
}

/// Contains the key signature and the event time in seconds.
/// Only supports major and minor keys.
///
/// # Examples
///
/// * Instantiate a C# minor KeySignature object at 3.14 seconds:
/// ```
/// use pretty_midi::KeySignature;
/// let ks = KeySignature::new(13, 3.14).unwrap();
/// assert_eq!("C# minor at 3.14 seconds".to_string(), ks.to_string());
/// ```
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct KeySignature {
    /// Key number according to ``[0, 11]`` Major, ``[12, 23]`` minor.
    /// For example, 0 is C Major, 12 is C minor.
    pub key_number: u8,
    /// Time of event in seconds.
    pub time: f64,
}

impl KeySignature {
    pub fn new(key_number: u8, time: f64) -> Result<Self> {
        if key_number >= 24 {
            return Err(ValueError(format!(
                "{} is not a valid `key_number` type or value",
                key_number
            ))
            .into());
        }
        if time < 0.0 {
            return Err(ValueError(format!("{} is not a valid `time` type or value", time)).into());
        }

        Ok(KeySignature { key_number, time })
    }
}

impl Display for KeySignature {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "{} at {:.2} seconds",
            key_number_to_key_name(self.key_number).unwrap(),
            self.time
        )
    }
}

/// Timestamped lyric text.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Lyric {
    /// The text of the lyric.
    pub text: String,
    /// The time in seconds of the lyric.
    pub time: f64,
}

impl Lyric {
    pub fn new(text: impl Into<String>, time: f64) -> Self {
        Lyric {
            text: text.into(),
            time,
        }
    }
}

impl Display for Lyric {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{} at {:.2} seconds", self.text, self.time)
    }
}

/// Timestamped text text.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Text {
    /// The text.
    pub text: String,
    /// The time it occurs in seconds.
    pub time: f64,
}

impl Text {
    pub fn new(text: impl Into<String>, time: f64) -> Self {
        Text {
            text: text.into(),
            time,
        }
    }
}

impl Display for Text {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{} at {:.2} seconds", self.text, self.time)
    }
}
