//! Utility functions for converting between MIDI data and human-readable/usable
//! values.

use super::{constants::MIDI_KEY_SIGNATURE_TO_KEY_NUMBER_MAP, error::Error::ValueError};
use anyhow::Result;
use midly::TrackEventKind;

pub(crate) type CustomTrack<'a> = Vec<CustomTrackEvent<'a>>;

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub(crate) struct CustomTrackEvent<'a> {
    pub track_idx: usize,
    pub tick: usize,
    pub kind: TrackEventKind<'a>,
}

/// Convert a key number to a key string.
///
/// Returns key name in the format ``"(root) (mode)"``, e.g. ``"Gb minor"``.
/// Gives preference for keys with flats, with the exception of F#, G# and
/// C# minor.
///
/// # Arguments
///
/// * `key_number` - Uses pitch classes to represent major and minor keys.
///                  For minor keys, adds a 12 offset.
///                  For example, C major is 0 and C minor is 12.
pub fn key_number_to_key_name(key_number: u8) -> Result<String> {
    if key_number >= 24 {
        Err(ValueError(
            "`key_number` is larger than or equal to 24".to_string(),
        ))?
    }

    // preference to keys with flats
    let keys = [
        "C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B",
    ];

    // circle around 12 pitch classes
    let key_idx = (key_number % 12) as usize;
    let mode = key_number / 12;

    // check if mode is major or minor
    if mode == 0 {
        Ok(format!("{}{}", keys[key_idx], " Major").to_string())
    } else {
        // preference to C#, F# and G# minor
        if [1, 6, 8].contains(&key_idx) {
            Ok(format!("{}{}", keys[key_idx - 1], "# minor").to_string())
        } else {
            Ok(format!("{}{}", keys[key_idx], " minor").to_string())
        }
    }
}

/// Convert a MIDI key signature to a key number.
///
/// Key number uses pitch classes to represent major and minor keys.
/// For minor keys, adds a 12 offset. For example, C major is 0 and C minor is 12.
///
/// # Arguments
///
/// * `key` - The number of flats or sharps.
///           If positive integer, it's the number of sharps,
///           and negative integer is the number of flats.
/// * `scale` - Major scale or minor scale. If `true`, it's major.
pub fn midi_key_signature_to_key_number(key: i8, scale: bool) -> u8 {
    *(MIDI_KEY_SIGNATURE_TO_KEY_NUMBER_MAP
        .get(&(key, scale))
        .unwrap())
}
