//! The Instrument class holds all events for a single instrument and contains
//! functions for extracting information from the events it contains.

use super::containers::{ControlChange, Note, PitchBend};
use serde::{Deserialize, Serialize};
use std::fmt::{Debug, Formatter};

/// Object to hold event information for a single instrument.
#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct Instrument {
    /// MIDI program number (instrument index), in ``[0, 127]``.
    pub program: u8,
    /// Is the instrument a drum instrument (channel 9)?
    pub is_drum: bool,
    /// Name of the instrument.
    pub name: String,
    /// Vector of `pretty_midi::Note` objects.
    pub notes: Vec<Note>,
    /// Vector of `pretty_midi::PitchBend` objects.
    pub pitch_bends: Vec<PitchBend>,
    /// Vector of `pretty_midi::ControlChange` objects.
    pub control_changes: Vec<ControlChange>,
}

impl Instrument {
    /// Create the Instrument.
    ///
    /// # Arguments
    ///
    /// * `program` - MIDI program number (instrument index), in ``[0, 127]``.
    /// * `is_drum` - Is the instrument a drum instrument (channel 9)?
    /// * `name` - Name of the instrument.
    pub fn new(program: u8, is_drum: bool, name: impl Into<String>) -> Self {
        Instrument {
            program,
            is_drum,
            name: name.into(),
            notes: vec![],
            pitch_bends: vec![],
            control_changes: vec![],
        }
    }

    /// Returns the time of the end of the events in this instrument.
    ///
    /// # Arguments
    ///
    /// * `end_time` - Time, in seconds, of the last event.
    pub fn get_end_time(&self) -> f64 {
        // Cycle through all note ends and all pitch bends and find the largest.
        // If there are no events, just return 0.0.
        let mut end_time = self.notes.iter().fold(0.0, |m, n| n.end.max(m));
        end_time = self.pitch_bends.iter().fold(end_time, |m, b| b.time.max(m));
        end_time = self
            .control_changes
            .iter()
            .fold(end_time, |m, c| c.time.max(m));
        end_time
    }

    pub fn remove_invalid_notes(&mut self) {
        // Crete a list of all invalid notes.
        let mut notes_to_delete = vec![];
        for note in self.notes.clone() {
            if note.end <= note.start {
                notes_to_delete.push(note);
            }
        }
        // Remove the notes found.
        for note in notes_to_delete.iter() {
            if let Some(pos) = notes_to_delete.iter().position(|x| *x == *note) {
                self.notes.remove(pos);
            }
        }
    }
}

impl Debug for Instrument {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Instrument {} program: {}, is_drum: {}, name: \"{}\" {}",
            "{", self.program, self.is_drum, self.name, "}"
        )
    }
}
