//! Utility functions for handling MIDI data in an easy to read/manipulate
//! format.

use super::{
    constants::KEY_NUMBER_TO_MIDI_KEY_SIGNATURE_MAP,
    containers::{ControlChange, KeySignature, Lyric, Note, PitchBend, Text, TimeSignature},
    error::Error::{IndexError, ValueError},
    instrument::Instrument,
    utilities::{midi_key_signature_to_key_number, CustomTrack, CustomTrackEvent},
};
use anyhow::Result;
use midly::{
    num::{u15, u24, u28, u4, u7},
    Format, Header, MetaMessage, MidiMessage, PitchBend as PitchBendEvent, Smf, Timing, TrackEvent,
    TrackEventKind,
};
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::{
    cell::{Ref, RefCell, RefMut},
    cmp::Ordering,
    fmt::{Debug, Formatter},
    fs,
    io::Write,
    path::Path,
    str,
};

// The largest we'd ever expect a tick to be.
const MAX_TICK: usize = 10usize.pow(7);
// MIDI clocks per click.
const CPC: u8 = 24;
// 32nd notes per quarter.
const TPQ: u8 = 8;

type PrettyMIDIEvents = (
    Vec<KeySignature>,
    Vec<TimeSignature>,
    Vec<Lyric>,
    Vec<Text>,
    Vec<Instrument>,
);

/// A container for MIDI data in an easily-manipulable format.
#[derive(Clone, PartialEq, Serialize, Deserialize)]
pub struct PrettyMIDI {
    /// Resolution of the MIDI data.
    pub resolution: u16,
    initial_tempo: f64,
    /// Vector of `pretty_midi.Instrument` objects.
    pub instruments: Vec<Instrument>,
    /// Vector of `pretty_midi.KeySignature` objects.
    pub key_signature_changes: Vec<KeySignature>,
    /// Vector of `pretty_midi.TimeSignature` objects.
    pub time_signature_changes: Vec<TimeSignature>,
    /// Vector of `pretty_midi.Lyric` objects.
    pub lyrics: Vec<Lyric>,
    /// Vector of `pretty_midi.Text` objects.
    pub text_events: Vec<Text>,
    _tick_scales: RefCell<Vec<(usize, f64)>>,
    __tick_to_time: RefCell<Vec<f64>>,
}

impl PrettyMIDI {
    /// Initialize from scratch with no data.
    ///
    /// # Arguments
    ///
    /// * `resolution` - Resolution of the MIDI data.
    /// * `initial_tempo` - Initial tempo for the MIDI data, when no file is provided.
    pub fn new(resolution: u16, initial_tempo: f64) -> Self {
        // Compute the tick scale for the provided initial tempo
        // and let the tick scale start from 0.
        let _tick_scales = vec![(0, 60.0 / (initial_tempo * resolution as f64))];

        PrettyMIDI {
            resolution,
            initial_tempo,
            _tick_scales: RefCell::new(_tick_scales),
            ..Default::default()
        }
    }

    /// Initialize by populating it with MIDI bytes.
    ///
    /// # Arguments
    ///
    /// * `bytes` - MIDI bytes.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self> {
        // Load in the MIDI data using the midi module.
        let midi_data = Smf::parse(bytes).unwrap();

        // Compute tick values in midi_data to absolute, a useful thing.
        let mut abs_tick_tracks = vec![];
        for (track_idx, track) in midi_data.tracks.iter().enumerate() {
            let mut tick = 0;
            let mut abs_tick_track = vec![];
            for event in track.iter() {
                tick += event.delta.as_int() as usize;
                abs_tick_track.push(CustomTrackEvent {
                    track_idx,
                    tick,
                    kind: event.kind,
                })
            }
            abs_tick_tracks.push(abs_tick_track);
        }

        // Store the resolution for later use.
        let resolution = match midi_data.header.timing {
            Timing::Metrical(t) => t.as_int(),
            _ => panic!("It's an unpredictable process"),
        };

        // Populate the list of tempo changes (tick scales).
        let _tick_scales = _load_tempo_changes(&abs_tick_tracks, resolution);
        let initial_tempo = _tick_scales[0].1;

        // Update the array which maps ticks to time.
        let max_tick = abs_tick_tracks.iter().fold(0, |m, t| {
            if let Some(e) = t.last() { e.tick } else { 0 }.max(m)
        });

        // If max_tick is huge, the MIDI file is probably corrupt
        // and creating the __tick_to_time array will thrash memory.
        if max_tick > MAX_TICK {
            return Err(ValueError(format!(
                "MIDI file has a largest tick of {}, it is likely corrupt",
                max_tick
            ))
            .into());
        }

        // Create list that maps ticks to time in seconds.
        let __tick_to_time = _create_tick_to_time(max_tick, &_tick_scales);

        // Populate the list of key and time signature changes.
        // let (key_signature_changes, time_signature_changes, lyrics, text_events) =
        //     _load_metadata(&abs_tick_tracks, &__tick_to_time)?;

        // Check that there are tempo, key and time change events
        // only on track 0.
        for tracks in midi_data.tracks.iter().skip(1) {
            for event in tracks.iter() {
                if let TrackEventKind::Meta(MetaMessage::Tempo(..))
                | TrackEventKind::Meta(MetaMessage::KeySignature(..))
                | TrackEventKind::Meta(MetaMessage::TimeSignature(..)) = event.kind
                {
                    log::warn!(
                        "Tempo, Key or Time signature change events found on \
                        non-zero tracks. This is not a valid type 0 or type 1 \
                        MIDI file. Tempo, Key or Time Signature may be wrong."
                    )
                }
            }
        }

        // Populate the list of key and time signature changes and the list of instruments.
        let (key_signature_changes, time_signature_changes, lyrics, text_events, instruments) =
            _load_events(&abs_tick_tracks, &__tick_to_time)?;

        Ok(PrettyMIDI {
            resolution,
            initial_tempo,
            instruments,
            key_signature_changes,
            time_signature_changes,
            lyrics,
            text_events,
            _tick_scales: RefCell::new(_tick_scales),
            __tick_to_time: RefCell::new(__tick_to_time),
        })
    }

    /// Initialize by populating it with MIDI data from a file.
    ///
    /// # Arguments
    ///
    /// * `midi_file` - Path to a MIDI file.
    pub fn from_file<P: AsRef<Path>>(midi_file: P) -> Result<Self> {
        let bytes = fs::read(midi_file).unwrap();
        PrettyMIDI::from_bytes(&bytes)
    }

    /// Creates ``self.__tick_to_time``, a class member array which maps
    /// ticks to time starting from tick 0 and ending at ``max_tick``.
    ///
    /// # Arguments
    ///
    /// * `max_tick` - Last tick to compute time for.  If ``self._tick_scales`` contains a
    ///                tick which is larger than this value, it will be used instead.
    pub fn _update_tick_to_time(&self, max_tick: usize) {
        // If max_tick is smaller than the largest tick in tick_scales,
        // use this largest tick instead.
        let max_scale_tick = self
            ._tick_scales
            .borrow()
            .iter()
            .map(|ts| ts.0)
            .max()
            .unwrap();
        let max_tick = max_tick.max(max_scale_tick);
        // Allocate tick to time array - indexed by tick from 0 to max_tick.
        *(self.__tick_to_time.borrow_mut()) = vec![0.; max_tick + 1];
        // Keep track of the end time of the last tick in the previous interval
        let mut last_end_time = 0.;
        // Cycle through intervals of different tempi.
        for i in 0..self._tick_scales.borrow().len() - 1 {
            let (start_tick, tick_scale) = self._tick_scales.borrow()[i];
            let (end_tick, _) = self._tick_scales.borrow()[i + 1];
            // Convert ticks in this interval to times
            for (j, tick) in (start_tick..end_tick - start_tick + 1).enumerate() {
                self.__tick_to_time.borrow_mut()[start_tick + j] =
                    last_end_time + tick_scale * tick as f64;
            }
            // Update the time of the last tick in this interval
            last_end_time = self.__tick_to_time.borrow()[end_tick];
        }
        // For the final interval, use the final tempo setting
        // and ticks from the final tempo setting until max_tick
        let (start_tick, tick_scale) = *self._tick_scales.borrow().last().unwrap();
        for (j, tick) in (0..max_tick + 1 - start_tick).enumerate() {
            self.__tick_to_time.borrow_mut()[start_tick + j] =
                last_end_time + tick_scale * tick as f64;
        }
    }

    /// Return arrays of tempo changes in quarter notes-per-minute and their
    /// times.
    ///
    /// # Returns
    ///
    /// `tempo_change_times` - Times, in seconds, where the tempo changes.
    /// `tempi` - What the tempo is, in quarter notes-per-minute, at each point in
    ///           time in ``tempo_change_times``.
    pub fn get_tempo_changes(&self) -> (Vec<f64>, Vec<f64>) {
        // Pre-allocate return arrays.
        let mut tempo_change_times = vec![0.0; self._tick_scales.borrow().len()];
        let mut tempi = vec![0.0; self._tick_scales.borrow().len()];
        for (n, (tick, tick_scale)) in self._tick_scales.borrow().iter().enumerate() {
            // Convert tick of this tempo change to time in seconds.
            tempo_change_times[n] = self.tick_to_time(*tick).unwrap();
            // Convert tick scale to a tempo.
            tempi[n] = 60.0 / (*tick_scale * self.resolution as f64);
        }
        (tempo_change_times, tempi)
    }

    /// Converts from an absolute tick to time in seconds using
    /// ``self.__tick_to_time``.
    ///
    /// # Arguments
    ///
    /// * `tick` - Absolute tick to convert.
    pub fn tick_to_time(&self, tick: usize) -> Result<f64> {
        // Check that the tick isn't too big.
        if tick >= MAX_TICK {
            return Err(IndexError("Supplied tick is too large.".to_string()).into());
        }
        // If we haven't compute the mapping for a tick this large, compute it.
        if tick >= self.__tick_to_time.borrow().len() {
            self._update_tick_to_time(tick)
        }
        // Otherwise just return the time.
        Ok(self.__tick_to_time.borrow()[tick])
    }

    /// Converts from a time in seconds to absolute tick using
    /// ``self._tick_scales``.
    ///
    /// # Arguments
    ///
    /// * `time` - Time, in seconds.
    pub fn time_to_tick(&self, time: f64) -> usize {
        // Find the index of the ticktime which is smaller than time.
        let mut tick = self.__tick_to_time.borrow().partition_point(|&x| x < time);
        // If the closest tick was the final tick in self.__tick_to_time...
        if tick == self.__tick_to_time.borrow().len() {
            // start from time at end of __tick_to_time.
            tick -= 1;
            // Add on ticks assuming the final tick_scale amount.
            let (_, final_tick_scale) = *self._tick_scales.borrow().last().unwrap();
            let tick = tick as f64 + (time - self.__tick_to_time.borrow()[tick]) / final_tick_scale;
            // Re-round/quantize.
            return tick.round() as usize;
        }
        // If the tick is not 0 and the previous ticktime in a is closer to time.
        if tick > 0
            && (time - self.__tick_to_time.borrow()[tick - 1]).abs()
                < (time - self.__tick_to_time.borrow()[tick]).abs()
        {
            // Decrement index by 1.
            tick - 1
        } else {
            tick
        }
    }

    /// Helper function to write the MIDI data out.
    fn _write(&self) -> Smf {
        /// Compares two events for sorting.
        ///
        /// # Arguments
        ///
        /// * `event1`, `event2` - Two events to be compared.
        fn event_compare(event1: &CustomTrackEvent, event2: &CustomTrackEvent) -> Ordering {
            // Construct a dictionary which will map event names to numeric
            // values which produce the correct sorting.  Each dictionary value
            // is a function which accepts an event and returns a score.
            // The spacing for these scores is 256, which is larger than the
            // largest value a MIDI value can take.
            let secondary_sort = |e: &CustomTrackEvent| -> Option<i32> {
                match e.kind {
                    TrackEventKind::Meta(meta) => match meta {
                        MetaMessage::Tempo(..) => Some(256 * 256),
                        MetaMessage::TimeSignature(..) => Some(2 * 256 * 256),
                        MetaMessage::KeySignature(..) => Some(3 * 256 * 256),
                        MetaMessage::Lyric(..) => Some(4 * 256 * 256),
                        MetaMessage::Text(..) => Some(5 * 256 * 256),
                        _ => None,
                    },
                    TrackEventKind::Midi { message, .. } => match message {
                        MidiMessage::ProgramChange { .. } => Some(6 * 256 * 256),
                        MidiMessage::PitchBend { bend } => {
                            Some((7 * 256 * 256) + bend.as_int() as i32)
                        }
                        MidiMessage::Controller { controller, value } => Some(
                            (8 * 256 * 256)
                                + (controller.as_int() as i32 * 256)
                                + value.as_int() as i32,
                        ),
                        MidiMessage::NoteOff { key, .. } => {
                            Some((9 * 256 * 256) + (key.as_int() as i32 * 256))
                        }
                        MidiMessage::NoteOn { key, vel } => Some(
                            (10 * 256 * 256) + (key.as_int() as i32 * 256) + vel.as_int() as i32,
                        ),
                        _ => None,
                    },
                    _ => None,
                }
            };
            // If the events have the same tick, and both events have types
            // which appear in the secondary_sort dictionary, use the dictionary
            // to determine their ordering.
            if event1.tick == event2.tick {
                if let (Some(s1), Some(s2)) = (secondary_sort(event1), secondary_sort(event2)) {
                    return s1.cmp(&s2);
                }
            }
            // Otherwise, just return the difference of their ticks.
            event1.tick.cmp(&event2.tick)
        }

        // Initialize output MIDI object.
        let mut mid = Smf::new(Header::new(
            Format::Parallel,
            Timing::Metrical(u15::new(self.resolution)),
        ));
        let mut tracks = vec![];
        // Create track 0 with timing information.
        let mut timing_track = CustomTrack::new();
        // Add tempo change event with default values.
        let mut add_ts = true;
        if !self.time_signature_changes.is_empty() {
            add_ts = self
                .time_signature_changes
                .iter()
                .fold(f64::MAX, |m, ts| ts.time.min(m))
                > 0.0;
        }
        if add_ts {
            // Add time signature event with default values (4/4).
            timing_track.push(CustomTrackEvent {
                track_idx: 0, // It's not necessary for this process.
                tick: 0,
                kind: TrackEventKind::Meta(MetaMessage::TimeSignature(4, 2, CPC, TPQ)),
            });
        }

        // Add in each tempo change event.
        for (tick, tick_scale) in self._tick_scales.borrow().iter() {
            timing_track.push(CustomTrackEvent {
                track_idx: 0, // It's not necessary for this process.
                tick: *tick,
                kind: TrackEventKind::Meta(MetaMessage::Tempo(u24::new(
                    // Convert from microseconds per quarter note to BPM.
                    (6e7 / (60.0 / (tick_scale * self.resolution as f64))) as u32,
                ))),
            });
        }
        // Add in each time signature.
        for ts in self.time_signature_changes.iter() {
            timing_track.push(CustomTrackEvent {
                track_idx: 0, // It's not necessary for this process.
                tick: self.time_to_tick(ts.time),
                kind: TrackEventKind::Meta(MetaMessage::TimeSignature(
                    ts.numerator as u8,
                    (ts.denominator as f64).sqrt() as u8,
                    CPC,
                    TPQ,
                )),
            });
        }
        // Add in each key signature.
        for ks in self.key_signature_changes.iter() {
            let (key, scale) = KEY_NUMBER_TO_MIDI_KEY_SIGNATURE_MAP
                .get(&ks.key_number)
                .unwrap();
            timing_track.push(CustomTrackEvent {
                track_idx: 0, // It's not necessary for this process.
                tick: self.time_to_tick(ks.time),
                kind: TrackEventKind::Meta(MetaMessage::KeySignature(*key, *scale)),
            });
        }
        // Add in all lyrics events.
        for l in self.lyrics.iter() {
            timing_track.push(CustomTrackEvent {
                track_idx: 0, // It's not necessary for this process.
                tick: self.time_to_tick(l.time),
                kind: TrackEventKind::Meta(MetaMessage::Lyric(l.text.as_bytes())),
            });
        }
        // Add text events.
        for t in self.text_events.iter() {
            timing_track.push(CustomTrackEvent {
                track_idx: 0, // It's not necessary for this process.
                tick: self.time_to_tick(t.time),
                kind: TrackEventKind::Meta(MetaMessage::Text(t.text.as_bytes())),
            });
        }
        // Sort the (absolute-tick-timed) events.
        timing_track.sort_by(event_compare);
        // Add in an end of track event.
        timing_track.push(CustomTrackEvent {
            track_idx: 0, // It's not necessary for this process.
            tick: timing_track[timing_track.len() - 1].tick + 1,
            kind: TrackEventKind::Meta(MetaMessage::EndOfTrack),
        });
        tracks.push(timing_track);
        // Create a list of possible channels to assign - this seems to matter
        // for some synths.
        // Don't assign the drum channel by mistake!
        let channels: Vec<u8> = (0..16).filter(|&c| c != 9).collect();
        for (n, instrument) in self.instruments.iter().enumerate() {
            // Initialize track for this instrument.
            let mut track = CustomTrack::new();
            // Add track name event if instrument has a name.
            if !instrument.name.is_empty() {
                track.push(CustomTrackEvent {
                    track_idx: 0, // It's not necessary for this process.
                    tick: 0,
                    kind: TrackEventKind::Meta(MetaMessage::TrackName(instrument.name.as_bytes())),
                });
            }
            // If it's a drum event, we need to set channel to 9.
            // Otherwise, choose a channel from the possible channel list.
            let channel = u4::new(if instrument.is_drum {
                9
            } else {
                channels[n % channels.len()]
            });
            // Set the program number.
            track.push(CustomTrackEvent {
                track_idx: 0, // It's not necessary for this process.
                tick: 0,
                kind: TrackEventKind::Midi {
                    channel,
                    message: MidiMessage::ProgramChange {
                        program: u7::new(instrument.program as u8),
                    },
                },
            });
            // Add all note events.
            for note in instrument.notes.iter() {
                // Construct the note-on event.
                track.push(CustomTrackEvent {
                    track_idx: 0, // It's not necessary for this process.
                    tick: self.time_to_tick(note.start),
                    kind: TrackEventKind::Midi {
                        channel,
                        message: MidiMessage::NoteOn {
                            key: u7::new(note.pitch as u8),
                            vel: u7::new(note.velocity as u8),
                        },
                    },
                });
                // Also need a note-off event (note on with velocity 0).
                track.push(CustomTrackEvent {
                    track_idx: 0, // It's not necessary for this process.
                    tick: self.time_to_tick(note.end),
                    kind: TrackEventKind::Midi {
                        channel,
                        message: MidiMessage::NoteOn {
                            key: u7::new(note.pitch as u8),
                            vel: u7::new(0),
                        },
                    },
                });
            }
            // Add all pitch bend events.
            for bend in instrument.pitch_bends.iter() {
                track.push(CustomTrackEvent {
                    track_idx: 0, // It's not necessary for this process.
                    tick: self.time_to_tick(bend.time),
                    kind: TrackEventKind::Midi {
                        channel,
                        message: MidiMessage::PitchBend {
                            bend: PitchBendEvent::from_int(bend.pitch as i16),
                        },
                    },
                });
            }
            // Add all control change events.
            for control_change in instrument.control_changes.iter() {
                track.push(CustomTrackEvent {
                    track_idx: 0, // It's not necessary for this process.
                    tick: self.time_to_tick(control_change.time),
                    kind: TrackEventKind::Midi {
                        channel,
                        message: MidiMessage::Controller {
                            controller: u7::new(control_change.number as u8),
                            value: u7::new(control_change.value as u8),
                        },
                    },
                });
            }
            // Sort all the events using the event_compare comparator.
            track.sort_by(event_compare);

            // If there's a note off event and a note on event with the same
            // tick and pitch, put the note off event first.
            for m in 0..track.len() - 1 {
                if track[m].tick == track[m + 1].tick {
                    if let (
                        TrackEventKind::Midi {
                            message:
                                MidiMessage::NoteOn {
                                    key: key1,
                                    vel: vel1,
                                },
                            ..
                        },
                        TrackEventKind::Midi {
                            message:
                                MidiMessage::NoteOn {
                                    key: key2,
                                    vel: vel2,
                                },
                            ..
                        },
                    ) = (track[m].kind, track[m + 1].kind)
                    {
                        if key1 == key2 && vel1.as_int() != 0 && vel2.as_int() == 0 {
                            track.swap(m, m + 1);
                        }
                    }
                }
            }
            // Finally, add in an end of track event.
            track.push(CustomTrackEvent {
                track_idx: 0, // It's not necessary for this process.
                tick: track[track.len() - 1].tick + 1,
                kind: TrackEventKind::Meta(MetaMessage::EndOfTrack),
            });
            // Add to the list of output tracks.
            tracks.push(track);
        }
        // Turn ticks to relative time from absolute.
        for abs_tick_track in tracks.iter() {
            let mut track = vec![];
            let mut previous_tick = 0;
            for abs_tick_event in abs_tick_track.iter() {
                track.push(TrackEvent {
                    delta: u28::new((abs_tick_event.tick - previous_tick) as u32),
                    kind: abs_tick_event.kind,
                });
                previous_tick = abs_tick_event.tick;
            }
            mid.tracks.push(track);
        }

        mid
    }

    /// Write the MIDI data out to a .mid file.
    ///
    /// # Arguments
    ///
    /// * `filename` - Path to write .mid file to.
    pub fn write<P: AsRef<Path>>(&self, filename: P) {
        let mid = self._write();

        // Write it out.
        mid.save(filename).unwrap();
    }

    /// Write the MIDI data out to a `std::io::Write` writer.
    ///
    /// # Arguments
    ///
    /// * `writer` - `std::io::Write` writer.
    pub fn write_to_writer<W: Write>(&self, writer: W) {
        let mid = self._write();

        // Write it out.
        mid.write_std(writer).unwrap();
    }

    pub fn get_tick_scales(&self) -> Ref<Vec<(usize, f64)>> {
        self._tick_scales.borrow()
    }

    pub fn get_tick_scales_mut(&self) -> RefMut<Vec<(usize, f64)>> {
        self._tick_scales.borrow_mut()
    }

    pub fn get_tick_to_time(&self) -> Ref<Vec<f64>> {
        self.__tick_to_time.borrow()
    }

    pub fn get_tick_to_time_mut(&self) -> RefMut<Vec<f64>> {
        self.__tick_to_time.borrow_mut()
    }
}

impl Default for PrettyMIDI {
    fn default() -> Self {
        PrettyMIDI {
            resolution: 220,
            initial_tempo: 120.0,
            instruments: vec![],
            key_signature_changes: vec![],
            time_signature_changes: vec![],
            lyrics: vec![],
            text_events: vec![],
            _tick_scales: RefCell::new(vec![(0, 60.0 / (120.0 * 220.0))]),
            __tick_to_time: RefCell::new(vec![0.0]),
        }
    }
}

impl Debug for PrettyMIDI {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "PrettyMIDI {{ resolution: {}, instruments: {:?}, key_signature_changes: {:?},\
            time_signature_changes: {:?}, lyrics: {:?}, text_events: {:?} }}",
            self.resolution,
            self.instruments,
            self.key_signature_changes,
            self.time_signature_changes,
            self.lyrics,
            self.text_events,
        )
    }
}

/// Populates ``self._tick_scales`` with tuples of
/// ``(tick, tick_scale)`` loaded from ``midi_data``.
///
/// # Arguments
///
/// * `midi_data` - MIDI object from which data will be read.
fn _load_tempo_changes(tracks: &[CustomTrack], resolution: u16) -> Vec<(usize, f64)> {
    let resolution = resolution as f64;
    // MIDI data is given in "ticks".
    // We need to convert this to clock seconds.
    // The conversion factor involves the BPM, which may change over time.
    // So, create a list of tuples, (time, tempo)
    // denoting a tempo change at a certain time.
    // By default, set the tempo to 120 bpm, starting at time 0
    let mut _tick_scales = vec![(0, 60.0 / (120.0 * resolution))];

    for event in tracks.iter().flatten() {
        if let TrackEventKind::Meta(MetaMessage::Tempo(tempo)) = event.kind {
            let tempo = tempo.as_int() as f64;
            if event.tick == 0 {
                let bpm = 6e7 / tempo;
                _tick_scales = vec![(0, 60.0 / (bpm * resolution))]
            } else {
                // Get time and BPM up to this point
                let (_, last_tick_scale) = _tick_scales.last().unwrap();
                let tick_scale = 60.0 / ((6e7 / tempo) * resolution);
                // Ignore repetition of BPM, which happens often
                if tick_scale != *last_tick_scale {
                    _tick_scales.push((event.tick, tick_scale))
                }
            }
        }
    }

    _tick_scales.sort_by(|a, b| a.0.cmp(&b.0));
    _tick_scales
}

/// Creates ``self.__tick_to_time``, a class member array which maps
/// ticks to time starting from tick 0 and ending at ``max_tick``.
///
/// # Arguments
///
/// * `max_tick` - Last tick to compute time for.  If ``self._tick_scales`` contains a
///                tick which is larger than this value, it will be used instead.
fn _create_tick_to_time(max_tick: usize, _tick_scales: &[(usize, f64)]) -> Vec<f64> {
    // If max_tick is smaller than the largest tick in tick_scales,
    // use this largest tick instead.
    assert!(!_tick_scales.is_empty());
    let max_scale_tick = _tick_scales.iter().map(|ts| ts.0).max().unwrap();
    let max_tick = max_tick.max(max_scale_tick);
    // Allocate tick to time array - indexed by tick from 0 to max_tick.
    let mut __tick_to_time = vec![0.0; max_tick + 1];
    // Keep track of the end time of the last tick in the previous interval
    let mut last_end_time = 0.0;
    // Cycle through intervals of different tempi.
    for i in 0.._tick_scales.len() - 1 {
        let (start_tick, tick_scale) = _tick_scales[i];
        let (end_tick, _) = _tick_scales[i + 1];
        // Convert ticks in this interval to times
        for (j, tick) in (start_tick..end_tick - start_tick + 1).enumerate() {
            __tick_to_time[start_tick + j] = last_end_time + tick_scale * tick as f64;
        }
        // Update the time of the last tick in this interval
        last_end_time = __tick_to_time[end_tick];
    }
    // For the final interval, use the final tempo setting
    // and ticks from the final tempo setting until max_tick
    let (start_tick, tick_scale) = _tick_scales.last().unwrap();
    for (j, tick) in (0..max_tick + 1 - *start_tick).enumerate() {
        __tick_to_time[*start_tick + j] = last_end_time + tick_scale * tick as f64;
    }
    __tick_to_time
}

/// Populates ``time_signature_changes``, ``key_signature_changes``, ``lyrics``,
/// ``text_events`` and ``instruments``.
///
/// # Arguments
///
/// * `tracks` - MIDI tracks from which data will be read.
/// * `__tick_to_time` - TODO
fn _load_events(tracks: &[CustomTrack], __tick_to_time: &[f64]) -> Result<PrettyMIDIEvents> {
    let mut key_signature_changes = vec![];
    let mut time_signature_changes = vec![];
    let mut lyrics = vec![];
    let mut text_events = vec![];

    let mut notes_map: FxHashMap<(u8, u8, usize), Vec<Note>> = FxHashMap::default();
    let mut pitch_bends_map: FxHashMap<(u8, usize), Vec<PitchBend>> = FxHashMap::default();
    let mut control_changes_map: FxHashMap<(u8, usize), Vec<ControlChange>> = FxHashMap::default();
    let mut track_name_map: FxHashMap<usize, &[u8]> = FxHashMap::default();
    let mut last_note_on: FxHashMap<(usize, u8, u8), Vec<(usize, u8)>> = FxHashMap::default();
    let mut current_instrument = [0; 16];

    for event in tracks.iter().flatten() {
        match event.kind {
            TrackEventKind::Meta(MetaMessage::KeySignature(key, scale)) => {
                let key_obj = KeySignature::new(
                    midi_key_signature_to_key_number(key, scale),
                    __tick_to_time[event.tick],
                )?;
                key_signature_changes.push(key_obj);
            }
            TrackEventKind::Meta(MetaMessage::TimeSignature(numerator, denominator, ..)) => {
                let ts_obj = TimeSignature::new(
                    numerator,
                    2u64.pow(denominator.into()), // TODO check overflow.
                    __tick_to_time[event.tick],
                )?;
                time_signature_changes.push(ts_obj);
            }
            TrackEventKind::Meta(MetaMessage::Lyric(text)) => lyrics.push(Lyric::new(
                str::from_utf8(text).unwrap(),
                __tick_to_time[event.tick],
            )),
            TrackEventKind::Meta(MetaMessage::Text(text)) => text_events.push(Text::new(
                str::from_utf8(text).unwrap(),
                __tick_to_time[event.tick],
            )),
            // Look for track name events.
            TrackEventKind::Meta(MetaMessage::TrackName(name)) => {
                track_name_map.insert(event.track_idx, name);
            }
            // Look for program change events.
            TrackEventKind::Midi {
                channel,
                message: MidiMessage::ProgramChange { program },
            } => {
                // Update the instrument for this channel
                current_instrument[channel.as_int() as usize] = program.as_int();
            }
            // Note ons are note on events with velocity > 0.
            TrackEventKind::Midi {
                channel,
                message: MidiMessage::NoteOn { key, vel },
            } if vel.as_int() > 0 => {
                // Store this as the last note-on location.
                let note_on_index = (event.track_idx, channel.as_int(), key.as_int());
                last_note_on
                    .entry(note_on_index)
                    .or_insert(vec![])
                    .push((event.tick, vel.as_int()));
            }
            // Note offs can also be note on events with 0 velocity.
            TrackEventKind::Midi {
                channel,
                message: MidiMessage::NoteOn { key, .. } | MidiMessage::NoteOff { key, .. },
            } => {
                // Check that a note-on exists (ignore spurious note-offs).
                let k = (event.track_idx, channel.as_int(), key.as_int());
                if last_note_on.contains_key(&k) {
                    // Get the start/stop times and velocity of every note
                    // which was turned on with this instrument/drum/pitch.
                    // One note-off may close multiple note-on events from
                    // previous ticks. In case there's a note-off and then
                    // note-on at the same tick we keep the open note from
                    // this tick.
                    let end_tick = event.tick;
                    let open_notes = last_note_on.get(&k).unwrap();

                    let notes_to_close: Vec<(usize, u8)> = open_notes
                        .iter()
                        .filter(|a| a.0 != end_tick)
                        .cloned()
                        .collect();
                    let notes_to_keep: Vec<(usize, u8)> = open_notes
                        .iter()
                        .filter(|a| a.0 == end_tick)
                        .cloned()
                        .collect();

                    for (start_tick, velocity) in notes_to_close.iter() {
                        let start_time = __tick_to_time[*start_tick];
                        let end_time = __tick_to_time[end_tick];
                        // Create the note event
                        let note = Note::new(*velocity, key.as_int(), start_time, end_time);
                        // Get the program and drum type for the current
                        // instrument.
                        let program = current_instrument[channel.as_int() as usize];
                        // Retrieve the Instrument instance for the current
                        // instrument.
                        // Create a new instrument if none exists.
                        // Add the note event.
                        notes_map
                            .entry((program, channel.as_int(), event.track_idx))
                            .or_insert(vec![])
                            .push(note);
                    }

                    if !notes_to_close.is_empty() && !notes_to_keep.is_empty() {
                        // Note-on on the same tick but we already closed
                        // some previous notes -> it will continue, keep it.
                        last_note_on.insert(k, notes_to_keep);
                    } else {
                        last_note_on.remove(&k);
                    }
                }
            }
            // Store pitch bends.
            TrackEventKind::Midi {
                channel,
                message: MidiMessage::PitchBend { bend },
            } => {
                // Create pitch bend class instance.
                let bend = PitchBend::new(bend.as_int(), __tick_to_time[event.tick]);
                // Retrieve the Instrument instance for the current inst.
                // Don't create a new instrument if none exists.
                // Add the pitch bend event.
                pitch_bends_map
                    .entry((channel.as_int(), event.track_idx))
                    .or_insert(vec![])
                    .push(bend);
            }
            // Store control changes.
            TrackEventKind::Midi {
                channel,
                message: MidiMessage::Controller { controller, value },
            } => {
                let control_change = ControlChange::new(
                    controller.as_int(),
                    value.as_int(),
                    __tick_to_time[event.tick],
                );
                // Retrieve the Instrument instance for the current inst.
                // Don't create a new instrument if none exists.
                // Add the control change event.
                control_changes_map
                    .entry((channel.as_int(), event.track_idx))
                    .or_insert(vec![])
                    .push(control_change);
            }
            _ => (),
        }
    }

    // Populate the list of instruments.
    let mut instruments = vec![];
    for (program, channel, track_idx) in notes_map.keys() {
        let is_drum = *channel == 9;
        let track_name = match track_name_map.get(track_idx) {
            Some(name) => name.to_vec(),
            _ => vec![],
        };
        let mut instrument = Instrument::new(*program, is_drum, String::from_utf8(track_name)?);

        if let Some(notes) = notes_map.get(&(*program, *channel, *track_idx)) {
            instrument.notes = notes.clone();
        }
        if let Some(pitch_bends) = pitch_bends_map.get(&(*channel, *track_idx)) {
            instrument.pitch_bends = pitch_bends.clone();
        }
        if let Some(control_changes) = control_changes_map.get(&(*channel, *track_idx)) {
            instrument.control_changes = control_changes.clone();
        }

        instruments.push(instrument);
    }

    Ok((
        key_signature_changes,
        time_signature_changes,
        lyrics,
        text_events,
        instruments,
    ))
}
