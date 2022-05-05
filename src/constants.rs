//! This file defines MIDI standard constants, which are useful for converting
//! between numeric MIDI data values and human-readable text.

use once_cell::sync::Lazy;
use std::collections::HashMap;

/// `INSTRUMENT_MAP[program_number]` maps the program_number to an instrument name.
pub const INSTRUMENT_MAP: [&str; 128] = [
    "Acoustic Grand Piano",
    "Bright Acoustic Piano",
    "Electric Grand Piano",
    "Honky-tonk Piano",
    "Electric Piano 1",
    "Electric Piano 2",
    "Harpsichord",
    "Clavinet",
    "Celesta",
    "Glockenspiel",
    "Music Box",
    "Vibraphone",
    "Marimba",
    "Xylophone",
    "Tubular Bells",
    "Dulcimer",
    "Drawbar Organ",
    "Percussive Organ",
    "Rock Organ",
    "Church Organ",
    "Reed Organ",
    "Accordion",
    "Harmonica",
    "Tango Accordion",
    "Acoustic Guitar (nylon)",
    "Acoustic Guitar (steel)",
    "Electric Guitar (jazz)",
    "Electric Guitar (clean)",
    "Electric Guitar (muted)",
    "Overdriven Guitar",
    "Distortion Guitar",
    "Guitar Harmonics",
    "Acoustic Bass",
    "Electric Bass (finger)",
    "Electric Bass (pick)",
    "Fretless Bass",
    "Slap Bass 1",
    "Slap Bass 2",
    "Synth Bass 1",
    "Synth Bass 2",
    "Violin",
    "Viola",
    "Cello",
    "Contrabass",
    "Tremolo Strings",
    "Pizzicato Strings",
    "Orchestral Harp",
    "Timpani",
    "String Ensemble 1",
    "String Ensemble 2",
    "Synth Strings 1",
    "Synth Strings 2",
    "Choir Aahs",
    "Voice Oohs",
    "Synth Choir",
    "Orchestra Hit",
    "Trumpet",
    "Trombone",
    "Tuba",
    "Muted Trumpet",
    "French Horn",
    "Brass Section",
    "Synth Brass 1",
    "Synth Brass 2",
    "Soprano Sax",
    "Alto Sax",
    "Tenor Sax",
    "Baritone Sax",
    "Oboe",
    "English Horn",
    "Bassoon",
    "Clarinet",
    "Piccolo",
    "Flute",
    "Recorder",
    "Pan Flute",
    "Blown bottle",
    "Shakuhachi",
    "Whistle",
    "Ocarina",
    "Lead 1 (square)",
    "Lead 2 (sawtooth)",
    "Lead 3 (calliope)",
    "Lead 4 chiff",
    "Lead 5 (charang)",
    "Lead 6 (voice)",
    "Lead 7 (fifths)",
    "Lead 8 (bass + lead)",
    "Pad 1 (new age)",
    "Pad 2 (warm)",
    "Pad 3 (polysynth)",
    "Pad 4 (choir)",
    "Pad 5 (bowed)",
    "Pad 6 (metallic)",
    "Pad 7 (halo)",
    "Pad 8 (sweep)",
    "FX 1 (rain)",
    "FX 2 (soundtrack)",
    "FX 3 (crystal)",
    "FX 4 (atmosphere)",
    "FX 5 (brightness)",
    "FX 6 (goblins)",
    "FX 7 (echoes)",
    "FX 8 (sci-fi)",
    "Sitar",
    "Banjo",
    "Shamisen",
    "Koto",
    "Kalimba",
    "Bagpipe",
    "Fiddle",
    "Shanai",
    "Tinkle Bell",
    "Agogo",
    "Steel Drums",
    "Woodblock",
    "Taiko Drum",
    "Melodic Tom",
    "Synth Drum",
    "Reverse Cymbal",
    "Guitar Fret Noise",
    "Breath Noise",
    "Seashore",
    "Bird Tweet",
    "Telephone Ring",
    "Helicopter",
    "Applause",
    "Gunshot",
];

/// `INSTRUMENT_CLASSES` contains the classes present in INSTRUMENTS.
pub const INSTRUMENT_CLASSES: [&str; 16] = [
    "Piano",
    "Chromatic Percussion",
    "Organ",
    "Guitar",
    "Bass",
    "Strings",
    "Ensemble",
    "Brass",
    "Reed",
    "Pipe",
    "Synth Lead",
    "Synth Pad",
    "Synth Effects",
    "Ethnic",
    "Percussive",
    "Sound Effects",
];

/// List which maps MIDI note number - 35 to drum name
/// from <http://www.midi.org/techspecs/gm1sound.php>.
pub const DRUM_MAP: [&str; 47] = [
    "Acoustic Bass Drum",
    "Bass Drum 1",
    "Side Stick",
    "Acoustic Snare",
    "Hand Clap",
    "Electric Snare",
    "Low Floor Tom",
    "Closed Hi Hat",
    "High Floor Tom",
    "Pedal Hi Hat",
    "Low Tom",
    "Open Hi Hat",
    "Low-Mid Tom",
    "Hi-Mid Tom",
    "Crash Cymbal 1",
    "High Tom",
    "Ride Cymbal 1",
    "Chinese Cymbal",
    "Ride Bell",
    "Tambourine",
    "Splash Cymbal",
    "Cowbell",
    "Crash Cymbal 2",
    "Vibraslap",
    "Ride Cymbal 2",
    "Hi Bongo",
    "Low Bongo",
    "Mute Hi Conga",
    "Open Hi Conga",
    "Low Conga",
    "High Timbale",
    "Low Timbale",
    "High Agogo",
    "Low Agogo",
    "Cabasa",
    "Maracas",
    "Short Whistle",
    "Long Whistle",
    "Short Guiro",
    "Long Guiro",
    "Claves",
    "Hi Wood Block",
    "Low Wood Block",
    "Mute Cuica",
    "Open Cuica",
    "Mute Triangle",
    "Open Triangle",
];

/// `MIDI_KEY_SIGNATURE_TO_KEY_NUMBER_MAP[(key, scale)]` maps the MIDI key signature to a key number.
pub static MIDI_KEY_SIGNATURE_TO_KEY_NUMBER_MAP: Lazy<HashMap<(i8, bool), u8>> = Lazy::new(|| {
    let mut m = HashMap::new();
    m.insert((-7, false), 11u8);
    m.insert((-6, false), 6);
    m.insert((-5, false), 1);
    m.insert((-4, false), 8);
    m.insert((-3, false), 3);
    m.insert((-2, false), 10);
    m.insert((-1, false), 5);
    m.insert((0, false), 0);
    m.insert((1, false), 7);
    m.insert((2, false), 2);
    m.insert((3, false), 9);
    m.insert((4, false), 4);
    m.insert((5, false), 11);
    m.insert((6, false), 6);
    m.insert((7, false), 1);
    m.insert((-7, true), 20);
    m.insert((-6, true), 15);
    m.insert((-5, true), 22);
    m.insert((-4, true), 17);
    m.insert((-3, true), 12);
    m.insert((-2, true), 19);
    m.insert((-1, true), 14);
    m.insert((0, true), 21);
    m.insert((1, true), 16);
    m.insert((2, true), 23);
    m.insert((3, true), 18);
    m.insert((4, true), 13);
    m.insert((5, true), 20);
    m.insert((6, true), 15);
    m.insert((7, true), 22);
    m
});

/// `KEY_NUMBER_TO_MIDI_KEY_SIGNATURE_MAP[key_number]` maps the key number to a MIDI key signature.
pub static KEY_NUMBER_TO_MIDI_KEY_SIGNATURE_MAP: Lazy<HashMap<u8, (i8, bool)>> = Lazy::new(|| {
    let mut m = HashMap::new();
    for (k, v) in MIDI_KEY_SIGNATURE_TO_KEY_NUMBER_MAP.clone() {
        m.insert(v, k);
    }
    m
});
