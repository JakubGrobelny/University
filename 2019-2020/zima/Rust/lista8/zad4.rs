// Preloaded:
//
// struct MorseDecoder {
//     morse_code: HashMap<String, String>,
// }
//
// MorseDecoder::new() populates the morse_code map, e.g. ".-" -> "A".

use std::collections::HashMap;

struct MorseDecoder {
    morse_code : HashMap<String, String>,
}

impl MorseDecoder {
    	
    fn new() -> MorseDecoder {
        MorseDecoder { morse_code : [
                ("....-", "4"), ("--..--", ","), (".--", "W"), (".-.-.-", "."), 
                ("..---",  "2"), (".", "E"), ("--..", "Z"), (".----", "1"),
                (".-..", "L"), (".--.", "P"), (".-.", "R"), ("...", "S"), 
                ("-.--", "Y"), ("...--", "3"), (".....", "5"), ("--.", "G"),
                ("-.--.", "("), ("-....", "6"), (".-.-.", "+") ,
                ("...-..-", "$"), (".--.-.", "@"), ("...---...", "SOS"), 
                ("..--.-", "_"), ("-.", "N"), ("-..-", "X"), ("-----", "0"), 
                ("....", "H"), ("-...", "B"), (".---", "J"), ("---...", ","), 
                ("-", "T"), ("---..", "8"), ("-..-.", "/"), ("--.-", "Q"), 
                ("...-", "V"), ("----.", "9"), ("--", "M"), ("-.-.-.", ";"), 
                ("-.-.--", "!"), ("..-.", "F"), ("..--..", "?"), ("-...-", "="),
                ("..-", "U"), (".----.", "'"), ("---", "O"), ("-.--.-", ")"), 
                ("..", "I"), ("-....-", "-"), (".-..-.", "\""), (".-", "A"), 
                ("-.-.", "C"), ("-..", "D"), (".-...", "&"), ("--...", "7"), 
                ("-.-", "K")
             ].iter()
              .map(|(k, v)| (k.to_string(), v.to_string()))
              .collect()
        }
    }

    fn decode_morse(&self, encoded: &str) -> String {
        let decode_word = |word: &str| -> String {
            word.split(' ')
                .map(|c| self.morse_code.get(&c.to_string()).unwrap().clone())
                .collect::<Vec<String>>()
                .join("")
        };

        encoded.trim()
               .split("   ")
               .filter(|&word| word != "")
               .map(decode_word)
               .collect::<Vec<String>>()
               .join(" ")
    }
}


#[test]
fn test0() {
    let decoder = MorseDecoder::new();
    assert_eq!(decoder.decode_morse(""), "")
}

#[test]
fn test1() {
    let decoder = MorseDecoder::new();
    assert_eq!(decoder.decode_morse("...---..."), "SOS")
}

#[test]
fn test2() {
    let decoder = MorseDecoder::new();
    assert_eq!(decoder.decode_morse("...---...   ...---..."), "SOS SOS")
}

#[test]
fn test3() {
    let decoder = MorseDecoder::new();
    assert_eq!(decoder.decode_morse(
        ".-. ..- ... -   -. --- -   -.-. .-.-. .-.-."
    ), "RUST NOT C++")
}

#[test]
fn test4() {
    let decoder = MorseDecoder::new();
    assert_eq!(decoder.decode_morse(
        ".... . .-.. .-.. ---   .-- --- .-. .-.. -.."
    ), "HELLO WORLD")
}

#[test]
fn test5() {
    let decoder = MorseDecoder::new();
    assert_eq!(decoder.decode_morse(
        "--.- .-- . .-. - -.--   ... --- ...   .- -... -.-. -..   .---- ..--- ...--"
    ), "QWERTY SOS ABCD 123")
}

#[test]
fn test6() {
    let decoder = MorseDecoder::new();
    assert_eq!(decoder.decode_morse(
        "    "
    ), "")
}

#[test]
fn test7() {
    let decoder = MorseDecoder::new();
    assert_eq!(decoder.decode_morse(
        "    .... . .-.. .-.. ---      .-- --- .-. .-.. -..                   "
    ), "HELLO WORLD")
}

#[test]
fn test8() {
    let decoder = MorseDecoder::new();
    assert_eq!(decoder.decode_morse(
        "--.-   .--   .   .-.   -   -.--"
    ), "Q W E R T Y")
}

#[test]
fn test9() {
    let decoder = MorseDecoder::new();
    assert_eq!(decoder.decode_morse(
        "-... --- .-. .-. --- .--   -.-. .... . -.-. -.- . .-."
    ), "BORROW CHECKER")
}

