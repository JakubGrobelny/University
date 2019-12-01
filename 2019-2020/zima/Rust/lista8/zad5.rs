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

    pub fn decode_bits(&self, encoded: &str) -> String {
        fn find_time_unit(encoded: &str) -> usize {
            fn min_len(encoded: &str, digit: char, other: char) -> usize {
                encoded.split(other)
                       .filter(|s| s.contains(digit))
                       .map(|s| s.len())
                       .min()
                       .unwrap_or(std::usize::MAX)
            }

            let min_ones = min_len(encoded, '1', '0');
            let min_zeros = min_len(encoded, '0', '1');

            std::cmp::min(min_ones, min_zeros)
        }
        
        let encoded = encoded.trim_end_matches('0')
                             .trim_start_matches('0');
        let time_unit = find_time_unit(encoded);
        
        let res = encoded.replace(&"0".repeat(7 * time_unit), "   ")
            .replace(&"0".repeat(3 * time_unit), " ")
            .replace(&"1".repeat(3 * time_unit), "-")
            .replace(&"1".repeat(time_unit), ".")
            .replace(&"0", "");

        return res
    }
    

    pub fn decode_morse(&self, encoded: &str) -> String {
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
    assert_eq!(decoder.decode_bits(
        "000111100001111111111110000111111111111000011110000000000001111111111110000111111111111000011110000000000001111000011110000111100000000000000000000000000001111111111110000111100"
    ), ".--. --. ...   -.")
}

#[test]
fn test1() {
    let decoder = MorseDecoder::new();
    assert_eq!(decoder.decode_bits(
        "0000000111011101110001110111011100000000000"
    ), "--- ---")
}

#[test]
fn test2() {
    let decoder = MorseDecoder::new();
    assert_eq!(decoder.decode_bits(
        "00001100111111001111110011000000111111001111110011000000110011001100000000000000111111001100000"
    ), ".--. --. ...   -.")
}

#[test]
fn test3() {
    let decoder = MorseDecoder::new();
    assert_eq!(decoder.decode_bits(
        "0000000111011101110000000001110111011100000000000"
    ), "---   ---")
}

#[test]
fn test4() {
    let decoder = MorseDecoder::new();
    assert_eq!(decoder.decode_bits(
        "0100011100000001000111"
    ), ". -   . -")
}

#[test]
fn test5() {
    let decoder = MorseDecoder::new();
    assert_eq!(decoder.decode_morse(decoder.decode_bits(
            "101010111011101110101010"
        ).as_str()),
        "SOS"
    )
}

#[test]
fn test6() {
    let decoder = MorseDecoder::new();
    assert_eq!(decoder.decode_morse(decoder.decode_bits(
            "101010111011101110101010000000101010111011101110101010"
        ).as_str()),
        "SOS SOS"
    )
}

#[test]
fn test7() {
    let decoder = MorseDecoder::new();
    assert_eq!(decoder.decode_morse(decoder.decode_bits(
            "00000000001100110011001111110011111100111111001100110011000"
        ).as_str()),
        "SOS"
    )
}

#[test]
fn test8() {
    let decoder = MorseDecoder::new();
    assert_eq!(decoder.decode_morse(decoder.decode_bits(
        "101010100010001011101010001011101010001110111011100000001011101110001110111011100010111010001011101010001110101"
    ).as_str()),
        "HELLO WORLD"
    )
}

#[test]
fn test9() {
    let decoder = MorseDecoder::new();
    assert_eq!(decoder.decode_morse(decoder.decode_bits(
        "0000111000111000111000111000000000111000000000111000111111111000111000111000000000111000111111111000111000111000000000111111111000111111111000111111111000000000000000000000111000111111111000111111111000000000111111111000111111111000111111111000000000111000111111111000111000000000111000111111111000111000111000000000111111111000111000111000000000000000000000"
    ).as_str()),
        "HELLO WORLD"
    )
}



