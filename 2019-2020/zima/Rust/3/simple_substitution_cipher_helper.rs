use std::collections::BTreeMap;

struct Cipher {
    map_encode : BTreeMap<char, char>,
    map_decode : BTreeMap<char, char>,
}

fn translate(map: &BTreeMap<char, char>, s: &str) -> String {
    s.chars()
     .map(|c| map.get(&c).unwrap_or(&c).clone())
     .collect::<String>()
}

impl Cipher {
    fn new(map1: &str, map2: &str) -> Cipher {
        assert!(map1.len() == map2.len());
        let mut map_encode = BTreeMap::new();
        let mut map_decode = BTreeMap::new();
        for (c0, c1) in map1.chars().zip(map2.chars()) {
            map_encode.insert(c0, c1);
            map_decode.insert(c1, c0);
        }
        Cipher {map_encode, map_decode}
    }

    fn encode(&self, string: &str) -> String {
        translate(&self.map_encode, string)
    }

    fn decode(&self, string: &str) -> String {
        translate(&self.map_decode, string)
    }
}

#[test]
fn test0() {
    let map1 = "abcdefghijklmnopqrstuvwxyz";
    let map2 = "etaoinshrdlucmfwypvbgkjqxz";
    let cipher = Cipher::new(map1, map2);

    assert_eq!(cipher.encode("abc"), "eta");
}

#[test]
fn test1() {
    let map1 = "abcdefghijklmnopqrstuvwxyz";
    let map2 = "etaoinshrdlucmfwypvbgkjqxz";
    let cipher = Cipher::new(map1, map2);

    assert_eq!(cipher.decode("erlang"), "aikcfu");
}

#[test]
fn test2() {
    let map1 = "abcdefghijklmnopqrstuvwxyz";
    let map2 = "etaoinshrdlucmfwypvbgkjqxz";
    let cipher = Cipher::new(map1, map2);

    assert_eq!(cipher.decode("eirfg"), "aeiou");
}

#[test]
fn test3() {
    let map1 = "abcdefghijklmnopqrstuvwxyz";
    let map2 = "etaoinshrdlucmfwypvbgkjqxz";
    let cipher = Cipher::new(map2, map1);

    assert_eq!(cipher.decode("haskell"), "hevliuu");
}

#[test]
fn test4() {
    let map1 = "ABC";
    let map2 = "CBA";
    let cipher = Cipher::new(map1, map2);

    assert_eq!(cipher.decode("ABCABC"), "CBACBA");
}

#[test]
fn test5() {
    let map1 = "qwerty";
    let map2 = "aaaaaa";
    let cipher = Cipher::new(map2, map1);

    assert_eq!(cipher.decode("qwertyqwerty"), "aaaaaaaaaaaa");
}


#[test]
fn test6() {
    let map1 = "qwerty";
    let map2 = "aaaaaa";
    let cipher = Cipher::new(map1, map2);

    assert_eq!(cipher.decode("QWERTY"), "QWERTY");
}

#[test]
fn test7() {
    let map1 = "12345";
    let map2 = "54321";
    let cipher = Cipher::new(map1, map2);

    assert_eq!(cipher.decode("531"), "135");
}

#[test]
fn test8() {
    let map1 = "qwerty";
    let map2 = "asdfgh";
    let cipher = Cipher::new(map1, map2);

    assert_eq!(cipher.decode("ytrewqA"), "ytrewqA");
}

#[test]
fn test9() {
    let map1 = "A1B2C3";
    let map2 = "111111";
    let cipher = Cipher::new(map2, map1);

    assert_eq!(cipher.decode("1111"), "1111");
}