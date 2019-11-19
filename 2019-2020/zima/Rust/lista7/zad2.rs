use std::collections::BTreeMap;

fn letter_frequency(input: &str) -> BTreeMap<char, i32> {
    let clean : String = input.to_ascii_lowercase()
                               .chars()
                               .filter(|c| c.is_lowercase())
                               .collect();

    let mut freqs : BTreeMap<char, i32> = BTreeMap::new();

    for c in clean.chars() {
        let frequency = match freqs.get(&c) {
            Some (n) => n+1,
            None     => 1,
        };
        freqs.insert(c, frequency);
    }

    freqs
}

#[test]
fn test0() {
    assert_eq!(
        letter_frequency(""), 
        [].iter().cloned().collect::<BTreeMap<char, i32>>()
    )
}

#[test]
fn test1() {
    assert_eq!(
        letter_frequency("ABC"),
        [('a', 1), ('b', 1), ('c', 1)]
            .iter()
            .cloned()
            .collect::<BTreeMap<char, i32>>()
    )
}

#[test]
fn test2() {
    assert_eq!(
        letter_frequency("ABCA"),
        [('a', 2), ('b', 1), ('c', 1)]
            .iter()
            .cloned()
            .collect::<BTreeMap<char, i32>>()
    )
}

#[test]
fn test3() {
    assert_eq!(
        letter_frequency("ABCAa"),
        [('a', 3), ('b', 1), ('c', 1)]
            .iter()
            .cloned()
            .collect::<BTreeMap<char, i32>>()
    )
}

#[test]
fn test4() {
    assert_eq!(
        letter_frequency("A a A a"),
        [('a', 4)]
            .iter()
            .cloned()
            .collect::<BTreeMap<char, i32>>()
    )
}

#[test]
fn test5() {
    assert_eq!(
        letter_frequency("A B C A a"),
        [('a', 3), ('b', 1), ('c', 1)]
            .iter()
            .cloned()
            .collect::<BTreeMap<char, i32>>()
    )
}

#[test]
fn test6() {
    assert_eq!(
        letter_frequency("99999"),
        []
            .iter()
            .cloned()
            .collect::<BTreeMap<char, i32>>()
    )
}

#[test]
fn test7() {
    assert_eq!(
        letter_frequency("Rust"),
        [('r', 1), ('u', 1), ('s', 1), ('t', 1)]
            .iter()
            .cloned()
            .collect::<BTreeMap<char, i32>>()
    )
}

#[test]
fn test8() {
    assert_eq!(
        letter_frequency("Rust Rust Rust"),
        [('r', 3), ('u', 3), ('s', 3), ('t', 3)]
            .iter()
            .cloned()
            .collect::<BTreeMap<char, i32>>()
    )
}

#[test]
fn test9() {
    assert_eq!(
        letter_frequency("Rust RUST RuSt"),
        [('r', 3), ('u', 3), ('s', 3), ('t', 3)]
            .iter()
            .cloned()
            .collect::<BTreeMap<char, i32>>()
    )
}











