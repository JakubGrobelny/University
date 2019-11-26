fn alphabet_position(text: &str) -> String {
    fn char_to_pos(c: char) -> String { 
        (c as u8 - 'a' as u8 + 1).to_string() 
    }
    text.to_ascii_lowercase()
        .chars()
        .filter(|c| c.is_alphabetic())
        .map(char_to_pos)
        .collect::<Vec<String>>()
        .join(" ")
}

#[test]
fn test0() {
    assert_eq!(
        alphabet_position("The sunset sets at twelve o' clock."),
        String::from(
            "20 8 5 19 21 14 19 5 20 19 5 20 19 1 20 20 23 5 12 22 5 15 3 12 15 3 11"
        )
    )
}

#[test]
fn test1() {
    assert_eq!(alphabet_position("abc"), "1 2 3")
}

#[test]
fn test2() {
    assert_eq!(alphabet_position("1 2 3"), "")
}

#[test]
fn test3() {
    assert_eq!(alphabet_position("jakub"), "10 1 11 21 2")
}

#[test]
fn test4() {
    assert_eq!(
        alphabet_position("haskell > rust"), 
        "8 1 19 11 5 12 12 18 21 19 20"
    )
}

#[test]
fn test5() {
    assert_eq!(alphabet_position(""), "")
}

#[test]
fn test6() {
    assert_eq!(alphabet_position("q w e rty"), "17 23 5 18 20 25")
}

#[test]
fn test7() {
    assert_eq!(alphabet_position("a B c D e F g"), "1 2 3 4 5 6 7")
}

#[test]
fn test8() {
    assert_eq!(alphabet_position("AAAaaa"), "1 1 1 1 1 1")
}

#[test]
fn test9() {
    assert_eq!(alphabet_position("To jest zdanie"), "20 15 10 5 19 20 26 4 1 14 9 5")
}




