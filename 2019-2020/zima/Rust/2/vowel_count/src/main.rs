fn get_count(string: &str) -> usize {
    fn is_vowel(c: char) -> usize {
        match c {
            'a' | 'e' | 'i' | 'o' | 'u' => 1,
            _ => 0
        }
    }

    string.chars().map(is_vowel).sum()
}

#[test]
fn test0() {
    assert_eq!(get_count("aaaaa"), 5);
}

#[test]
fn test1() {
    assert_eq!(get_count("bbbbb"), 0);
}

#[test]
fn test2() {
    assert_eq!(get_count(""), 0);
}

#[test]
fn test3() {
    assert_eq!(get_count("bbbbba"), 1);
}

#[test]
fn test4() {
    assert_eq!(get_count("aeiouzzaeiou"), 10);
}

#[test]
fn test5() {
    assert_eq!(get_count("a e i o u"), 5);
}

#[test]
fn test6() {
    assert_eq!(get_count("q w e r t y"), 1);
}

#[test]
fn test7() {
    assert_eq!(get_count("    "), 0);
}

#[test]
fn test8() {
    assert_eq!(get_count("iqqqq i"), 2);
}

#[test]
fn test9() {
    assert_eq!(get_count("quo i e a q"), 5);
}