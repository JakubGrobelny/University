fn xo(string: &'static str) -> bool {
    string.chars().fold(0i64, |acc, c| {
        match c {
            'X' | 'x' => acc + 1,
            'O' | 'o' => acc - 1,
            _ => acc
        }
    }) == 0
}

#[test]
fn test0() {
    assert_eq!(xo(""), true)
}

#[test]
fn test1() {
    assert_eq!(xo("INNE ZNAKI"), true)
}

#[test]
fn test2() {
    assert_eq!(xo("xOoX"), true)
}

#[test]
fn test3() {
    assert_eq!(xo("xxxOOOO"), false)
}

#[test]
fn test4() {
    assert_eq!(xo("x"), false)
}

#[test]
fn test5() {
    assert_eq!(xo("xoxoxoxoxoxoxoXXXO"), false)
}

#[test]
fn test6() {
    assert_eq!(xo("xooxoXXoxoXXxoxoxoxoXXXO"), false)
}

#[test]
fn test7() {
    assert_eq!(xo("XXOoqwe"), true)
}

#[test]
fn test8() {
    assert_eq!(xo("XaaOaaOaaX"), true)
}

#[test]
fn test9() {
    assert_eq!(xo("QxO"), true)
}