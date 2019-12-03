fn order(sentence: &str) -> String {
    let mut words = sentence.split_whitespace()
                            .collect::<Vec<&str>>();
    words.sort_by_key(|w| w.chars()
                           .find(|c| c.is_digit(10))
                           .and_then(|c| c.to_digit(10))
                           .unwrap_or(0));
    words.join(" ")
}

#[test]
fn test0() {
    assert_eq!(order("is2 Thi1s T4est 3a"), "Thi1s is2 3a T4est");
}

#[test]
fn test1() {
    assert_eq!(order(""), "")
}

#[test]
fn test2() {
    assert_eq!(order("s0rt di6its b1"), "s0rt b1 di6its")
}

#[test]
fn test3() {
    assert_eq!(order("a1 b2"), "a1 b2")
}

#[test]
fn test4() {
    assert_eq!(order("th4ese a7re som0e rand1om wo3rds"), "som0e rand1om wo3rds th4ese a7re")
}

#[test]
fn test5() {
    assert_eq!(order("b1a b1a b1a"), "b1a b1a b1a")
}

#[test]
fn test6() {
    assert_eq!(order("0 8 6 4 9 2 5"), "0 2 4 5 6 8 9")
}

#[test]
fn test7() {
    assert_eq!(order("  "), "")
}

#[test]
fn test8() {
    assert_eq!(order("5rust 2is 3better 4than 0haskell"), "0haskell 2is 3better 4than 5rust")
}

#[test]
fn test9() {
    assert_eq!(order("  str0ing  "), "str0ing")
}