fn summy(s: &str) -> i32 {
    s.split(" ").map(|x| x.parse::<i32>().unwrap_or(0)).sum()
}

#[test]
fn test0() {
    assert_eq!(summy(""), 0)
}

#[test]
fn test1() {
    assert_eq!(summy("1 1 2"), 4)
}

#[test]
fn test2() {
    assert_eq!(summy("42"), 42)
}

#[test]
fn test3() {
    assert_eq!(summy("0      130"), 130)
}

#[test]
fn test4() {
    assert_eq!(summy("0      135 0 0"), 135)
}

#[test]
fn test5() {
    assert_eq!(summy("-2"), -2)
}

#[test]
fn test6() {
    assert_eq!(summy("-2 -2 4"), 0)
}

#[test]
fn test7() {
    assert_eq!(summy("-2           -2"), -4)
}

#[test]
fn test8() {
    assert_eq!(summy("-1 0 1"), 0)
}

#[test]
fn test9() {
    assert_eq!(summy("    "), 0)
}