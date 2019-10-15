fn longest(a1: &str, a2: &str) -> String {
    let mut s : Vec<char> = format!("{}{}", a1, a2).chars().collect();
    s.sort();
    s.dedup();
    s.iter().collect()
}

#[test]
fn test0() {
    assert_eq!(longest("", ""), "");
}

#[test]
fn test1() {
    assert_eq!(longest("aaa", "aaa"), "a");
}

#[test]
fn test2() {
    assert_eq!(longest("abc", "abc"), "abc");
}

#[test]
fn test3() {
    assert_eq!(longest("abc", "cde"), "abcde");
}

#[test]
fn test4() {
    assert_eq!(longest("qwe", "eeerte"), "eqrtw");
}

#[test]
fn test5() {
    assert_eq!(longest("", "cdba"), "abcd");
}

#[test]
fn test6() {
    assert_eq!(longest("zyx", ""), "xyz");
}

#[test]
fn test7() {
    assert_eq!(longest("", "cdba"), "abcd");
}

#[test]
fn test8() {
    assert_eq!(longest("zyxx", "cdbax"), "abcdxyz");
}

#[test]
fn test9() {
    assert_eq!(longest("aaaaabc", "abbbbc"), "abc");
}
