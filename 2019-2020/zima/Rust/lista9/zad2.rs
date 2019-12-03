fn dont_give_me_five(start: isize, end: isize) -> isize {
    (start..=end).filter(|n| !n.to_string().contains('5')).count() as isize
}

#[test]
fn test0() {
    assert_eq!(dont_give_me_five(1, 9), 8);
}

#[test]
fn test1() {
    assert_eq!(dont_give_me_five(4, 17), 12)
}

#[test]
fn test2() {
    assert_eq!(dont_give_me_five(-9, -1), 8)
}

#[test]
fn test3() {
    assert_eq!(dont_give_me_five(-17, -4), 12)
}

#[test]
fn test4() {
    assert_eq!(dont_give_me_five(0, 5), 5)
}

#[test]
fn test5() {
    assert_eq!(dont_give_me_five(-5, 0), 5)
}

#[test]
fn test6() {
    assert_eq!(dont_give_me_five(5000, 5999), 0)
}

#[test]
fn test7() {
    assert_eq!(dont_give_me_five(-5999, -5000), 0)
}

#[test]
fn test8() {
    assert_eq!(dont_give_me_five(-6000, -5000), 1)
}

#[test]
fn test9() {
    assert_eq!(dont_give_me_five(0, 15), 14)
}
















