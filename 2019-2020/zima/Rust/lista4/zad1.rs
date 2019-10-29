fn find_digit(num: i32, nth: i32) -> i32 {
    if num < 0 { 
        return find_digit(-num, nth) 
    } 
    if nth <= 0 { 
        return -1 
    }
    if nth == 1 { 
        return num % 10 
    }
    find_digit(num / 10, nth-1)
}

#[test]
fn test0() {
    assert_eq!(find_digit(0, 5), 0);
}

#[test]
fn test1() {
    assert_eq!(find_digit(1, 1), 1);
}

#[test]
fn test2() {
    assert_eq!(find_digit(-1, 1), 1);
}

#[test]
fn test3() {
    assert_eq!(find_digit(101, 2), 0);
}

#[test]
fn test4() {
    assert_eq!(find_digit(1234, -4), -1);
}

#[test]
fn test5() {
    assert_eq!(find_digit(123456, 3), 4);
}

#[test]
fn test6() {
    assert_eq!(find_digit(0, 0), -1);
}

#[test]
fn test7() {
    assert_eq!(find_digit(999, 3), 9);
}

#[test]
fn test8() {
    assert_eq!(find_digit(-999, 100), 0);
}

#[test]
fn test9() {
    assert_eq!(find_digit(1_000_000_000, 10), 1);
}
