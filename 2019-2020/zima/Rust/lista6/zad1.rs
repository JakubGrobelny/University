fn encode(msg: String, n: i32) -> Vec<i32> {
    let n_str = n.to_string();
    let digits = n_str.chars()
                      .map(|d| (d as u8 - '0' as u8) as i32)
                      .cycle();

    msg.chars().zip(digits).map(|(c, digit)| {
        (c as u8 - 'a' as u8 + 1) as i32 + digit
    }).collect::<Vec<i32>>()
}

#[test]
fn test0() {
    assert_eq!(encode("".to_string(), 420666), vec![]);
}

#[test]
fn test1() {
    assert_eq!(encode("scout".to_string(), 1939), vec![20, 12, 18, 30, 21]);
}

#[test]
fn test2() {
    assert_eq!(encode("abbba".to_string(), 1), vec![2,3,3,3,2]);
}

#[test]
fn test3() {
    assert_eq!(encode("rust".to_string(), 2), vec![20, 23, 21, 22]);
}

#[test]
fn test4() {
    assert_eq!(encode("jakisnapis".to_string(), 2137), vec![12, 2, 14, 16, 21, 15, 4, 23, 11, 20]);
}

#[test]
fn test5() {
    assert_eq!(encode("qqqq".to_string(), 2), vec![19, 19, 19, 19]);
}

#[test]
fn test6() {
    assert_eq!(encode("jakisnapis".to_string(), 555334), vec![15, 6, 16, 12, 22, 18, 6, 21, 14, 22]);
}

#[test]
fn test7() {
    assert_eq!(encode("bacg".to_string(), 0), vec![2,1,3,7]);
}

#[test]
fn test8() {
    assert_eq!(encode("informatyka".to_string(), 123), vec![10, 16, 9, 16, 20, 16, 2, 22, 28, 12, 3]);
}

#[test]
fn test9() {
    assert_eq!(encode("dereferencja".to_string(), 2137), vec![6, 6, 21, 12, 8, 6, 21, 12, 16, 4, 13, 8]);
}
