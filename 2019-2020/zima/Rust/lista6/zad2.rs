fn dig_pow(n: i64, p: i32) -> i64 {
    let sum = n.to_string()
               .chars()
               .enumerate()
               .map(|(i, c)| (c.to_digit(10).unwrap() as i128)
                               .pow(i as u32 + p))
               .sum::<i128>();

    if sum % n as i128 == 0 { (sum / n as i128) as i64 } else { -1 }
}

#[test]
fn test0() {
    assert_eq!(dig_pow(89, 1), 1);
}

#[test]
fn test1() {
    assert_eq!(dig_pow(92, 1), -1);
}

#[test]
fn test2() {
    assert_eq!(dig_pow(46288, 3), 51);
}

#[test]
fn test3() {
    assert_eq!(dig_pow(1, 1000), 1);
}

#[test]
fn test4() {
    assert_eq!(dig_pow(2137, 42), -1);
}

#[test]
fn test5() {
    assert_eq!(dig_pow(2, 5), 16);
}

#[test]
fn test6() {
    assert_eq!(dig_pow(76, 4), -1)
}

#[test]
fn test7() {
    assert_eq!(dig_pow(135, 3), -1)
}

#[test]
fn test8() {
    assert_eq!(dig_pow(263, 5), -1)
}

#[test]
fn test9() {
    assert_eq!(dig_pow(123, 1), -1)
}
