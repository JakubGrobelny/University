fn string_to_number(s: &str) -> i32 {
    s.parse::<i32>().unwrap()
}

#[test]
fn test_negative() {
    assert_eq!(string_to_number("-1"), -1);
    assert_eq!(string_to_number("-0"), 0);
    assert_eq!(string_to_number("-05"), -5);
    assert_eq!(string_to_number("-123321"), -123321)
}

#[test]
fn test_long() {
    assert_eq!(string_to_number("999999999"), 999999999)
    assert_eq!(string_to_number("123321"), 123321)
}

#[test]
fn test_zero() {
    assert_eq!(string_to_number("0"), 0);
    assert_eq!(string_to_number("00"), 0);
    assert_eq!(string_to_number("000"), 0);
    assert_eq!(string_to_number("0000"), 0);
}
