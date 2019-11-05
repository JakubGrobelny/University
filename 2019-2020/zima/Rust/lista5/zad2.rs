fn descending_order(x: u64) -> u64 {
    let mut chars_vec : Vec<char> = x.to_string().chars().collect();
    chars_vec.sort_by(|lhs, rhs| rhs.cmp(lhs));
    let sorted_str : String = chars_vec.iter().collect();
    sorted_str.parse::<u64>().unwrap()
}

#[test]
fn test0() {
    assert_eq!(descending_order(0), 0)
}

#[test]
fn test1() {
    assert_eq!(descending_order(321), 321)
}

#[test]
fn test2() {
    assert_eq!(descending_order(123), 321)
}

#[test]
fn test3() {
    assert_eq!(descending_order(101010), 111000)
}

#[test]
fn test4() {
    assert_eq!(descending_order(1029384756), 9876543210)
}

#[test]
fn test5() {
    assert_eq!(descending_order(4578347534857345345), 8877755555444443333)
}

#[test]
fn test6() {
    assert_eq!(descending_order(9199999999999), 9999999999991)
}

#[test]
fn test7() {
    assert_eq!(descending_order(64446), 66444)
}

#[test]
fn test8() {
    assert_eq!(descending_order(123654), 654321)
}

#[test]
fn test9() {
    assert_eq!(descending_order(57575775), 77775555)
}
