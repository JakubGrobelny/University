fn last_digit(lst: &[u64]) -> u64 {
    fn trim(num: u64, modulo: u64) -> u64 {
        std::cmp::min(num % modulo + modulo, num)
    }

    lst.iter().rev().fold(1u64, |acc, &x| {
        // lcm(4, 10) == 20 so it's possible to calculate
        // both mod 10 and mod 4 without loss of information
        trim(x, 20).pow(trim(acc, 4) as u32)
    }) % 10
}

#[test]
fn test0() {
    assert_eq!(last_digit(&[2, 2]), 4)
}

#[test]
fn test1() {
    assert_eq!(last_digit(&[3,2,2]), 1)
}

#[test]
fn test2() {
    assert_eq!(last_digit(&[0, 0]), 1)
}

#[test]
fn test3() {
    assert_eq!(last_digit(&[]), 1)
}

#[test]
fn test4() {
    assert_eq!(last_digit(&[12, 30, 21]), 6)
}

#[test]
fn test5() {
    assert_eq!(last_digit(&[123232, 694022, 140249]), 6)

}

#[test]
fn test6() {
    assert_eq!(last_digit(&[2, 2, 101, 2]), 6)
}

#[test]
fn test7() {
    assert_eq!(last_digit(&[123232, 694022, 140249]), 6)
}

#[test]
fn test8() {
    assert_eq!(last_digit(&[499942, 898102, 846073]), 6)
}

#[test]
fn test9() {
    assert_eq!(last_digit(&[12, 30, 21]), 6)
}

#[test]
fn test10() {
    assert_eq!(last_digit(&[2, 2, 2, 0]), 4)
}