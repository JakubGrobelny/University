fn count_bits(n: i64) -> u32 {
    n.count_ones()
}

#[test]
fn test0() {
    assert_eq!(count_bits(0), 0);
}

#[test]
fn test1() {
    assert_eq!(count_bits(0b1000), 1);
}

#[test]
fn test2() {
    assert_eq!(count_bits(0b1111), 4)
}

#[test]
fn test3() {
    assert_eq!(count_bits(0b1010101), 4)
}

#[test]
fn test4() {
    assert_eq!(count_bits(0xFF), 8)
}

#[test]
fn test5() {
    assert_eq!(count_bits(0xF0F0), 8)
}

#[test]
fn test6() {
    assert_eq!(count_bits(0xF0F0E), 11)
}

#[test]
fn test7() {
    assert_eq!(count_bits(0x8000_0000), 1)
}

#[test]
fn test8() {
    assert_eq!(count_bits(0x8000_8000), 2)
}

#[test]
fn test9() {
    assert_eq!(count_bits(0x8888_8888), 8)
}
