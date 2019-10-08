fn count_red_beads(n: u32) -> u32 {
    if n == 0 { 0 } else { 2 * (n-1) }
}

#[test]
fn test_0() {
    assert_eq!(count_red_beads(0), 0);
}

#[test]
fn test_1() {
    assert_eq!(count_red_beads(1), 0);
}

#[test]
fn test_2() {
    assert_eq!(count_red_beads(3), 4);
}

#[test]
fn test_3() {
    assert_eq!(count_red_beads(5), 8);
}

#[test]
fn test_4() {
    assert_eq!(count_red_beads(6), 10);
}

#[test]
fn test_5() {
    assert_eq!(count_red_beads(2001), 4000);
}

#[test]
fn test_6() {
    assert_eq!(count_red_beads(7), 12);
}

#[test]
fn test_7() {
    assert_eq!(count_red_beads((std::u32::MAX - 1) / 2 + 1), (std::u32::MAX - 1));
}

#[test]
fn test_8() {
    assert_eq!(count_red_beads(2001), 4000);
}

#[test]
fn test_9() {
    assert_eq!(count_red_beads(2), 2);
}



