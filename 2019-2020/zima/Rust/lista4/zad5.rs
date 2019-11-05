fn sum_pairs(ints: &[i8], s: i8) -> Option<(i8, i8)> {
    use std::collections::HashSet;
    let mut set = HashSet::new();
    
    for i in ints {
        if set.contains(&(s - i)) {
            return Some((s - i, *i));
        }
        set.insert(i);
    }

    None
}

#[test]
fn test0() {
    assert_eq!(sum_pairs(&[1,2,3], 0), None)
}

#[test]
fn test1() {
    assert_eq!(sum_pairs(&[3,6,7,9], 10), Some((3, 7)))
}

#[test]
fn test2() {
    assert_eq!(sum_pairs(&[0, 0, 1, 10, 7, 2], 10), Some((0, 10)))
}

#[test]
fn test3() {
    assert_eq!(sum_pairs(&[10, 5, 2, 3, 7, 5], 10), Some((3, 7)))
}

#[test]
fn test4() {
    assert_eq!(sum_pairs(&[5, 2, 10, 7 ,5, 3], 10), Some((5, 5)))
}

#[test]
fn test5() {
    assert_eq!(sum_pairs(&[], 66), None)
}

#[test]
fn test6() {
    assert_eq!(sum_pairs(&[1, 2, 3, 4, 5, 6, 7, 8, 9], 13), Some((6, 7)))
}

#[test]
fn test7() {
    assert_eq!(sum_pairs(&[1, -1], 0), Some((1, -1)))
}

#[test]
fn test8() {
    assert_eq!(sum_pairs(&[-10, 10, -9, 9, -8, 8, -7, 7, -6, 6, -5, 5], 0), Some((-10, 10)))
}

#[test]
fn test9() {
    assert_eq!(sum_pairs(&[-10, 10, -9, 9, -8, 7, -7, 7, -6, 6, -5, 5], -1), Some((-10, 9)))
}