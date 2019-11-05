fn solution(x: f64) -> f64 {
    (x * 2.0).round() / 2.0
}

#[test]
fn test0() {
    assert_eq!(solution(3.0), 3.0)
}

#[test]
fn test1() {
    assert_eq!(solution(3.5), 3.5)
}

#[test]
fn test2() {
    assert_eq!(solution(3.7), 3.5)
}

#[test]
fn test3() {
    assert_eq!(solution(3.24), 3.0)
}

#[test]
fn test4() {
    assert_eq!(solution(3.27), 3.5)
}

#[test]
fn test5() {
    assert_eq!(solution(3.76), 4.0)
}

#[test]
fn test6() {
    assert_eq!(solution(-0.3), -0.5)
}

#[test]
fn test7() {
    assert_eq!(solution(-0.6), -0.5)
}

#[test]
fn test8() {
    assert_eq!(solution(-0.8), -1.0)
}

#[test]
fn test9() {
    assert_eq!(solution(5e-10), 0.0)
}

