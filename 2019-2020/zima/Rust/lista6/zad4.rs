fn comp(a: Vec<i64>, b: Vec<i64>) -> bool {
    if a.len() != b.len() { return false }

    let mut a0 = a.iter().map(|x| x*x).collect::<Vec<i64>>();
    let mut b0 = b.clone();
    a0.sort();
    b0.sort();

    a0.iter().zip(b0.iter()).all(|(a,b)| a == b)
}

#[test]
fn test0() {
    assert_eq!(comp(vec![], vec![]), true);
}

#[test]
fn test1() {
    assert_eq!(comp(vec![1,2,3], vec![1,4,9]), true);
}

#[test]
fn test2() {
    assert_eq!(comp(vec![1,1,2], vec![1,4]), false);
}

#[test]
fn test3() {
    assert_eq!(comp(vec![8, 8, 8, 8], vec![64, 64, 64, 64]), true);
}

#[test]
fn test4() {
    assert_eq!(comp(vec![8,1,4], vec![1, 16, 64]), true);
}

#[test]
fn test5() {
    assert_eq!(comp(vec![-1, -2, -3, -4], vec![16, 4, 1, 9]), true)
}

#[test]
fn test6() {
    assert_eq!(comp(vec![-2,-2,2], vec![4,4,4]), true)
}

#[test]
fn test7() {
    assert_eq!(comp(vec![-5, 5], vec![25]), false);
}

#[test]
fn test8() {
    assert_eq!(comp(vec![4,2,4,2,4], vec![16,4,16,4,16]), true);
}

#[test]
fn test9() {
    assert_eq!(comp(vec![4,2,4,2,4], vec![4,16,16,4,16]), true);
}
