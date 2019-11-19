fn fcn(n: i32) -> i64 {
    // 1i64 << n
    let mut u : Vec<i64> = vec![1, 2];
    for i in 2..=(n as usize) {
        let un0 = u[i-2] as i128;
        let un1 = u[i-1] as i128;
        let un = (6*un0*un1) / (5*un0-un1);
        u.push(un as i64);
    }

    u[n as usize]
}

#[test]
fn test0() {
    assert_eq!(fcn(0), 1)
}

#[test]
fn test1() {
    assert_eq!(fcn(1), 2)
}

#[test]
fn test2() {
    assert_eq!(fcn(2), 4)
}

#[test]
fn test3() {
    assert_eq!(fcn(5), 32)
}

#[test]
fn test4() {
    assert_eq!(fcn(9), 1i64 << 9)
}

#[test]
fn test5() {
    assert_eq!(fcn(17), 1i64 << 17)    
}

#[test]
fn test6() {
    assert_eq!(fcn(21), 1i64 << 21)
}

#[test]
fn test7() {
    assert_eq!(fcn(11), 1i64 << 11)
}

#[test]
fn test8() {
    assert_eq!(fcn(25), 1i64 << 25)
}

#[test]
fn test9() {
    assert_eq!(fcn(30), 1i64 << 30)
}