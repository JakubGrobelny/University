fn game(n: u64) -> Vec<u64> {
    let n_squared = n*n;
    if n_squared % 2 == 0 {
        vec![n_squared / 2]
    } else {
        vec![n_squared, 2]
    }
}

#[test]
fn test0() {
    assert_eq!(game(204), vec![20808])
}

#[test]
fn test1() {
    assert_eq!(game(807), vec![651249, 2])
}

#[test]
fn test2() {
    assert_eq!(game(5014), vec![12570098])
}

#[test]
fn test3() {
    assert_eq!(game(750001), vec![562501500001, 2])
}

#[test]
fn test4() {
    assert_eq!(game(2137), vec![4566769, 2])
}

#[test]
fn test5() {
    assert_eq!(game(21370), vec![228338450])
}

#[test]
fn test6() {
    assert_eq!(game(5435345), vec![29542975269025, 2])
}

#[test]
fn test7() {
    assert_eq!(game(80085), vec![6413607225, 2])
}

#[test]
fn test8() {
    assert_eq!(game(64561323), vec![4168164427510329, 2])
}

#[test]
fn test9() {
    assert_eq!(game(989798), vec![489850040402])
}

#[test]
fn test10() {
    assert_eq!(game(56456), vec![1593639968])
}
