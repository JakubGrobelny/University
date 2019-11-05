fn dbl_linear(n: u32) -> u32 {
    let n = n as usize;
    let mut u: Vec<u32> = vec![1];
    let mut y: usize = 0;
    let mut z: usize = 0;

    while y <= n && z <= n {
        let next_y = 2 * u[y] + 1;
        let next_z = 3 * u[z] + 1;

        if next_y < next_z {
            u.push(next_y);
            y += 1;
        } else if next_y == next_z {
            u.push(next_y);
            y += 1;
            z += 1;
        } else {
            u.push(next_z);
            z += 1;
        }
    }

    u[n]
}

#[test]
fn test0() {
    assert_eq!(dbl_linear(0), 1)
}

#[test]
fn test1() {
    assert_eq!(dbl_linear(1), 3)
}

#[test]
fn test2() {
    assert_eq!(dbl_linear(100), 447)
}

#[test]
fn test3() {
    assert_eq!(dbl_linear(11), 27)
}


#[test]
fn test5() {
    assert_eq!(dbl_linear(377), 2325)
}

#[test]
fn test6() {
    assert_eq!(dbl_linear(42), 136)
}

#[test]
fn test7() {
    assert_eq!(dbl_linear(13), 31)
}

#[test]
fn test8() {
    assert_eq!(dbl_linear(338), 2095)
}

#[test]
fn test9() {
    assert_eq!(dbl_linear(255), 1467)
}















