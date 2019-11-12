fn print(n: i32) -> Option<String> {
    if n % 2 == 0 || n < 0 { 
        return None;
    }
    
    let mut res = String::new();

    for row in 0..n {
        let spaces = (n/2 - row).abs() as usize;
        let stars = n as usize - spaces * 2;
        res.push_str(&" ".repeat(spaces));
        res.push_str(&"*".repeat(stars));
        res.push('\n');
    }

    Some(res)
}


fn test_str(s: &str) -> Option<String> {
    Some(s.to_string())
}

#[test]
fn test0() {
    assert_eq!(print(0), None);
}

#[test]
fn test1() {
    assert_eq!(print(-2137), None);
}

#[test]
fn test2() {
    assert_eq!(print(42), None);
}

#[test]
fn test3() {
    assert_eq!(print(3), test_str(" *\n***\n *\n"));
}

#[test]
fn test4() {
    assert_eq!(print(5), test_str("  *\n ***\n*****\n ***\n  *\n"));
}

#[test]
fn test5() {
    assert_eq!(print(7), test_str("   *\n  ***\n *****\n*******\n *****\n  ***\n   *\n"));
}

#[test]
fn test6() {
    assert_eq!(print(1), test_str("*\n"));
}

#[test]
fn test7() {
    assert_eq!(print(-4), None);
}

#[test]
fn test8() {
    assert_eq!(print(9), test_str("    *\n   ***\n  *****\n *******\n*********\n *******\n  *****\n   ***\n    *\n"))
}

#[test]
fn test9() {
    assert_eq!(print(17), test_str("        *\n       ***\n      *****\n     *******\n    *********\n   ***********\n  *************\n ***************\n*****************\n ***************\n  *************\n   ***********\n    *********\n     *******\n      *****\n       ***\n        *\n"));
}
