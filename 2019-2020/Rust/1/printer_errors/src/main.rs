fn printer_error(s: &str) -> String {
    let wrong : String = s.chars().filter(|&c| -> bool { c > 'm' }).collect();
    wrong.len().to_string() + "/" + &s.len().to_string()
}

#[test]
fn test0() {
    assert_eq!(&printer_error("m"), "0/1");
}

#[test]
fn test1() {
    assert_eq!(&printer_error("mabc"), "0/4");
}

#[test]
fn test2() {
    assert_eq!(&printer_error("maaaaaaaaaaabbbbbbbbbbbbbbbbzzz"), "3/31");
}

#[test]
fn test3() {
    assert_eq!(&printer_error("abcdefgh"), "0/8");
}

#[test]
fn test4() {
    assert_eq!(&printer_error("nnooppzz"), "8/8");
}