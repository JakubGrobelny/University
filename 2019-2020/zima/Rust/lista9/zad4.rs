fn camel_case(words: &str) -> String {
    words.split(" ")
         .filter(|&w| w != "")
         .map(|w| {
            let first = w.chars()
                         .next()
                         .unwrap()
                         .to_ascii_uppercase();
            let rest = w.get(1..).unwrap();
            format!("{}{}", first, rest)
       }).collect::<Vec<String>>().join("")
}

#[test]
fn test0() {
    assert_eq!(camel_case("hello world"), "HelloWorld")
}

#[test]
fn test1() {
    assert_eq!(camel_case("camel"), "Camel")
}

#[test]
fn test2() {
    assert_eq!(camel_case("camel case word"), "CamelCaseWord")
}

#[test]
fn test3() {
    assert_eq!(camel_case("abc 123"), "Abc123")
}

#[test]
fn test4() {
    assert_eq!(camel_case("a b c d e f"), "ABCDEF")
}

#[test]
fn test5() {
    assert_eq!(camel_case("a 1b 2c"), "A1b2c")
}

#[test]
fn test6() {
    assert_eq!(camel_case(""), "")
}

#[test]
fn test7() {
    assert_eq!(camel_case("CamelCase"), "CamelCase")
}

#[test]
fn test8() {
    assert_eq!(camel_case("CamelCase CamelCase"), "CamelCaseCamelCase")
}

#[test]
fn test9() {
    assert_eq!(camel_case("snake_case > CamelCase"), "Snake_case>CamelCase")
}
