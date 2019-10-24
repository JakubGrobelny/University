fn last_digit(str1: &str, str2: &str) -> i32 {

    if str2 == "0" { return 1 };

    fn modulo4(num: &str) -> u32 {
        num.chars().fold(0, |res, c| {
            let n = c.to_digit(10).unwrap();
            (res*10 + n) % 4
        })
    }

    let last_a : u32 = str1.chars().last().unwrap().to_digit(10).unwrap();
    let power  : u32 = modulo4(str2);
    let power0 = if power == 0 { 4 } else { power };

    (last_a.pow(power0) % 10) as i32
}

#[test]
fn test0() {
  assert_eq!(last_digit("23", "3"), 7);
}

#[test]
fn test1() {
  assert_eq!(last_digit("5", "9"), 5);
}

#[test]
fn test2() {
  assert_eq!(last_digit("0", "0"), 1);
}

#[test]
fn test3() {
  assert_eq!(last_digit("100", "200"), 0);
}

#[test]
fn test4() {
  assert_eq!(last_digit("200", "100"), 0);
}

#[test]
fn test5() {
  assert_eq!(last_digit("111", "69"), 1);
}

#[test]
fn test6() {
  assert_eq!(last_digit("3", "8"), 1);
}

#[test]
fn test7() {
  assert_eq!(last_digit("878", "35243545"), 8);
}

#[test]
fn test8() {
  assert_eq!(last_digit("4534534", "123123123"), 4);
}

#[test]
fn test9() {
  assert_eq!(last_digit("876878", "1313232"), 6);
}