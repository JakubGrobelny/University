fn dec2_fact_string(nb: u64) -> String {
    fn to_digit(n: u64) -> char {
        if n < 10 {
            (n as u8 + '0' as u8) as char
        } else {
            (n as u8 - 10 + 'A' as u8) as char
        }
    }

    let mut factorials = vec![1];
    loop {
        let last = *factorials.last().unwrap();
        let n = factorials.len();
        let fact = last * n as u64;

        if fact > nb {
            break;
        }
        factorials.push(fact);
    }

    let mut num = nb;
    let mut res = String::new();

    for fact in factorials.iter().skip(1).rev() {
        let digit = num / fact;
        num -= fact * digit;
        res.push(to_digit(digit));
    }

    res.push('0');
    return res
}

fn fact_string_2dec(s: String) -> u64 {
    fn from_digit(c: char) -> u64 {
        (match c {
            '0'..='9' => c as u8 - '0' as u8,
            _ => c as u8 - 'A' as u8 + 10
        }) as u64
    }

    s.chars().rev().fold((0, 1, 1), |(sum, fact, n), c| {
        let digit_value = from_digit(c);
        (sum + fact * digit_value, n * fact, n + 1)
    }).0
}

#[test]
fn test0() {
    assert_eq!(dec2_fact_string(120), "100000")
}

#[test]
fn test1() {
    assert_eq!(dec2_fact_string(1), "10")
}

#[test]
fn test2() {
    assert_eq!(dec2_fact_string(1254), "1421000")
}

#[test]
fn test3() {
    assert_eq!(dec2_fact_string(2982), "4041000")
}

#[test]
fn test4() {
    assert_eq!(dec2_fact_string(45365346), "115010223000")
}

#[test]
fn test5() {
    assert_eq!(fact_string_2dec("100000".to_string()), 120)
}

#[test]
fn test6() {
    assert_eq!(fact_string_2dec("10".to_string()), 1)
}

#[test]
fn test7() {
    assert_eq!(fact_string_2dec("1421000".to_string()), 1254)
}

#[test]
fn test8() {
    assert_eq!(fact_string_2dec("4041000".to_string()), 2982)
}

#[test]
fn test9() {
    assert_eq!(fact_string_2dec("2137420".to_string()), 1684)
}
