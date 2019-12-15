extern crate regex;
use regex::Regex;

fn change(s: &str, prog: &str, version: &str) -> String {    
    let re_version = Regex::new(r"Version: (\d+\.\d+)\n").unwrap();
    let re_phone   = Regex::new(r"Phone: \+1-\d{3}-\d{3}-\d{4}\n").unwrap();

    if !re_version.is_match(s) || !re_phone.is_match(s) {
        return "ERROR: VERSION or PHONE".to_string();
    }

    let prev_version = re_version.captures(s)
                                 .unwrap()
                                 .get(1)
                                 .map_or("", |m| m.as_str());

    let version = if prev_version == "2.0" { prev_version } else { version };

    format!(
        "Program: {} \
         Author: g964 \
         Phone: +1-503-555-0090 \
         Date: 2019-01-01 \
         Version: {}", 
        prog, 
        version
    )
}


#[test]
fn test0() {
    assert_eq!(change("Program title: Primes\nAuthor: Kern\nCorporation: Gold\nPhone: +1-503-555-0091\nDate: Tues April 9, 2005\nVersion: 6.7\nLevel: Alpha", "Ladder", "1.1"), "Program: Ladder Author: g964 Phone: +1-503-555-0090 Date: 2019-01-01 Version: 1.1")
}

#[test]
fn test1() {
    assert_eq!(change("Program title: Primes\nAuthor: Kern\nCorporation: Gold\nPhone: +1-503-555-0091\nDate: Tues April 9, 2005\nVersion: 2.1.3.7\nLevel: Alpha", "Ladder", "1.1"), "ERROR: VERSION or PHONE")
}

#[test]
fn test2() {
    assert_eq!(change("Program title: Primes\nAuthor: Kern\nCorporation: Gold\nPhone: +1-2137-555-0091\nDate: Tues April 9, 2005\nVersion: 6.9\nLevel: Alpha", "Ladder", "1.1"), "ERROR: VERSION or PHONE")
}

#[test]
fn test3() {
    assert_eq!(change("Program title: Primes\nAuthor: Kern\nCorporation: Gold\nPhone: +1-503-555-0091\nDate: Tues April 9, 2005\nVersion: 2173\nLevel: Alpha", "Ladder", "1.1"), "ERROR: VERSION or PHONE")
}

#[test]
fn test4() {
    assert_eq!(change("Program title: Primes\nAuthor: Kern\nCorporation: Gold\nPhone: +1-2137-555-0091\nDate: Tues April 9, 2005\nVersion: 6.9\nLevel: Alpha", "Ladder", "1.1"), "ERROR: VERSION or PHONE")
}

#[test]
fn test5() {
    assert_eq!(change("Program title: Primes\nAuthor: Kern\nCorporation: Gold\nPhone: +1-503-555-2137\nDate: Tues April 9, 2005\nVersion: 21.37\nLevel: Alpha", "Program", "66.6"), "Program: Program Author: g964 Phone: +1-503-555-0090 Date: 2019-01-01 Version: 66.6")
}

#[test]
fn test6() {
    assert_eq!(change("Program title: PRIMEEEEES\nAuthor: Kern\nCorporation: Gold\nPhone: +1-503-555-2137\nDate: Tues April 9, 2005\nVersion: 2.137\nLevel: Alpha", "xD", "6.66"), "Program: xD Author: g964 Phone: +1-503-555-0090 Date: 2019-01-01 Version: 6.66")
}

#[test]
fn test7() {
    assert_eq!(change("Program title: PRIMEEEEES\nAuthor: Kern\nCorporation: Gold\nPhone: 1-503-555-2137\nDate: Tues April 9, 2005\nVersion: 2.137\nLevel: Alpha", "xD", "6.66"), "ERROR: VERSION or PHONE")
}

#[test]
fn test8() {
    assert_eq!(change("Program title: PRIMEEEEES\nAuthor: Kern\nCorporation: Gold\nPhone: +1-555-2137\nDate: Tues April 9, 2005\nVersion: 2.137\nLevel: Alpha", "xD", "6.66"), "ERROR: VERSION or PHONE")
}

#[test]
fn test9() {
    assert_eq!(change("Program title: PRIMEEEEES\nAuthor: Kern\nCorporation: Gold\nPhone: +1-555-2137\nDate: Tues April 9, 2005\nVersion: .\nLevel: Alpha", "xD", "6.66"), "ERROR: VERSION or PHONE")
}