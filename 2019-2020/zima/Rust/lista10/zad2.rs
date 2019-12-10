fn stock_list(list_art: Vec<&str>, list_cat: Vec<&str>) -> String {
    if list_art.len() == 0 || list_cat.len() == 0 {
        return String::new();
    }

    list_cat.iter().map(|category| {
        let count = list_art.iter()
            .filter(|book| &&book[0..1] == category)
            .map(|w| w.split_whitespace()
                      .map(|x| x.parse::<usize>().unwrap_or(0))
                      .sum::<usize>())
            .sum::<usize>();
        format!("({} : {})", category, count)
    }).collect::<Vec<String>>()
      .join(" - ")
}

#[test]
fn test0() {
    assert_eq!(
        stock_list(vec!["ABC 10", "BCD 20", "ACB 30"], vec!["A"]), 
        "(A : 40)"
    )
}

#[test]
fn test1() {
    assert_eq!(
        stock_list(vec!["ABC 10", "BCD 20", "ACB 30"], vec!["B"]), 
        "(B : 20)"
    )
}

#[test]
fn test2() {
    assert_eq!(
        stock_list(vec!["ABC 10", "BCD 20", "ACB 30"], vec![]), 
        ""
    )
}

#[test]
fn test3() {
    assert_eq!(
        stock_list(vec!["ABC 10", "BCD 20", "ACB 30"], vec!["A", "B"]), 
        "(A : 40) - (B : 20)"
    )
}

#[test]
fn test4() {
    assert_eq!(
        stock_list(vec!["ABC 10", "BCD 20", "ACB 30", "CCC 5", "BAC 80"], vec!["C", "B"]), 
        "(C : 5) - (B : 100)"
    )
}

#[test]
fn test5() {
    assert_eq!(stock_list(vec![], vec!["A", "B", "C"]), "")
}

#[test]
fn test6() {
    assert_eq!(stock_list(vec![], vec![]), "")
}

#[test]
fn test7() {
    assert_eq!(stock_list(vec!["KQWE 13", "KOT 7", "KOC 22"], vec!["K"]), "(K : 42)")
}

#[test]
fn test8() {
    assert_eq!(stock_list(vec!["KQWE 13", "PAPIEZ 2000", "KOT 7", "PAWEL 100","KOC 22", "PP 37"], vec!["P", "K"]), "(P : 2137) - (K : 42)")
}

#[test]
fn test9() {
    assert_eq!(stock_list(vec!["KQWE 13", "PAPIEZ 2000", "KOT 7", "PAWEL 100","KOC 22", "PP 37"], vec!["K","P"]), "(K : 42) - (P : 2137)")
}
