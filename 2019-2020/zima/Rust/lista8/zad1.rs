fn dna_strand(dna: &str) -> String {
    dna.chars().map(|c| match c {
        'A' => 'T',
        'C' => 'G',
        'T' => 'A',
        'G' => 'C',
        _   => c
    }).collect::<String>()
}

#[test]
fn test0() {
    assert_eq!(dna_strand(""), String::from(""))
}

#[test]
fn test1() {
    assert_eq!(dna_strand("ACTG"), String::from("TGAC"))
}

#[test]
fn test2() {
    assert_eq!(dna_strand("TGAC"), String::from("ACTG"))
}

#[test]
fn test3() {
    assert_eq!(dna_strand("AATT"), String::from("TTAA"))
}

#[test]
fn test4() {
    assert_eq!(dna_strand("AAAACCTG"), String::from("TTTTGGAC"))
}

#[test]
fn test5() {
    assert_eq!(dna_strand("GTAT"), String::from("CATA"))
}

#[test]
fn test6() {
    assert_eq!(dna_strand("GTATTTTTT"), String::from("CATAAAAAA"))
}

#[test]
fn test7() {
    assert_eq!(dna_strand("CATAAAAAA"), String::from("GTATTTTTT"))
}

#[test]
fn test8() {
    assert_eq!(dna_strand("invalid"), String::from("invalid"))
}

#[test]
fn test9() {
    assert_eq!(dna_strand("G1T1A1T"), String::from("C1A1T1A"))
}