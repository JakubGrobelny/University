fn good_vs_evil(good: &str, evil: &str) -> String {
    fn calculate_strength(counts: &str, values: &[i32]) -> i32 {
        counts.split(' ')
              .map(|s| s.parse::<i32>().unwrap())
              .zip(values.iter())
              .fold(0, |acc, (count, value)| acc + count * value)
    }

    let worth_good = [1, 2, 3, 3, 4, 10   ];
    let worth_evil = [1, 2, 2, 2, 3, 5, 10];

    let good = calculate_strength(good, &worth_good);
    let evil = calculate_strength(evil, &worth_evil);

    if good > evil {
        "Battle Result: Good triumphs over Evil".to_string()
    } else if evil > good {
        "Battle Result: Evil eradicates all trace of Good".to_string()
    } else {
        "Battle Result: No victor on this battle field".to_string()
    }
}

const GOOD : &str = "Battle Result: Good triumphs over Evil";
const EVIL : &str = "Battle Result: Evil eradicates all trace of Good";
const DRAW : &str = "Battle Result: No victor on this battle field";

#[test]
fn test0() {
    assert_eq!(
        good_vs_evil("0 0 0 0 0 10", "0 0 0 0 0 0 0"), 
        GOOD.to_string()
    )
}

#[test]
fn test1() {
    assert_eq!(
        good_vs_evil("0 0 0 0 0 0", "0 0 0 0 0 0 10"), 
        EVIL.to_string()
    )
}

#[test]
fn test2() {
    assert_eq!(
        good_vs_evil("0 0 0 0 0 10", "0 0 0 0 0 0 10"), 
        DRAW.to_string()
    )
}

#[test]
fn test3() {ood: &str, 
    assert_eq!(
        good_vs_evil("0 0 0 0 0 0", "0 0 0 0 0 0 0"),
        DRAW.to_string()
    )

}

#[test]
fn test4() {
    assert_eq!(
        good_vs_evil("1 1 1 1 1 1", "1 1 1 1 1 1 1"),
        EVIL.to_string()
    )
}

#[test]
fn test5() {
    assert_eq!(
        good_vs_evil("2 2 2 2 2 2", "1 1 1 1 1 1 1"),
        GOOD.to_string()
    )
}

#[test]
fn test6() {
    assert_eq!(
        good_vs_evil("2 1 3 7 0 0", "0 2 1 3 7 0 0"),
        GOOD.to_string()
    )
}

#[test]
fn test7() {
    assert_eq!(
        good_vs_evil("0 2 1 3 7 0", "2 1 3 7 0 0 0"),
        GOOD.to_string()
    )
}

#[test]
fn test8() {
    assert_eq!(
        good_vs_evil("2 1 3 7 5 5", "5 2 1 3 7 0 5"),
        GOOD.to_string()
    )
}

#[test]
fn test9() {
    assert_eq!(
        good_vs_evil("0 0 0 0 0 1000", "0 0 0 0 0 0 1000"),
        DRAW.to_string()
    )
}


