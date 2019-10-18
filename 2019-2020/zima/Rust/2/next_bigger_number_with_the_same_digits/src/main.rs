fn next_bigger_number(n: i64) -> i64 {
    let mut characters : Vec<char> = n.to_string().chars().collect();

    fn is_sorted(vec: Vec<char>, cmp: &dyn Fn(char, char) -> bool) -> bool {
        for i in 1..vec.len() {
            if !cmp(vec[i], vec[i-1]) { return false }
        }
        true
    }

    if is_sorted(characters, &|a, b| a <= b) || n <= 9 { 
        -1 
    } else if is_sorted(characters, &|a, b| a >= b) {
        let temp = characters[characters.len()-1];
        characters[characters.len()-1] = characters[characters.len()-2];
        characters[characters.len()-2] = temp;
        characters.into_iter().collect::<String>().parse::<i64>().unwrap_or(-1)
    } else {
        -1
    }

}

#[test]
fn test0() {
    assert_eq!(next_bigger_number(0), -1)
}
