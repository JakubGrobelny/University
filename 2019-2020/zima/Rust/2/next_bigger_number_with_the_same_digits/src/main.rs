fn next_bigger_number(n: i64) -> i64 {
    let mut digits : Vec<char> = n.to_string().chars().collect();

    fn is_sorted_desc(digits: &Vec<char>) -> bool {
        for i in 1..digits.len() {
            if digits[i-1] < digits[i] { return false };
        }
        true
    }

    fn rightmost_smaller(digits: &Vec<char>) -> usize {
        for i in (0..digits.len()-1).rev() {
            if digits[i] < digits[i+1] {
                return i;
            }
        }
        0
    }

    if n < 10 || is_sorted_desc(&digits) { 
        -1 
    } else {        
        let first_smaller = rightmost_smaller(&digits);
        let mut first_greater = first_smaller+1;
        
        for i in first_smaller+1..digits.len() {
            if digits[i] > digits[first_smaller] && 
               digits[first_greater] > digits[i] {
                first_greater = i;
            }
        }

        let temp = digits[first_smaller];
        digits[first_smaller] = digits[first_greater];
        digits[first_greater] = temp;

        let mut digits_last : Vec<char> = digits[first_smaller+1..].to_vec();
        digits_last.sort();
                
        for i in first_smaller+1..digits.len() {
            digits[i] = digits_last[i - first_smaller - 1];
        }
        digits.iter().collect::<String>().parse::<i64>().unwrap_or(-1)
    }
}


#[test]
fn test0() {
    assert_eq!(next_bigger_number(0), -1)
}

#[test]
fn test1() {
    assert_eq!(next_bigger_number(1234), 1243)
}

#[test]
fn test2() {
    assert_eq!(next_bigger_number(4321), -1)
}

#[test]
fn test3() {
    assert_eq!(next_bigger_number(21), -1)
}

#[test]
fn test4() {
    assert_eq!(next_bigger_number(12), 21)
}

#[test]
fn test5() {
    assert_eq!(next_bigger_number(5823), 5832)
}

#[test]
fn test6() {
    assert_eq!(next_bigger_number(9989), 9998)
}

#[test]
fn test7() {
    assert_eq!(next_bigger_number(10_000_001), 10_000_010)
}

#[test]
fn test8() {
    assert_eq!(next_bigger_number(144), 414);
}

#[test]
fn test9() {
    assert_eq!(next_bigger_number(9899), 9989)
}