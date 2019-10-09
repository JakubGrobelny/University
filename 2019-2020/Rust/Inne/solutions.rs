// https://www.codewars.com/kata/57cebe1dc6fdc20c57000ac9
fn find_short(s: &str) -> u32 {
    s.split_whitespace().map(|s| -> u32 {s.len() as u32}).min().unwrap()
}

// https://www.codewars.com/kata/55fd2d567d94ac3bc9000064
fn row_sum_odd_numbers(n:i64) -> i64 {
    let first = n*n - (n-1);
    let last = first + (n-1)*2;
    (first + last) * n / 2
}

// https://www.codewars.com/kata/51e0007c1f9378fa810002a9/train/rust
fn parse(code: &str) -> Vec<i32> {    
    let mut results : Vec<i32> = Vec::new();
    let mut value = 0;

    for c in code.chars() {
        match c {
            'i' => value += 1,
            'd' => value -= 1,
            's' => value *= value,
            'o' => results.push(value),
            _   => (),
        }
    }

    results
}

// https://www.codewars.com/kata/5467e4d82edf8bbf40000155/train/rust
fn descending_order(x: u64) -> u64 {
    let mut chars_vec : Vec<char> = x.to_string().chars().collect();
    chars_vec.sort_by(|lhs, rhs| rhs.cmp(lhs));
    let sorted_str : String = chars_vec.iter().collect();
    sorted_str.parse::<u64>().unwrap()
}

// https://www.codewars.com/kata/55bf01e5a717a0d57e0000ec/train/rust
fn persistence(num: u64) -> u64 {

    fn mult_digits(num: u64) -> u64 {

        fn to_digit(c: char) -> u64 {
            c.to_digit(10).unwrap() as u64
        }

        num.to_string().chars().map(to_digit).product()
    }

    if num < 10 { 0 } else { 1 + persistence(mult_digits(num)) }
}

// https://www.codewars.com/kata/get-the-middle-character/train/rust
fn get_middle(s: &str) -> &str {
    &s[s.len()/2 - if s.len() % 2 == 0 { 1 } else { 0 } .. s.len()/2 + 1]
}

// https://www.codewars.com/kata/create-phone-number/train/rust
fn create_phone_number(numbers: &[u8]) -> String {
    fn to_digit(i: &u8) -> char {
        std::char::from_digit(*i as u32, 10).unwrap()
    }

    let nums : String = numbers.iter().map(to_digit).collect();
    format!("({}) {}-{}", &nums[0..3], &nums[3..6], &nums[6..10])
}

// https://www.codewars.com/kata/bit-counting/train/rust
fn count_bits(n: i64) -> u32 {
    n.count_ones()
}

// https://www.codewars.com/kata/century-from-year/train/rust
fn century(year: u32) -> u32 {
    (year - 1) / 100 + 1
}

// https://www.codewars.com/kata/multiples-of-3-or-5/train/rust
fn solution(num: i32) -> i32 {
    let num0 = num - 1;
    let threes = (3 + (num0 - num0%3)) * (num0/3) / 2;
    let fives = (5 + (num0 - num0%5)) * (num0/5) / 2;
    let fifteens = (15 + (num0 - num0%15)) * (num0/15) / 2;
    threes + fives - fifteens
}

// https://www.codewars.com/kata/even-or-odd/train/rust
fn even_or_odd(i: i32) -> &'static str {
    if i % 2 == 0 { "Even" } else { "Odd" }
}

// https://www.codewars.com/kata/opposite-number/train/rust
fn opposite(number: i32) -> i32 {
    -number
}

// https://www.codewars.com/kata/sum-of-positive/train/rust
fn positive_sum(arr: &[i32]) -> i32 {
    arr.iter().fold(0, |prev, a| if *a > 0 {prev + *a} else {prev})
}

// https://www.codewars.com/kata/find-the-smallest-integer-in-the-array/train/rust
fn find_smallest_int(arr: &[i32]) -> i32 {
    *arr.iter().min().unwrap()
}

// https://www.codewars.com/kata/remove-string-spaces/train/rust
fn no_space(x : String) -> String{
    x.chars().filter(|c| *c != ' ').collect()
}

// https://www.codewars.com/kata/highest-and-lowest/train/rust
fn high_and_low(numbers: &str) -> String {
    let nums : Vec<i32> = numbers.split_whitespace().map(|n| n.parse::<i32>().unwrap()).collect();
    let min = nums.iter().min().unwrap();
    let max = nums.iter().max().unwrap();
    format!("{} {}", max, min)
}

// https://www.codewars.com/kata/grasshopper-terminal-game-move-function/train/rust
fn move_hero(position: u32, roll: u32) -> u32 {
    position + 2 * roll
}

// https://www.codewars.com/kata/valid-braces/train/rust
fn valid_braces(s: &str) -> bool {
    let mut stack : Vec<char> = Vec::new();

    fn matching(co: char, cc: char) -> bool {
        (co == '(' && cc == ')') ||
        (co == '[' && cc == ']') ||
        (co == '{' && cc == '}')
    }

    for bracket in s.chars() {
        match bracket {
            '(' | '[' | '{' => stack.push(bracket),
            _ => match stack.pop() {
                None => return false,
                Some(opening_bracket) =>
                    if !matching(opening_bracket, bracket) { return false }
            },
        }
    }
    
    stack.is_empty()
}