pub fn highlight(code: &str) -> String {
    if (code.len() == 0) { return String::new() }
    let mut count = 0;
    let mut c = ' ';
    for (prev, c) in code.chars().zip(code.chars().next) {
        c = prev;
        if prev != c { break; }
        count += 1;
    }

    fn match_color(c: char) -> String {
        match 
    }
}