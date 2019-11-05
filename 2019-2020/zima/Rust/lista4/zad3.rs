pub fn highlight(code: &str) -> String {
    if code == "" { 
        return String::new()
    }

    fn append_opening_tag(s: &String, c: char, closed: bool) -> (String, bool) {
        let color = match c {
            'F' => "<span style=\"color: pink\">",
            'L' => "<span style=\"color: red\">",
            'R' => "<span style=\"color: green\">",
            '0'..='9' => "<span style=\"color: orange\">",
            _ => ""
        };

        (format!("{}{}{}{}", s, (if closed {""} else {"</span>"}), color, c), color == "")
    }

    let (res, closed, _) = code.chars().fold((String::new(), true, '\0'), 
        {|(acc, closed, prev), c| 
            if prev == c || prev.is_digit(10) && c.is_digit(10) {
                (format!("{}{}", acc, c), closed, c)
            } else {
                let (acc, closed) = append_opening_tag(&acc, c, closed);
                (acc, closed, c)
            }
        });

    if closed { res } else { format!("{}</span>", res) }
}

#[test]
fn test0() {
    assert_eq!(highlight(""), "")
}

#[test]
fn test1() {
    assert_eq!(highlight("()()("), "()()(")
}

#[test]
fn test2() {
    assert_eq!(highlight("0123456789"), "<span style=\"color: orange\">0123456789</span>")
}

#[test]
fn test3() {
    assert_eq!(highlight("012()F"), "<span style=\"color: orange\">012</span>()<span style=\"color: pink\">F</span>")
}

#[test]
fn test4() {
    assert_eq!(highlight("(0F0L0R)"), "(<span style=\"color: orange\">0</span><span style=\"color: pink\">F</span><span style=\"color: orange\">0</span><span style=\"color: red\">L</span><span style=\"color: orange\">0</span><span style=\"color: green\">R</span>)")
}

#[test]
fn test5() {
    assert_eq!(highlight("(1FFF230F0L0RRR)R"), "(<span style=\"color: orange\">1</span><span style=\"color: pink\">FFF</span><span style=\"color: orange\">230</span><span style=\"color: pink\">F</span><span style=\"color: orange\">0</span><span style=\"color: red\">L</span><span style=\"color: orange\">0</span><span style=\"color: green\">RRR</span>)<span style=\"color: green\">R</span>")
}

#[test]
fn test6() {
    assert_eq!(highlight("F40)3F0347L76)L6"), "<span style=\"color: pink\">F</span><span style=\"color: orange\">40</span>)<span style=\"color: orange\">3</span><span style=\"color: pink\">F</span><span style=\"color: orange\">0347</span><span style=\"color: red\">L</span><span style=\"color: orange\">76</span>)<span style=\"color: red\">L</span><span style=\"color: orange\">6</span>")
}

#[test]
fn test7() {
    assert_eq!(highlight(")(5L8(L862)R4FR2"), ")(<span style=\"color: orange\">5</span><span style=\"color: red\">L</span><span style=\"color: orange\">8</span>(<span style=\"color: red\">L</span><span style=\"color: orange\">862</span>)<span style=\"color: green\">R</span><span style=\"color: orange\">4</span><span style=\"color: pink\">F</span><span style=\"color: green\">R</span><span style=\"color: orange\">2</span>")
}

#[test]
fn test8() {
    assert_eq!(highlight("0R6L6695)4R03151"), "<span style=\"color: orange\">0</span><span style=\"color: green\">R</span><span style=\"color: orange\">6</span><span style=\"color: red\">L</span><span style=\"color: orange\">6695</span>)<span style=\"color: orange\">4</span><span style=\"color: green\">R</span><span style=\"color: orange\">03151</span>")
}

#[test]
fn test9() {
    assert_eq!(highlight("(8R))9(L88)57095"), "(<span style=\"color: orange\">8</span><span style=\"color: green\">R</span>))<span style=\"color: orange\">9</span>(<span style=\"color: red\">L</span><span style=\"color: orange\">88</span>)<span style=\"color: orange\">57095</span>")
}