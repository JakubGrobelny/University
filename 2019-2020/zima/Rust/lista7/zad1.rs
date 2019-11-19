fn chessboard_cell_color(cell1: &str, cell2: &str) -> bool {
    fn cell_to_pair(cell: &str) -> (u8, u8) {
        let first = cell.chars().nth(0).unwrap() as u8 - 'A' as u8 + 1;
        let second = cell.chars().nth(1).and_then(|c| c.to_digit(10)).unwrap();
        (first, second  as u8)
    }
    
    let first = cell_to_pair(cell1);
    let second = cell_to_pair(cell2);

    (first.0 + first.1) % 2 == (second.0 + second.1) % 2
}

#[test]
fn test0() {
    assert_eq!(chessboard_cell_color("C3", "A1"), true);
}

#[test]
fn test1() {
    assert_eq!(chessboard_cell_color("A1", "H3"), false);
}

#[test]
fn test2() {
    assert_eq!(chessboard_cell_color("A1", "A2"), false);
}

#[test]
fn test3() {
    assert_eq!(chessboard_cell_color("A1", "C1"), true);
}

#[test]
fn test4() {
    assert_eq!(chessboard_cell_color("A1", "A1"), true);
}

#[test]
fn test5() {
    assert_eq!(chessboard_cell_color("C4", "D3"), true);
}

#[test]
fn test6() {
    assert_eq!(chessboard_cell_color("B5", "F7"), true);
}

#[test]
fn test7() {
    assert_eq!(chessboard_cell_color("B5", "F6"), false);
}

#[test]
fn test8() {
    assert_eq!(chessboard_cell_color("H8", "A1"), true);
}

#[test]
fn test9() {
    assert_eq!(chessboard_cell_color("G8", "G7"), false);
}