fn even_numbers(array: &Vec<i32>, number: usize) -> Vec<i32> {
    let mut temp = array.iter()
                        .filter(|x| **x % 2 == 0)
                        .map(|x| *x)
                        .rev()
                        .take(number)
                        .collect::<Vec<i32>>();
    temp.reverse();
    temp
}

#[test]
fn test0() {
    assert_eq!(even_numbers(&vec!(1, 2, 3, 4, 5, 6, 7, 6, 9), 3), vec!(4, 6, 6));
}

#[test]
fn test1() {
    assert_eq!(even_numbers(&vec!(1, 2, 3, 4, 5, 6, 7), 3), vec!(2, 4, 6));
}

#[test]
fn test2() {
    assert_eq!(even_numbers(&vec!(2,4,6), 0), vec!());
}

#[test]
fn test3() {
    assert_eq!(even_numbers(&vec!(2,4,6), 2), vec!(4, 6));
}

#[test]
fn test4() {
    assert_eq!(even_numbers(&vec!(), 0), vec!()); 
}

#[test]
fn test5() {
    assert_eq!(even_numbers(&vec!(2,1,1,1,1,1), 1), vec!(2));
}

#[test]
fn test6() {
    assert_eq!(even_numbers(&vec!(2,1,4,1,1,1), 1), vec!(4));
}

#[test]
fn test7() {
    assert_eq!(even_numbers(&vec!(6,6,6,6), 3), vec!(6,6,6));
}

#[test]
fn test8() {
    assert_eq!(even_numbers(&vec!(6,-6,6,-6), 3), vec!(-6,6,-6));
}

#[test]
fn test9() {
    assert_eq!(even_numbers(&vec!(6,-3,6,-6), 3), vec!(6,6,-6));
}
