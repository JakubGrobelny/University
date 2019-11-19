fn john_and_ann(n: i32) -> (Vec<i32>, Vec<i32>) {
    let mut john : Vec<i32> = vec![0; n as usize];
    let mut ann  : Vec<i32> = vec![1; n as usize];

    for day in 1..n as usize {
        ann[day] = day as i32 - john[ann[day-1] as usize];
        john[day] = day as i32 - ann[john[day-1] as usize];
    }

    (john, ann)
}

fn john(n: i32) -> Vec<i32> {
    john_and_ann(n).0
}

fn ann(n: i32) -> Vec<i32> {
    john_and_ann(n).1
}

fn sum_john(n: i32) -> i32 {
    john(n).iter().sum()
}

fn sum_ann(n: i32) -> i32 {
    ann(n).iter().sum()
}

#[test]
fn test0() {
    assert_eq!(john(11), vec![0, 0, 1, 2, 2, 3, 4, 4, 5, 6, 6]);
}

#[test]
fn test1() {
    assert_eq!(john(14), vec![0, 0, 1, 2, 2, 3, 4, 4, 5, 6, 6, 7, 7, 8]);
}

#[test]
fn test2() {
    assert_eq!(john(7), vec![0, 0, 1, 2, 2, 3, 4])
}

#[test]
fn test3() {
    assert_eq!(ann(15), vec![1, 1, 2, 2, 3, 3, 4, 5, 5, 6, 6, 7, 8, 8, 9]);
}

#[test]
fn test4() {
    assert_eq!(ann(14), vec![1, 1, 2, 2, 3, 3, 4, 5, 5, 6, 6, 7, 8, 8]);
}

#[test]
fn test5() {
    assert_eq!(ann(3), vec![1, 1, 2]);
}

#[test]
fn test6() {
    assert_eq!(sum_john(14), vec![0, 0, 1, 2, 2, 3, 4, 4, 5, 6, 6, 7, 7, 8].iter().sum());
}

#[test]
fn test7() {
    assert_eq!(sum_ann(14), vec![1, 1, 2, 2, 3, 3, 4, 5, 5, 6, 6, 7, 8, 8].iter().sum());
}

#[test]
fn test8() {
    assert_eq!(sum_john(7), vec![0, 0, 1, 2, 2, 3, 4].iter().sum())
}

#[test]
fn test9() {
    assert_eq!(sum_ann(3), vec![1, 1, 2].iter().sum());
}

