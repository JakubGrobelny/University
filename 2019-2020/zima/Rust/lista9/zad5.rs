mod solution {   
    pub fn range_extraction(a: &[i32]) -> String {
        let mut ranges : Vec<(i32, i32)> = vec![];
        let mut i = 0;

        while i < a.len() {
            let beginning = a[i];
            while i < a.len()-1 && a[i+1] - a[i] == 1 {
                i += 1;
            }
            let end = a[i];
            i += 1;

            ranges.push((beginning, end));
        }

        ranges.iter().map(|(b,e)| {
            if b == e { 
                format!{"{}", b} 
            } else if e - b < 2 { 
                format!{"{},{}", b, e} 
            } else { 
                format!{"{}-{}", b, e}
            }
        }).collect::<Vec<String>>().join(",")
    }
}

use solution::range_extraction;

#[test]
fn test0() {
    assert_eq!(range_extraction(&[1,2,3,4,8]), "1-4,8")
}

#[test]
fn test1() {
    assert_eq!(range_extraction(&[1,3,4]), "1,3,4")
}

#[test]
fn test2() {
    assert_eq!(range_extraction(&[]), "")
}

#[test]
fn test3() {
    assert_eq!(range_extraction(&[1,2,3,4,5,6]), "1-6")
}

#[test]
fn test4() {
    assert_eq!(range_extraction(&[0]), "0")
}

#[test]
fn test5() {
    assert_eq!(range_extraction(&[0, 2, 4, 6]), "0,2,4,6")
}

#[test]
fn test6() {
    assert_eq!(range_extraction(&[100,101,102]), "100-102")
}

#[test]
fn test7() {
    assert_eq!(range_extraction(&[101,102,104,106,107,108]), "101,102,104,106-108")
}

#[test]
fn test8() {
    assert_eq!(range_extraction(&[100, 101,102,104,106,107,108]), "100-102,104,106-108")
}

#[test]
fn test9() {
    assert_eq!(range_extraction(&[1,2]), "1,2")
}
