fn number(bus_stops: &[(i32,i32)]) -> i32 {
    bus_stops.iter().fold(0, |inside, (i, o)| inside + i - o)
}

#[test]
fn test0() {
    assert_eq!(number(&[(10,0),(3,5),(5,8)]), 5);
}

#[test]
fn test1() {
      assert_eq!(number(&[(3,0),(9,1),(4,10),(12,2),(6,1),(7,10)]), 17);
}

#[test]
fn test2() {
    assert_eq!(number(&[(3,0),(9,1),(4,8),(12,2),(6,1),(7,8)]), 21);
}

#[test]
fn test3() {
    assert_eq!(number(&[(5, 0), (0, 5), (0, 0)]), 0);
}

#[test]
fn test4() {
    assert_eq!(number(&[]), 0);
}

#[test]
fn test5() {
    assert_eq!(number(&[(10,0),(9,1),(4,2),(4,6),(2,1),(0,2)]), 17);
}

#[test]
fn test6() {
    assert_eq!(number(&[(11,0),(10,1),(5,2),(5,6),(3,1),(1,2)]), 23);
}

#[test]
fn test7() {
    assert_eq!(number(&[(11,0),(11,2),(6,3),(6,7),(4,2),(3,3)]), 24);
}

#[test]
fn test8() {
    assert_eq!(number(&[(11,0),(11,0),(11,0)]), 33);
}

#[test]
fn test9() {
    assert_eq!(number(&[(11,0),(11,11),(11,11)]), 11);
}









