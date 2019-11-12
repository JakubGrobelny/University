#[derive(std::fmt::Debug)]
struct Sudoku{
    data: Vec<Vec<u32>>,
}

impl Sudoku{
    fn is_valid(&self) -> bool {
        let n = self.data.len();
        let bsize = (n as f64).sqrt() as usize;

        if !self.data.iter().fold(true, |acc, row| row.len() == n && acc) {
            return false
        }

        let mut row_count : Vec<Vec<bool>> = vec![vec![false; n+1]; n+1];
        let mut col_count : Vec<Vec<bool>> = vec![vec![false; n+1]; n+1];   

        for block_y in 0..n / bsize {
            for block_x in 0..n / bsize {
                let mut nums : Vec<bool> = vec![false; n+1];
                
                for y in 0..bsize {
                    for x in 0..bsize {
                        let y0 = block_y * bsize + y;
                        let x0 = block_x * bsize + x;
                        let val = self.data[y0][x0] as usize;

                        if val > n || val == 0 { return false }

                        if nums[val] { return false }
                        nums[val] = true;

                        if row_count[y0][val] { return false }
                        row_count[y0][val] = true;

                        if col_count[x0][val] { return false }
                        col_count[x0][val] = true;
                    }
                }
            }
        }

        true
    }
}

#[test]
fn test0() {
    assert_eq!(Sudoku {data: vec![
        vec![1,2, 3,4],
        vec![3,4, 2,1],
        vec![2,1, 4,3],
        vec![4,3, 1,2]
    ]}.is_valid(), true)
}

#[test]
fn test1() {
    assert_eq!(Sudoku {data: vec![
        vec![1,2, 3,4],
        vec![1,4, 2,1],
        vec![2,1, 4,3],
        vec![4,3, 1,2]
    ]}.is_valid(), false)
}

#[test]
fn test2() {
    assert_eq!(Sudoku {data: vec![
        vec![1,2, 3,4],
        vec![3,4, 2,1],
        vec![2,1, 4,3],
        vec![4,3, 1,2],
        vec![]
    ]}.is_valid(), false)
}

#[test]
fn test3() {
    assert_eq!(Sudoku {data: vec![
        vec![1,2, 3,4],
        vec![3,4, 2],
        vec![2,1, 4,3],
        vec![4,3, 1,2]
    ]}.is_valid(), false)
}

#[test]
fn test4() {
    assert_eq!(Sudoku {data: vec![
        vec![1,7,5, 9,3,6, 4,2,8],
        vec![2,6,3, 7,4,8, 9,5,1],
        vec![8,9,4, 5,1,2, 6,7,3],
        
        vec![6,4,2, 1,8,5, 3,9,7],
        vec![9,3,1, 6,7,4, 5,8,2],
        vec![7,5,8, 3,2,9, 1,4,6],

        vec![3,2,6, 4,9,7, 8,1,5],
        vec![4,1,7, 8,5,3, 2,6,9],
        vec![5,8,9, 2,6,1, 7,3,4]
    ]}.is_valid(), true)
}


#[test]
fn test5() {
    assert_eq!(Sudoku {data: vec![
        vec![1,7,5, 9,3,6, 4,2,8],
        vec![2,6,3, 7,4,8, 9,5,1],
        vec![8,9,4, 5,1,2, 6,7,3],
        vec![],    
        vec![6,4,2, 1,8,5, 3,9,7],
        vec![9,3,1, 6,7,4, 5,8,2],
        vec![7,5,8, 3,2,9, 1,4,6],

        vec![3,2,6, 4,9,7, 8,1,5],
        vec![4,1,7, 8,5,3, 2,6,9],
        vec![5,8,9, 2,6,1, 7,3,4]
    ]}.is_valid(), false)
}


#[test]
fn test6() {
    assert_eq!(Sudoku {data: vec![
        vec![1,7,5, 9,3,6, 4,2,8],
        vec![2,6,3, 7,4,8, 9  ,1],
        vec![8,9,4, 5,1,2, 6,7,3],
        
        vec![6,4,2, 1,8,5, 3,9,7],
        vec![9,3,1, 6,7,4, 5,8,2],
        vec![7,5,8, 3,2,9, 1,4,6],

        vec![3,2,6, 4,9,7, 8,1,5],
        vec![4,1,7, 8,5,3, 2,6,9],
        vec![5,8,9, 2,6,1, 7,3,4]
    ]}.is_valid(), false)
}

#[test]
fn test7() {
    assert_eq!(Sudoku {data: vec![
        vec![3,9,7, 8,1,6, 2,5,4],
        vec![8,4,1, 2,5,7, 3,9,6],
        vec![2,6,5, 9,3,4, 7,1,8],
        
        vec![5,3,8, 6,9,2, 1,4,7],
        vec![4,7,9, 5,8,1, 6,3,2],
        vec![1,2,6, 4,7,3, 5,8,9],

        vec![7,5,4, 3,6,9, 8,2,1],
        vec![6,8,2, 1,4,5, 9,7,3],
        vec![9,1,3, 7,2,8, 4,6,5]
    ]}.is_valid(), true)
}

#[test]
fn test8() {
    assert_eq!(Sudoku {data: vec![
        vec![3,9,7, 8,1,6, 2,5,4],
        vec![8,4,1, 2,5,7, 3,9,6],
        vec![2,6,4, 9,3,4, 7,1,8],
        
        vec![5,3,8, 6,9,2, 1,4,7],
        vec![4,7,9, 5,8,1, 6,3,2],
        vec![1,2,6, 4,7,3, 5,8,9],

        vec![7,5,4, 3,6,9, 8,2,1],
        vec![6,8,2, 1,4,5, 9,7,3],
        vec![9,1,3, 7,2,8, 4,6,5]
    ]}.is_valid(), false)
}

#[test]
fn test9() {
    assert_eq!(Sudoku {data: vec![
        vec![0x8,0xE,0xB,0xC, 0x3,0xD,0x7,0x5, 0x9,0x2,0x0,0xF, 0x1,0x6,0xA,0x4],
        vec![0x6,0x4,0x7,0xD, 0x1,0x0,0x2,0xF, 0x5,0xC,0xE,0xA, 0x9,0x3,0x8,0xB],
        vec![0x1,0x2,0x0,0xA, 0x4,0xC,0xB,0x9, 0x3,0x6,0xD,0x8, 0xE,0x5,0x7,0xF],
        vec![0x9,0x3,0x5,0xF, 0x8,0xA,0xE,0x6, 0xB,0x7,0x4,0x1, 0xC,0x2,0x0,0xD],

        vec![0xF,0xD,0x9,0x7, 0x5,0x4,0x0,0xC, 0xE,0xA,0x8,0x2, 0x3,0xB,0x6,0x1],
        vec![0xB,0xC,0x3,0x6, 0x2,0x1,0xF,0x8, 0x0,0xD,0x5,0x9, 0x7,0xE,0x4,0xA],
        vec![0x0,0xA,0x2,0x4, 0x7,0x9,0xD,0xE, 0x1,0xB,0x3,0x6, 0x5,0xF,0xC,0x8],
        vec![0xE,0x1,0x8,0x5, 0xB,0x3,0x6,0xA, 0x7,0x4,0xF,0xC, 0xD,0x0,0x9,0x2],
        
        vec![0x2,0x0,0xA,0x1, 0xF,0xE,0x9,0x4, 0x8,0x3,0x6,0x4, 0xB,0x7,0xD,0xC],
        vec![0x3,0x6,0xC,0x8, 0x0,0xB,0xA,0xD, 0x2,0xE,0x1,0x7, 0x4,0x9,0xF,0x5],
        vec![0x5,0xB,0xF,0xE, 0xC,0x7,0x1,0x2, 0x4,0x0,0x9,0xD, 0xA,0x8,0x3,0x6],
        vec![0x4,0x7,0xD,0x9, 0x6,0x5,0x8,0x3, 0xC,0xF,0xA,0xB, 0x0,0x1,0x2,0xE],
        
        vec![0xC,0x5,0xE,0xB, 0xD,0x2,0x3,0x0, 0x6,0x8,0x7,0x4, 0xF,0xA,0x1,0x9],
        vec![0x7,0x9,0x4,0x3, 0xA,0x6,0xC,0x1, 0xF,0x5,0x2,0xE, 0x8,0xD,0xB,0x0],
        vec![0xD,0x8,0x6,0x0, 0x9,0xF,0x5,0xB, 0xA,0x1,0xC,0x3, 0x2,0x4,0xE,0x7],
        vec![0xA,0xF,0x1,0x2, 0xE,0x8,0x4,0x7, 0xD,0x9,0xB,0x0, 0x6,0xC,0x5,0x3]
    ]}.is_valid(), false)
}




