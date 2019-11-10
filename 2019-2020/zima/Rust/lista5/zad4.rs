use std::collections::BTreeSet;

enum Action {
    Left, Right, Forward
}

struct Operation {
    action: Action,
    times:  Option<u32>,
}

enum Direction {
    Left, Right, Up, Down
}


fn apply_turn(dir: Direction, turn: &Action) -> Direction {
    match turn {
        Action::Forward => dir,
        Action::Left => match dir {
            Direction::Left  => Direction::Down,
            Direction::Right => Direction::Up,
            Direction::Up    => Direction::Left,
            Direction::Down  => Direction::Right,
        },
        Action::Right => match dir {
            Direction::Left  => Direction::Up,
            Direction::Right => Direction::Down,
            Direction::Up    => Direction::Right,
            Direction::Down  => Direction::Left
        }
    }
}


fn apply_movement(pos: (i64, i64), dir: &Direction) -> (i64, i64) {
    match dir {
        Direction::Left  => (pos.0 - 1, pos.1),
        Direction::Right => (pos.0 + 1, pos.1),
        Direction::Up    => (pos.0, pos.1 - 1),
        Direction::Down  => (pos.0, pos.1 + 1),
    }
}


pub fn execute(code: &str) -> String {

    let mut moves : Vec<Operation> = vec![];

    for c in code.chars() {
        match c {
            'L' => moves.push(Operation{action: Action::Left, times: None}),
            'R' => moves.push(Operation{action: Action::Right, times: None}),
            'F' => moves.push(Operation{action: Action::Forward, times: None}),
            '0'..='9' => {
                let mut last = moves.last_mut().unwrap();
                let digit = c.to_digit(10).unwrap();
                match last.times {
                    None => last.times = Some(digit),
                    Some(times) => last.times = Some(times * 10 + digit),
                }
            },
            _ => (),
        }
    }

    let mut position : (i64, i64) = (0,0);
    let mut direction = Direction::Right;
    let mut visited = BTreeSet::<(i64, i64)>::new();

    visited.insert(position);

    for action in moves {
        let times = action.times.unwrap_or(1);

        match action.action {
            Action::Forward => {
                for _ in 0..times {
                    position = apply_movement(position, &direction);
                    visited.insert(position);

                }
            },
            turn => {
                for _ in 0..times {
                    direction = apply_turn(direction, &turn)
                }
            },
        }
    }

    let min_row = visited.iter().min_by_key(|a| a.1).unwrap_or(&(1,1)).1;
    let min_col = visited.iter().min_by_key(|a| a.0).unwrap_or(&(1,1)).0;
    let max_row = visited.iter().max_by_key(|a| a.1).unwrap_or(&(1,1)).1;
    let max_col = visited.iter().max_by_key(|a| a.0).unwrap_or(&(1,1)).0;

    let mut result = String::new();

    for row in min_row..=max_row {
        for col in min_col..=max_col {
            if visited.contains(&(col, row)) {
                result.push('*');
            } else {
                result.push(' ');
            }
        }
        
        if row != max_row { 
            result.push_str("\r\n") 
        }
    }

    result
}
