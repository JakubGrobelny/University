fn interpreter(code: &str, iters: usize, width: usize, height: usize) -> String {
    fn wrap(pos: isize, size: usize) -> isize {
        if pos < 0 {
            size as isize + pos
        } else {
            pos % size as isize
        }
    }

    fn jump(ip: isize, code: &Vec<char>, count: isize, dir: isize) -> isize {
        if count == 0 {
            return ip - dir;
        }

        let count = match code[ip as usize] {
            '[' => count - 1,
            ']' => count + 1,
            _   => count,
        };

        jump(ip + dir, code, count, dir)
    }

    let code : Vec<char> = code.chars().collect();
    let mut memory : Vec<Vec<bool>> = vec![vec![false; width]; height];
    let mut instr_ptr : isize = 0;
    let mut iter : isize = 0;
    let (mut data_y, mut data_x) : (isize, isize) = (0, 0);

    while (iter < iters as isize) && ((instr_ptr as usize) < code.len()) {
        match code[instr_ptr as usize] {
            'n' => data_y = wrap(data_y - 1, height),
            's' => data_y = wrap(data_y + 1, height),
            'e' => data_x = wrap(data_x + 1, width),
            'w' => data_x = wrap(data_x - 1, width),
            '*' => memory[data_y as usize][data_x as usize] ^= true,
            ']' => {
                if memory[data_y as usize][data_x as usize] {
                    instr_ptr = jump(instr_ptr - 1, &code, 1, -1);
                }
            },
            '[' => {
                if !memory[data_y as usize][data_x as usize] {
                    instr_ptr = jump(instr_ptr + 1, &code, -1, 1);
                }
            },
            _ => iter -= 1,
        }

        instr_ptr += 1;
        iter += 1;
    }

    memory.iter()
          .map(|col| col.iter()
                        .map(|&b| (b as u32).to_string())
                        .collect::<String>())
          .collect::<Vec<String>>()
          .join("\r\n")
}


#[test]
fn test0() {
    assert_eq!(interpreter("*[es*]", 5, 5, 6), "10000\r\n01000\r\n00000\r\n00000\r\n00000\r\n00000")
}

#[test]
fn test1() {
    assert_eq!(interpreter("*[es*]", 9, 5, 6),"10000\r\n01000\r\n00100\r\n00000\r\n00000\r\n00000")
}

#[test]
fn test2() {
    assert_eq!(interpreter("*[s[e]*]", 5, 5, 5),"10000\r\n10000\r\n00000\r\n00000\r\n00000")
}

#[test]
fn test3() {
    assert_eq!(interpreter("o*e*eq*reqrqp*ppooqqeaqqsr*yqaooqqqfqarppppfffpppppygesfffffffffu*wrs*agwpffffst*w*uqrw*qyaprrrrrw*nuiiiid???ii*n*ynyy??ayd*r:rq????qq::tqaq:y???ss:rqsr?s*qs:q*?qs*tr??qst?q*r",7,6,9), "111100\r\n000000\r\n000000\r\n000000\r\n000000\r\n000000\r\n000000\r\n000000\r\n000000")
}

#[test]
fn test4() {
    assert_eq!(interpreter("",0,1,1), "0")
}

#[test]
fn test5() {
    assert_eq!(interpreter("*e*e*e*es*es*ws*ws*w*w*w*n*n*n*ssss*s*s*s*",0,6,9), "000000\r\n000000\r\n000000\r\n000000\r\n000000\r\n000000\r\n000000\r\n000000\r\n000000")
}

#[test]
fn test6() {
    assert_eq!(interpreter("eee*s*s*s*w*w*w*w*w*w*w*n*n*n*n*n*n*n*n*n*e*e*e*e*e*e*e*s*s*s*s*s*",1000,8,10), "00011000\r\n00011000\r\n00011000\r\n11111111\r\n11111111\r\n00011000\r\n00011000\r\n00011000\r\n00011000\r\n00011000")
}

#[test]
fn test7() {
    assert_eq!(interpreter("wewewnweesnsewnsn*nwwsssw*ewe*nnnwe**wsswennnns*ese*nwwws*e*sew***w*wnws*n**snewsnswe*ne**we*wwesnensswnwwwen**ws*eswwnneeseswww*snwsnsneeeeesssnnween*wsnsessswwnwsnnnsesews*wnswwnewnssw*snnn*w*we*nsnwennwnwns*nsnsnes*e**essn*nneneewnsens*weesnewewnssws**en*ws**s*wsnwss*ewn**swsweseee*wwn*wwsennne**wee*s*weeeeseesw*es*wsssnwnsnsnnn*swwn*nnenww*s*ss*wwww*nwwwssensen*ww*eewwwww**ewseswwnwne*n*ee*wwwsnwwseseeewwe*wse**ewwenenn*eessenesneseeeen*wwnewnssnwewsww*neewsnnwne*n*ewnw*ews*wnwee*se*ww*sss***e*wn*w**eene*seee**s*ew**wwese**senw*se*enn*eewnee*ewsee**ew*esnnn*ww*seenwseenss*ses*snsssnww**we*n*wenww*snwewne*e**snweessseenw*eeewe*wswnsnswwwsewe*nnw*wswssws*seeswnnws*s*ns*wnnnwwewssnew*swewsnsswss**nesweenwnsws*nwenwwe**sn*ew**s*sswwe*nwn*ses*nne*ne*s*ns*wn*s***ene*ewn*wnswwn*e**nsnewnse*ssssess*ww*snneesnewessswewnwnwsen*e*nnnwwn**nennneeen*wwswwen*wnws*swwssn*ewweneeesww*s*e**nnw*wnsnww*se*neneewn**nns*wen*wwe*wwwwe*www**sn*e*nwwnwww*en*ens*wwseessnenssnwee***eewnnwsesswnnn*ewsw**",894,10,10), "1000000001\r\n0010111111\r\n0100001001\r\n0001011010\r\n0100001000\r\n0110001110\r\n0001010100\r\n0000111000\r\n1111111110\r\n0001000000")
}

#[test]
fn test8() {
    assert_eq!(interpreter("*[es*]",1,5,6), "10000\r\n00000\r\n00000\r\n00000\r\n00000\r\n00000")
}

#[test]
fn test9() {
    assert_eq!(interpreter("",0,2,2), "00\r\n00")
}