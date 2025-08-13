static NEIGBOUR_OFFSETS: &[(i32, i32)] = &[
    (-1, -1),
    (0, -1),
    (1, -1),
    (-1, 0),
    // (0, 0),
    (1, 0),
    (-1, 1),
    (0, 1),
    (1, 1),
];

pub fn annotate(minefield: &[&str]) -> Vec<String> {
    let rows = minefield.len() as i32;
    (0..rows)
        .map(|y| {
            let cols = minefield[y as usize].len() as i32;
            (0..cols)
                .map(|x| {
                    if minefield[y as usize].as_bytes()[x as usize] == b'*' {
                        '*'
                    } else {
                        match NEIGBOUR_OFFSETS
                            .iter()
                            .map(|&(dx, dy)| (x + dx, y + dy))
                            .filter(|&(x, y)| (0 <= x && x < cols) && (0 <= y && y < rows))
                            .filter(|&(x, y)| minefield[y as usize].as_bytes()[x as usize] == b'*')
                            .count()
                        {
                            0 => ' ',
                            n => (b'0' + n as u8) as char,
                        }
                    }
                })
                .collect()
        })
        .collect()
}
