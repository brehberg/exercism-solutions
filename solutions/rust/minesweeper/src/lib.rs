static NEIGHBORS: &[(i32, i32)] = &[
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
        .map(|r| {
            let cols = minefield[r as usize].len() as i32;
            (0..cols)
                .map(|c| {
                    if minefield[r as usize].as_bytes()[c as usize] == b'*' {
                        '*'
                    } else {
                        match NEIGHBORS
                            .iter()
                            .map(|&(row_offset, col_offset)| (r + row_offset, c + col_offset))
                            .filter(|&(r, c)| (0 <= r && r < rows) && (0 <= c && c < cols))
                            .filter(|&(r, c)| minefield[r as usize].as_bytes()[c as usize] == b'*')
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
