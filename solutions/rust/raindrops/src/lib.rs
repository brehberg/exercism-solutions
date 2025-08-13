pub fn raindrops(n: u32) -> String {
    let mut output = Vec::new();

    if n % 3 == 0 {
        output.push("Pling");
    }
    if n % 5 == 0 {
        output.push("Plang");
    }
    if n % 7 == 0 {
        output.push("Plong");
    }

    if output.is_empty() {
        n.to_string()
    } else {
        output.join("")
    }
}
