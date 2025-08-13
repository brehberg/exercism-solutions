#[derive(Debug)]
pub enum Category {
    Ones,
    Twos,
    Threes,
    Fours,
    Fives,
    Sixes,
    FullHouse,
    FourOfAKind,
    LittleStraight,
    BigStraight,
    Choice,
    Yacht,
}

type Dice = [u8; 5];

pub fn score(dice: Dice, category: Category) -> u8 {
    use Category::*;
    match category {
        Ones => sum_for_value(dice, 1),
        Twos => sum_for_value(dice, 2),
        Threes => sum_for_value(dice, 3),
        Fours => sum_for_value(dice, 4),
        Fives => sum_for_value(dice, 5),
        Sixes => sum_for_value(dice, 6),
        FullHouse => check_full_house(dice),
        FourOfAKind => check_four_of_a_kind(dice),
        LittleStraight => check_straight(dice, [1, 2, 3, 4, 5]),
        BigStraight => check_straight(dice, [2, 3, 4, 5, 6]),
        Yacht => check_yacht(dice),
        Choice => dice.iter().sum(),
    }
}

// a yacht is worth 50 points if all dice values are the same
fn check_yacht(mut dice: Dice) -> u8 {
    dice.sort();
    if dice[0] == dice[4] {
        50
    } else {
        0
    }
}

// a full house is worth the sum of all dice if three of one value and two of another
#[rustfmt::skip]
fn check_full_house(mut dice: Dice) -> u8 {
    dice.sort();
    let [d1, d2, d3, d4, d5] = dice;
    if ((d1 == d2 && d3 == d5)      // [s, s, b, b, b]
        || (d1 == d3 && d4 == d5))  // or [s, s, s, b, b]
        && (d1 != d5) {             // and not [d, d, d, d, d]
        dice.iter().sum()
    } else {
        0
    }
}

// a four of a kind is worth the sum of the four dice showing the same face
fn check_four_of_a_kind(mut dice: Dice) -> u8 {
    dice.sort();
    let [d1, d2, _d3, d4, d5] = dice;
    if d1 == d4 {
        4 * d1 // [s, s, s, s, _]
    } else if d2 == d5 {
        4 * d2 // [_, b, b, b, b]
    } else {
        0
    }
}

// a straight is worth 30 points if all dice values required are present
fn check_straight(mut dice: Dice, values: Dice) -> u8 {
    dice.sort();
    if dice == values {
        30
    } else {
        0
    }
}

fn sum_for_value(dice: Dice, value: u8) -> u8 {
    dice.iter().filter(|&die| *die == value).sum()
}
