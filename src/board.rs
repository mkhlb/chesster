use crate::pieces::{Direction, Square}
use std::fmt::Debug;

pub const PADDING: usize = 2;
pub const BOARD_SIDE: usize = 8 + 2 * PADDING;
pub const BOARD_SIZE: usize = BOARD_SIDE * BOARD_SIDE;

pub const A8: usize = BOARD_SIDE * PADDING + PADDING;
pub const H8: usize = A8 + 7;
pub const A1: usize = A8 + 7 * BOARD_SIDE;
pub const H1: usize = A1 + 7;

#[derive]