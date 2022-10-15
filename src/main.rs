use std::str::FromStr;

use chess::{Game, Board, MoveGen, ChessMove, Square};
use std::io;
fn main()
{
  let board;

  match Board::from_str("8/2kr4/8/8/8/8/8/4K2R w K - 0 1") {
    Ok(b) => board = b,
    Err(e) => panic!("{e}"),
  }

  let movegen = MoveGen::new_legal(&board);

  let mo = ChessMove::new(Square::E1, Square::G1, None);
  

  let newboard = board.make_move_new(mo);
  let mut game = Game::new();

  println!("{}", newboard.to_string());

}