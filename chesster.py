from itertools import count
from collections import namedtuple
import time
from tokenize import endpats
from tracemalloc import start
from turtle import pos

from loggering import *

import utils

import chess

#------------------------------------------------------------
# PIECE VALUES
#------------------------------------------------------------



piece = [ None, (136, 140), (782, 770), (830, 840), (1289, 1310), (2529, 2529), (43000, 43000)]


piece_square_tables = ( #PAWN = 1, KNIGHT = 2, BISHOP, ROOK, QUEEN, KING
  None,

  ( (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), # P
    (-3, -18), (9, -6), (21, 6), (16, 16), (18, 5), (11, 9), (4, -6), (2, -8), 
    (-20, -5), (6, -8), (23, 3), (31, 2), (15, 5), (11, -10), (-15, -7), (-9, -9), 
    (-5, -6), (2, -11), (17, -13), (39, -14), (19, -2), (8, -8), (-20, 1), (-3, 7), 
    (5, 9), (-12, 14), (0, -4), (11, -5), (2, -6), (-11, 2), (-4, 6), (11, 12), 
    (-11, 14), (-14, 8), (-5, 9), (-8, 30), (22, 29), (-6, 19), (-11, 18), (3, 27), 
    (-9, 7), (10, 7), (-14, 17), (4, 24), (-11, 22), (-2, 13), (6, -14), (-7, -1), 
    (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0),),

  ( (-175, -96), (-92, -65), (-74, -49), (-73, -21), (-73, -21), (-74, -49), (-92, -65), (-175, -96), #N
    (-77, -67), (-41, -54), (-27, -18), (-15, 8), (-15, 8), (-27, -18), (-41, -54), (-77, -67), 
    (-61, -40), (-17, -27), (6, -8), (12, 29), (12, 29), (6, -8), (-17, -27), (-61, -40), 
    (-35, -36), (8, -2), (40, 13), (49, 28), (49, 28), (40, 13), (8, -2), (-35, -35), 
    (-34, -45), (13, -16), (44, 9), (51, 39), (51, 39), (44, 9), (13, -16), (-34, -45), 
    (-9, -51), (22, -44), (58, -16), (53, 17), (53, 17), (58, -16), (22, -44), (-9, -51), 
    (-67, -69), (-27, -50), (4, -51), (37, 12), (37, 12), (4, -51), (-27, -50), (-67, -69), 
    (-201, -100), (-84, -88), (-56, -56), (-26, -17), (-26, -17), (-56, -56), (-84, -88), (-201, -100),),

  ( (-37, -40), (-4, -21), (-6, -26), (-16, -8), (-16, -8), (-6, -26), (-4, -21), (-37, -40), # B
    (-11, -26), (6, -9), (13, -12), (3, 1), (3, 1), (13, -12), (6, -9), (-11, -26), 
    (-5, -11), (15, -1), (-4, -1), (12, 7), (12, 7), (-4, -1), (15, -1), (-5, -11), 
    (-4, -14), (8, -4), (18, 0), (27, 12), (27, 12), (18, 0), (8, -4), (-4, -14), 
    (-8, -12), (20, -1), (15, -10), (22, 11), (22, 11), (15, -10), (20, -1), (-8, -12), 
    (-11, -21), (4, 4), (1, 3), (8, 4), (8, 4), (1, 3), (4, 4), (-11, -21), 
    (-12, -22), (-10, -14), (4, -1), (0, 1), (0, 1), (4, -1), (-10, -14), (-12, -22), 
    (-34, -32), (-1, -29), (-10, -26), (-16, -17), (-16, -17), (-10, -26), (-1, -29), (-34, -32),),

  ( (-31, -9), (-20, -13), (-14, -10), (-5, -9), (-5, -9), (-14, -10), (-20, -13), (-31, -9),  #R
    (-21, -12), (-13, -9), (-8, -1), (6, -2), (6, -2), (-8, -1), (-13, -9), (-21, -12), 
    (-25, 6), (-11, -8), (-1, -2), (3, -6), (3, -6), (-1, -2), (-11, -8), (-25, 6), 
    (-13, -6), (-5, 1), (-4, -9), (-6, 7), (-6, 7), (-4, -9), (-5, 1), (-13, -6), 
    (-27, -5), (-15, 8), (-4, 7), (3, -6), (3, -6), (-4, 7), (-15, 8), (-27, -5), 
    (-22, 6), (-2, 1), (6, -7), (12, 10), (12, 10), (6, -7), (-2, 1), (-22, 6), 
    (-2, 4), (12, 5), (16, 20), (18, -5), (18, -5), (16, 20), (12, 5), (-2, 4), 
    (-17, -18), (-19, 0), (-1, 19), (9, 13), (9, 13), (-1, 19), (-19, 0), (-17, -18),),
          #A          #B         #C         #D
  ( (3, -69), (-5, -57), (-5, -47), (4, -26), (4, -26), (-5, -47), (-5, -57), (3, -69), # 1  #Q
    (-3, -54), (5, -31), (8, -22), (12, -4), (12, -4), (8, -22), (5, -31), (-3, -54),  # 2
    (-3, -39), (6, -18), (13, -9), (7, 3), (7, 3), (13, -9), (6, -18), (-3, -39), #3
    (4, -13), (5, -3), (9, 13), (8, 24), (8, 24), (9, 13), (5, -3), (4, -23),  #4
    (0, -29), (14, -6), (11, 9), (5, 21), (5, 21), (12, 9), (14, -6), (0, -29), #5
    (-4, -38), (10, -18), (6, -11), (8, 1), (8, 1), (6, -11), (10, -18), (-4, -38), 
    (-5, -50), (6, -27), (10, -24), (8, -8), (8, -8), (10, -24), (6, -27), (-5, -50), 
    (-2, -74), (-2, -52), (1, -43), (-2, -34), (-2, -34), (1, -43), (-2, -52), (-2, -74),),

  ( (271, 1), (327, 45), (271, 85), (198, 76), (198, 76), (271, 85), (327, 45), (271, 1),  #K
    (278, 53), (303, 100), (234, 133), (179, 135), (179, 135), (234, 133), (303, 100), (278, 53), 
    (195, 88), (258, 130), (169, 169), (120, 175), (120, 175), (169, 169), (258, 130), (195, 88), 
    (164, 103), (190, 156), (138, 172), (98, 172), (98, 172), (138, 172), (190, 156), (164, 103), 
    (154, 96), (179, 166), (105, 199), (70, 199), (70, 199), (105, 199), (179, 166), (154, 96), 
    (123, 92), (145, 172), (81, 184), (31, 191), (31, 191), (81, 184), (145, 172), (123, 92), 
    (88, 47), (120, 121), (65, 116), (33, 131), (33, 131), (65, 116), (120, 121), (88, 47), 
    (59, 11), (89, 59), (45, 73), (-1, 78), (-1, 78), (45, 73), (89, 59), (59, 11),),
  
)

piece_mobility_bonuses = [
  None, #pad
  None, #pawn
  ( (-75,-76), (-56,-54), (-9,-26), (-2,-10), (6,5), (15,11), (22,26), (30,28), (36,29),), #knight
  ( (-48,-58), (-21,-19), ( 16, -2), ( 26, 12), ( 37, 22), ( 51, 42), ( 54, 54), ( 63, 58), 
          ( 65, 63), ( 71, 70), ( 79, 74), ( 81, 86), ( 92, 90), ( 97, 94),), #bishop

  ( (-56,-78), (-25,-18), (-11, 26), ( -5, 55), ( -4, 70), ( -1, 81), (  8,109), ( 14,120), #rook
          ( 21,128), ( 23,143), ( 31,154), ( 32,160), ( 43,165), ( 49,168), ( 59,169),),

  ( (-40,-35), (-25,-12), (  2,  7), (  4, 19), ( 14, 37), ( 24, 55), ( 25, 62), ( 40, 76), #queen
          ( 43, 79), ( 47, 87), ( 54, 94), ( 56,102), ( 60,111), ( 70,116), ( 72,118), ( 73,122), 
          ( 75,128), ( 77,130), ( 85,133), ( 94,136), ( 99,140), (108,157), (112,158), (113,161),
          (118,174), (119,177), (123,191), (128,199),),
  None
]

# pad tables and add material value to pst dicts
# for k, table in piece_square_tables.items():
#   padrow = lambda row: (0,) + tuple((x[0] + piece[k][0], x[1] + piece[k][1]) for x in row) + (0,)
#   piece_square_tables[k] = sum((padrow(table[i*8:i*8+8]) for i in range(8)), ())
#   piece_square_tables[k] = (0,)*20 + piece_square_tables[k] + (0,)*20

piece_square_tables = (None,) + tuple(tuple((x[0] + piece[piece_type][0], x[1] + piece[piece_type][1]) for x in piece_square_tables[piece_type]) for piece_type in chess.PIECE_TYPES)



#------------------------------------------------------------
# GLOBAL CONSTANTS
#------------------------------------------------------------

# Compass directions as relative indexes in string
N, E, S, W = 8, 1, -8, -1
# List of all directions white pieces can move
directions = {
  'P': (N, N+N, N+W, N+E),
  'N': (N+N+E, E+N+E, E+S+E, S+S+E, S+S+W, W+S+W, W+N+W, N+N+W),
  'B': (N+E, S+E, S+W, N+W),
  'R': (N, E, S, W),
  "Q": (N, E, S, W, N+E, S+E, S+W, N+W),
  "K": (N, E, S, W, N+E, S+E, S+W, N+W)
}

# Mate value must be greater than 8*queen + 2*(rook+knight+bishop)
# King value is set to twice this value such that if the opponent is
# 8 queens up, but we got the king, we still exceed MATE_VALUE.
# When a MATE is detected, we'll set the score to MATE_UPPER - plies to get there
# E.g. Mate in 3 will be MATE_UPPER - 6
MATE_LOWER = piece[chess.KING][1] - 10*piece[chess.QUEEN][1]
MATE_UPPER = piece[chess.KING][1] + 10*piece[chess.QUEEN][1]

#like chess.PIECE_TYPES but with no pawn and king
MAJOR_PIECE_TYPES = range(2,5)


#------------------------------------------------------------
# CHESS GAME
#------------------------------------------------------------

def calculate_phase(remaining_pieces):
  e = 2.718
  phase = min(max(1-(1/(1+e**(.2*(-remaining_pieces+16))) * 1.5 - .25), 0), 1)
  return phase

class Position(chess.Board):
  def __init__(selfT, material=None, remaining_pieces=None, fen=chess.STARTING_BOARD_FEN) -> None:
    super().__init__(fen)
    if selfT.turn == chess.BLACK:
      selfT.apply_mirror()
    
    if remaining_pieces:
      selfT.remaining_pieces = remaining_pieces
    else:
      selfT.remaining_pieces = len(list(selfT.pieces(p, c) for c in chess.COLORS for p in chess.PIECE_TYPES))
    if material:
      selfT.material = material
    else:
      midscore = sum(piece_square_tables[piece_type][i][0] for piece_type in chess.PIECE_TYPES for i in selfT.pieces(piece_type, chess.WHITE))
      endscore = sum(piece_square_tables[piece_type][i][1] for piece_type in chess.PIECE_TYPES for i in selfT.pieces(piece_type, chess.WHITE))
      midscore -= sum(piece_square_tables[piece_type][i][1] for piece_type in chess.PIECE_TYPES for i in selfT.pieces(piece_type, chess.BLACK))
      endscore -= sum(piece_square_tables[piece_type][i][1] for piece_type in chess.PIECE_TYPES for i in selfT.pieces(piece_type, chess.BLACK))
      phase = calculate_phase(selfT.remaining_pieces) 
      selfT.material = phase * endscore + (1 - phase) * midscore

  def copy(self, *, stack = False):
    board = Position(material=self.material, remaining_pieces=self.remaining_pieces)

    board.pawns = self.pawns
    board.knights = self.knights
    board.bishops = self.bishops
    board.rooks = self.rooks
    board.queens = self.queens
    board.kings = self.kings

    board.occupied_co[chess.WHITE] = self.occupied_co[chess.WHITE]
    board.occupied_co[chess.BLACK] = self.occupied_co[chess.BLACK]
    board.occupied = self.occupied
    board.promoted = self.promoted

    board.ep_square = self.ep_square
    board.castling_rights = self.castling_rights
    board.turn = self.turn
    board.fullmove_number = self.fullmove_number
    board.halfmove_clock = self.halfmove_clock

    return board
  
  def update_score(self):
    score = 0
    midgame_score = 0
    endgame_score = 0
    for piece_type in MAJOR_PIECE_TYPES:
      mobility = sum(bin(self.attacks_mask(square)).count('1') for square in self.pieces(piece_type, chess.WHITE))
      midgame_score += piece_mobility_bonuses[piece_type][mobility][0]
      endgame_score += piece_mobility_bonuses[piece_type][mobility][1]
    
    phase = calculate_phase(self.remaining_pieces)
    score = endgame_score * phase + (1 - phase) * midgame_score
    return score + self.material

  def move(self, move: chess.Move):
    try:
      new = self.copy()
      
      if move != chess.Move.null(): new.material += self.lazy_value(move)
      # print(self)
      # print(move)
      new.push(move)
      new.apply_mirror()
      return new
    except AttributeError as e:
      print(self)
      print(move)
      print(e)
      quit()

  def lazy_value_phase(self, move: chess.Move, game_stage):
    piece, capture = self.piece_at(move.from_square), self.piece_at(move.to_square)

    score = piece_square_tables[piece.piece_type][move.to_square][game_stage] - piece_square_tables[piece.piece_type][move.from_square][game_stage]

    if capture != None: score += piece_square_tables[capture.piece_type][move.to_square][game_stage]

    if piece.piece_type == chess.KING and abs(move.to_square - move.from_square) == 2:
      score += piece_square_tables[chess.ROOK][(move.from_square + move.to_square)//2][game_stage]
      score -= piece_square_tables[chess.ROOK][chess.A1 if move.to_square < move.from_square else chess.H1][game_stage]
    
    if piece.piece_type == chess.PAWN:
      if move.promotion != None:
        score += piece_square_tables[move.promotion][move.to_square][game_stage] - piece_square_tables[move.promotion][move.from_square][game_stage]
      if self.is_en_passant(move):
        score -= piece_square_tables[chess.QUEEN][chess.square_mirror(move.to_square + S)][game_stage]
    
    return score
  
  def lazy_value(self, move: chess.Move):
    remaining_pieces = self.remaining_pieces

    phase = calculate_phase(remaining_pieces)
    midgame = self.lazy_value_phase(move, 0)
    endgame = self.lazy_value_phase(move, 1)
    score = phase * endgame + (1 - phase) * midgame

    return score

class Searcher():
  def __init__(self):
    self.top_score = {}
    self.top_move = {} # key: position object, object is tuple (move object (from pos.gen_moves()), )
    self.history = set()

  def alpha_beta2(self, position : Position, root: bool, alpha=-MATE_UPPER, beta=MATE_UPPER, depth=8):
    if root:
      best_move = None
    if depth == 0:
      return self.quiesce(position, alpha, beta)
    for move in sorted(position.gen_moves(), key=position.lazy_value, reverse=True):
      if position.board[move[1]] == 'k': # if king is captured we dont need to go down further in the tree for trades this game is over so we are at a leaf
        logging.debug('taking k with ' + position.board[move[0]])
        score = MATE_UPPER + depth # mates that take longer worth less
        #score = -position.move(move).score
      else: score = -self.alpha_beta2(position.move(move), False, -beta, -alpha, depth - 1)
      if score >= beta:
        if root: return move, beta, ()
        return beta
      if score > alpha:
        alpha = score
        if root: best_move = move
    if root: return best_move, alpha, ()
    
    return alpha

  def quiesce(self, position : Position, alpha, beta):
    stand_pat = position.score
    if stand_pat >= beta:
      return beta
    if alpha < stand_pat:
      alpha = stand_pat
    
    for move in sorted(position.gen_moves(), key=position.lazy_value, reverse=True):
      if not position.board[move[1]].islower():
        continue
      if position.board[move[1]] == 'k': 
        score = -position.move(move).score # if king is captured we dont need to go down further in the tree for trades this game is over so we are at a leaf
      else: score = -self.quiesce(position.move(move), -beta, -alpha)

      if score >= beta:
        return beta
      if score > alpha:
        alpha = score
    return alpha
  
  def alpha_beta3(self, position : Position, root: bool, alpha=-MATE_UPPER, beta=MATE_UPPER, depth=8):
    if root:
      best_move = None
    if depth == 0:
      return self.quiesce2(position, alpha, beta)

    king_take_global = False

    for move in sorted(position.gen_moves(), key=position.lazy_value, reverse=True):
      king_take = False
      if position.board[move[1]] == 'k': # if king is captured we dont need to go down further in the tree for trades this game is over so we are at a leaf
        logging.debug('taking k with ' + position.board[move[0]])
        score = MATE_UPPER + depth # mates that take longer worth less
        king_take = True
        #score = -position.move(move).score
      else: 
        score, mate = self.alpha_beta3(position.move(move), False, -beta, -alpha, depth - 1)
        score = -score
        if mate and position.board[move[0]] == 'K' and abs(move[0]-move[1]) == 2:
          pass
      if score >= beta:
        if root: best_move = move
        king_take_global = king_take
        alpha = beta
        break
      if score > alpha:
        alpha = score
        king_take_global = king_take
        if root: best_move = move
    if root: return best_move, alpha, ()
    return alpha, king_take_global
  
  def quiesce2(self, position : Position, alpha, beta):
    stand_pat = position.score
    if stand_pat >= beta:
      return beta, False
    if alpha < stand_pat:
      alpha = stand_pat

    king_take_global = False

    for move in sorted(position.gen_moves(), key=position.lazy_value, reverse=True):
      king_take = False
      if not position.board[move[1]].islower():
        continue
      if position.board[move[1]] == 'k': 
        score = -position.move(move).score # if king is captured we dont need to go down further in the tree for trades this game is over so we are at a leaf
        king_take = True
      else: 
        score, mate = self.quiesce2(position.move(move), -beta, -alpha)
        score = -score
        if mate and position.board[move[0]] == 'K' and abs(move[0]-move[1]) == 2:
          pass
      if score >= beta:
        alpha=beta
        king_take_global=king_take
        break
      if score > alpha:
        alpha = score
        king_take_global=king_take
    return alpha, king_take_global

Entry = namedtuple('Entry', 'lower upper')

TABLE_SIZE = 1e8

class TranspositionOptimizedSearcher():
  def __init__(self):
    self.transposition_score = {}
    self.transposition_move = {} # key: position object, object is tuple (move object (from pos.gen_moves()), )
    self.history = set()
    self.nodes = 0

  #alpha beta but alpha == beta
  def alpha_beta_zero_window(self, position: Position, root: bool, gamma, depth, history=()):
    
    depth = max(depth, 0)

    if depth == 0:
      #return self.quiesce_zero_window(position, gamma)
      return self.quiesce_zero_window(position, gamma, history=history)

    if position.material <= -MATE_LOWER:
      return -MATE_UPPER - depth

    #CHECK FOR DRAWS
    if not root and position.board_fen() in history:
      return 0

    entry = self.transposition_score.get((position.board_fen(), depth, root), Entry(-MATE_UPPER, MATE_UPPER))
    if entry.lower >= gamma and (not root or self.transposition_move.get(position.board_fen()) is not None):
      if root: return entry.lower
      else: return entry.lower
    elif entry.upper < gamma:
      if root: return entry.upper
      else: return entry.upper

    best_score = -MATE_UPPER
    # check = any(rotated.lazy_value(m) >= MATE_LOWER for m in rotated.gen_moves())
    check = position.is_check()

    # generator of moves to search
    def moves():

      if not root and any(position.pieces(p, int(c)) != 0 for p in MAJOR_PIECE_TYPES for c in chess.COLORS):
        score = self.alpha_beta_zero_window(position.move(chess.Move.null()), False, 1-gamma, depth-3, history=history)
        yield None, -score

      killer = self.transposition_move.get(position.board_fen())
      if killer:
        score = self.alpha_beta_zero_window(position.move(killer), False, 1-gamma, depth - 1, history=history) #-(gamma - 1)
        yield killer, -score
        
      
      for move in sorted(position.generate_pseudo_legal_moves(), key=position.lazy_value, reverse=True):
        score = self.alpha_beta_zero_window(position.move(move), False, 1-gamma, depth - 1, history=history)
        yield move, -score
      
    for move, score in moves():
      best_score = max(best_score, score)
      if best_score >= gamma:
        #clear transposition table first
        
        if len(self.transposition_move) > TABLE_SIZE: self.transposition_move.clear()
        
        self.transposition_move[position.board_fen()] = move
        
        break

    if best_score < gamma and best_score < 0:
      # is_dead = lambda position: any(position.lazy_value(m) >= MATE_LOWER for m in position.generate_pseudo_legal_moves())
      # if all(is_dead(position.move(m)) for m in position.gen_moves()):
      #   in_check = is_dead(position.nullmove())
      #   best_score = -MATE_UPPER - depth if in_check else 0
      if position.is_stalemate():
        best_score = -MATE_UPPER - depth
      if position.is_checkmate():
        best_score = 0

    if len(self.transposition_score) > TABLE_SIZE: self.transposition_score.clear()

    if best_score >= gamma:
      self.transposition_score[position.board_fen(), depth, root] = Entry(best_score, entry.upper) #fail high
    else:
      self.transposition_score[position.board_fen(), depth, root] = Entry(entry.lower, best_score) #fail low
    
    return best_score

    


  def quiesce_zero_window(self, position : Position, gamma, history=()):
    if position.material <= -MATE_LOWER:
      return -MATE_UPPER

    #CHECK FOR DRAWS
    if position.board_fen() in history:
      return 0
    
    entry = self.transposition_score.get((position.board_fen(), 0, False), Entry(-MATE_UPPER, MATE_UPPER))
    if entry.lower >= gamma:
      return entry.lower
    elif entry.upper < gamma:
      return entry.upper

    best_score = -MATE_UPPER

    def moves(): #FIXME IF IN CHECK WHEN QUISCENCE HAPPENS BUT THERE ARE NO TAKES TO GET OUT OF THAT CHECK, THEN EVALUATION STOPS, IN CASE OF A FORK YOU WANT TO GO A LEVEL DEEPER
      
      yield None, position.material

      killer = self.transposition_move.get(position.board_fen())
      if killer and position.lazy_value(killer) >= 200:
        score = self.quiesce_zero_window(position.move(killer), 1-gamma, history=history) #-(gamma - 1)
        yield killer, -score
      
      for move in sorted(position.generate_pseudo_legal_moves(), key=position.lazy_value, reverse=True):
        #if position.board[move[1]].islower(): 
        if position.lazy_value(move) >= 200:
          score = self.quiesce_zero_window(position.move(move), 1-gamma, history=history)
          yield move, -score
    
    for move, score in moves():
      best_score = max(best_score, score)
      if best_score >= gamma:
        #clear transposition table first
        
        if len(self.transposition_move) > TABLE_SIZE: self.transposition_move.clear()
        
        self.transposition_move[position.board_fen()] = move
        
        break
    
    if len(self.transposition_score) > TABLE_SIZE: self.transposition_score.clear()

    if best_score >= gamma:
      self.transposition_score[position.board_fen(), 0, False] = Entry(best_score, entry.upper) #fail high
    else:
      self.transposition_score[position.board_fen(), 0, False] = Entry(entry.lower, best_score) #fail low
    
    return best_score

    


  def quiesce(self, position : Position, alpha, beta):
    stand_pat = position.score
    if stand_pat >= beta:
      return beta, False
    if alpha < stand_pat:
      alpha = stand_pat

    king_take_global = False

    for move in sorted(position.gen_moves(), key=position.lazy_value, reverse=True):
      king_take = False
      if not position.board[move[1]].islower():
        continue
      if position.board[move[1]] == 'k': 
        score = -position.move(move).score # if king is captured we dont need to go down further in the tree for trades this game is over so we are at a leaf
        king_take = True
      else: 
        score, mate = self.quiesce(position.move(move), -beta, -alpha)
        score = -score
        if mate and position.board[move[0]] == 'K' and abs(move[0]-move[1]) == 2:
          pass
      if score >= beta:
        alpha=beta
        king_take_global=king_take
        break
      if score > alpha:
        alpha = score
        king_take_global=king_take
    return alpha, king_take_global

  def iterative_deepening_mtdf(self, position : Position, root : bool, max_depth, movetime):
    guess = 0
    move_score = None
    move = None
    start = time.time()
    for depth in range(1, max_depth):
      lower, upper = -MATE_UPPER, MATE_UPPER
      while True:
        beta = guess + int(guess == lower)
        guess = self.alpha_beta_zero_window(position, True, beta, depth)
        if guess < beta:
          upper = guess
        else:
          lower = guess
        if lower >= upper:
          break

      self.alpha_beta_zero_window(position, True, lower, depth) # call to always fail high to get a move if it's out of the tp
      
      #if time is up: break
      if movetime > 0 and (time.time() - start) * 1000 > movetime: break

    move = self.transposition_move.get(position.board_fen())
    move_score = self.transposition_score.get((position.board_fen(), depth, True))
    return move, move_score.lower
  
  def iterative_deepening_mtdbi(self, position: Position, root: bool, max_depth, movetime, history=()):
    guess = 0
    move_score = None
    move = None
    start = time.time()
    finaldepth = 0
    for depth in range(1, max_depth + 1):
      
      lower, upper = -MATE_UPPER, MATE_UPPER
      while True:
        beta = (lower+upper+1)//2 #binary search between values of beta to accomodate for value functions that return floats
        guess = self.alpha_beta_zero_window(position, True, beta, depth, history=history)
        if guess < beta:
          upper = guess
        else:
          lower = guess
        if lower >= upper - 10: #have some level of error for the search
          break

      self.alpha_beta_zero_window(position, True, lower, depth, history=history) # call to always fail high to get a move if it's out of the tp
      
      output('info depth {} timeleft {}'.format(depth, movetime - 1000 * (time.time() - start)))
      finaldepth = depth

      #if time is up: break
      if movetime > 0 and (time.time() - start) * 1000 > movetime: break
    move = self.transposition_move.get(position.board_fen())
    move_score = self.transposition_score.get((position.board_fen(), finaldepth, True))
    return move, move_score.lower, move_score.upper

  

    



# def parse(c):
#   fil, rank = ord(c[0]) - ord('a'), int(c[1]) - 1
#   return A1 + fil - 10*rank

# def render(i):
#   rank, fil = divmod(i - A1, 10)
#   return chr(fil + ord('a')) + str(-rank + 1)



    
