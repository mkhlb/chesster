from itertools import count
from collections import namedtuple
import time
from tokenize import endpats
from tracemalloc import start

from loggering import *

import utils



#------------------------------------------------------------
# PIECE VALUES
#------------------------------------------------------------

piece = { 'P': (136, 140), 'N': (782, 770), 'B': (830, 840), 'R': (1289, 1310), 'Q': (2529, 2529), 'K':(43000, 43000)}


piece_square_tables = {
  'P' : ( (0,0), (0,0), (0,0), (0,0), (0,0), (0,0), (0,0), (0,0),
          (-7,-1), (6,-14), (-2,13), (-11,22), (4,24), (-14,17), (10,7), (-9,7),
          (3,27), (-11,18), (-6,19), (22,29), (-8,30), (-5,9), (-14,8), (-11,14),
          (11,12), (-4,6), (-11,2), (2,-6), (11,-5), (0,-4), (-12,14), (5,9),
          (-3,7), (-20,1), (8,-8), (19,-2), (39,-14), (17,-13), (2,-11), (-5,-6),
          (-9,-9), (-15,-7), (11,-10), (15,5), (31,2), (23,3), (6,-8), (-20,-5),
          (2,-8), (4,-6), (11,9), (18,5), (16,16), (21,6), (9,-6), (-3,-18),
          (0,0), (0,0), (0,0), (0,0), (0,0), (0,0), (0,0), (0,0),),

  'B' : ( (-34, -32), (-1, -29), (-10, -26), (-16, -17), (-16, -17), (-10, -26), (-1, -29), (-34, -32),
          (-12, -22), (-10, -14), (4, -1), (0, 1), (0, 1), (4, -1), (-10, -14), (-12, -22),
          (-11,-21), (4,4), (1,3), (8,4), (8,4), (1,3), (4,4), (-11,-21),
          (-8,-12), (20,-1), (15,-10), (22,11), (22,11), (15,-10), (20,-1), (-8,-12),
          (-4,-14), (8,-4), (18,0), (27,12), (27,12), (18,0), (8,-4), (-4,-14),
          (-5,-11), (15,-1), (-4,-1), (12,7), (12,7), (-4,-1), (15,-1), (-5,-11),
          (-11,-26), (6,-9), (13,-12), (3,1), (3,1), (13,-12), (6,-9), (-11,-26),
          (-37,-40), (-4,-21), (-6,-26), (-16,-8), (-16,-8), (-6,-26), (-4,-21), (-37,-40),),
  
  'N' : ( (-201, -100), (-84, -88), (-56, -56), (-26, -17), (-26, -17), (-56, -56), (-84, -88), (-201, -100),
          (-67,-69), (-27,-50), (4,-51), (37,12), (37,12), (4,-51), (-27,-50), (-67,-69),
          (-9,-51), (22,-44), (58,-16), (53,17), (53,17), (58,-16), (22,-44), (-9,-51),
          (-34,-45), (13,-16), (44,9), (51,39), (51,39), (44,9), (13,-16), (-34,-45),
          (-35,-35), (8,-2), (40,13), (49,28), (49,28), (40,13), (8,-2), (-35,-36),
          (-61,-40), (-17,-27), (6,-8), (12,29), (12,29), (6,-8), (-17,-27), (-61,-40),
          (-77,-67), (-41,-54), (-27,-18), (-15,8), (-15,8), (-27,-18), (-41,-54), (-77,-67),
          (-175,-96), (-92,-65), (-74,-49), (-73,-21), (-73,-21), (-74,-49), (-92,-65), (-175,-96),),

  'R' : ( (-17,-18), (-19,0), (-1,19), (9,13), (9,13), (-1,19), (-19,0), (-17,-18),
          (-2,4), (12,5), (16,20), (18,-5), (18,-5), (16,20), (12,5), (-2,4),
          (-22,6), (-2,1), (6,-7), (12,10), (12,10), (6,-7), (-2,1), (-22,6),
          (-27,-5), (-15,8), (-4,7), (3,-6), (3,-6), (-4,7), (-15,8), (-27,-5),
          (-13,-6), (-5,1), (-4,-9), (-6,7), (-6,7), (-4,-9), (-5,1), (-13,-6),
          (-25,6), (-11,-8), (-1,-2), (3,-6), (3,-6), (-1,-2), (-11,-8), (-25,6),
          (-21,-12), (-13,-9), (-8,-1), (6,-2), (6,-2), (-8,-1), (-13,-9), (-21,-12),
          (-31,-9), (-20,-13), (-14,-10), (-5,-9), (-5,-9), (-14,-10), (-20,-13), (-31,-9),),

  'Q' : ( (-2,-74), (-2,-52), (1,-43), (-2,-34), (-2,-34), (1,-43), (-2,-52), (-2,-74),
          (-5,-50), (6,-27), (10,-24), (8,-8), (8,-8), (10,-24), (6,-27), (-5,-50),
          (-4,-38), (10,-18), (6,-11), (8,1), (8,1), (6,-11), (10,-18), (-4,-38),
          (0,-29), (14,-6), (12,9), (5,21), (5,21), (11,9), (14,-6), (0,-29),
          (4,-23), (5,-3), (9,13), (8,24), (8,24), (9,13), (5,-3), (4,-13),
          (-3,-39), (6,-18), (13,-9), (7,3), (7,3), (13,-9), (6,-18), (-3,-39),
          (-3,-54), (5,-31), (8,-22), (12,-4), (12,-4), (8,-22), (5,-31), (-3,-54),
          (3,-69), (-5,-57), (-5,-47), (4,-26), (4,-26), (-5,-47), (-5,-57), (3,-69),),

  'K' : ( (59,11), (89,59), (45,73), (-1,78), (-1,78), (45,73), (89,59), (59,11),
          (88,47), (120,121), (65,116), (33,131), (33,131), (65,116), (120,121), (88,47),
          (123,92), (145,172), (81,184), (31,191), (31,191), (81,184), (145,172), (123,92),
          (154,96), (179,166), (105,199), (70,199), (70,199), (105,199), (179,166), (154,96),
          (164,103), (190,156), (138,172), (98,172), (98,172), (138,172), (190,156), (164,103),
          (195,88), (258,130), (169,169), (120,175), (120,175), (169,169), (258,130), (195,88),
          (278,53), (303,100), (234,133), (179,135), (179,135), (234,133), (303,100), (278,53),
          (271,1), (327,45), (271,85), (198,76), (198,76), (271,85), (327,45), (271,1),),
  
}

# pad tables and add material value to pst dicts
for k, table in piece_square_tables.items():
  padrow = lambda row: (0,) + tuple((x[0] + piece[k][0], x[1] + piece[k][1]) for x in row) + (0,)
  piece_square_tables[k] = sum((padrow(table[i*8:i*8+8]) for i in range(8)), ())
  piece_square_tables[k] = (0,)*20 + piece_square_tables[k] + (0,)*20



#------------------------------------------------------------
# GLOBAL CONSTANTS
#------------------------------------------------------------

# Board is a 120 character 10x12 (width includes newlines)
# Having each line 10 long allows easy up and down by adding or subtracting 10 from index
A1, H1, A8, H8 = 91, 98, 21, 28
initial = (
  '         \n'  #   0 -  9
  '         \n'  #  10 - 19
  ' rnbqkbnr\n'  #  20 - 29
  ' pppppppp\n'  #  30 - 39
  ' ........\n'  #  40 - 49
  ' ........\n'  #  50 - 59
  ' ........\n'  #  60 - 69
  ' ........\n'  #  70 - 79
  ' PPPPPPPP\n'  #  80 - 89
  ' RNBQKBNR\n'  #  90 - 99
  '         \n'  # 100 -109
  '         \n'  # 110 -119
)

# Compass directions as relative indexes in string
N, E, S, W = -10, 1, 10, -1
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
MATE_LOWER = piece['K'][1] - 10*piece['Q'][1]
MATE_UPPER = piece['K'][1] + 10*piece['Q'][1]

#------------------------------------------------------------
# CHESS GAME
#------------------------------------------------------------

def calculate_phase(remaining_pieces):
  e = 2.718
  phase = min(max(1-(1/(1+e**(.2*(-remaining_pieces+16))) * 1.5 - .25), 0), 1)
  return phase

class Position(namedtuple('Position', 'board score white_castle black_castle en_passant king_passant remaining_pieces')):
  def gen_moves(self): #returns moves in format (start pos, end pos)
    for i, p in enumerate(self.board):
      if not p.isupper(): continue
      for d in directions[p]:
        for j in count(i+d, d):
            q = self.board[j]
            # Stay inside the board, and off friendly pieces
            if q.isspace() or q.isupper(): break
            # Pawn move, double move and capture
            if p == 'P' and d in (N, N+N) and q != '.': break
            if p == 'P' and d == N+N and (i < A1+N or self.board[i+N] != '.'): break
            if p == 'P' and d in (N+W, N+E) and q == '.' \
                and j not in (self.en_passant, self.king_passant, self.king_passant-1, self.king_passant+1): break
            # Move it
            yield (i, j)
            # Stop crawlers from sliding, and sliding after captures
            if p in 'PNK' or q.islower(): break
            # Castling, by sliding the rook next to the king
            if i == A1 and self.board[j+E] == 'K' and self.white_castle[0]: yield (j+E, j+W)
            if i == H1 and self.board[j+W] == 'K' and self.white_castle[1]: yield (j+W, j+E)

  def rotate(self):
    ''' flips board, maintains enpassant '''
    return Position(
      self.board[::-1].swapcase(), -self.score, self.black_castle, self.white_castle, 
      119-self.en_passant if self.en_passant else 0, 
      119-self.king_passant if self.king_passant else 0, self.remaining_pieces
    )

  def nullmove(self):
    ''' Like rotate, but clear passant '''
    return Position(
      self.board[::-1].swapcase(), -self.score,
      self.black_castle, self.white_castle, 0, 0, self.remaining_pieces
    )

  def move(self, move):
    start_pos, end_pos = move
    piece, capture = self.board[start_pos], self.board[end_pos]
    put = lambda board, start_pos, piece: board[:start_pos] + piece + board[start_pos+1:]

    remaining_pieces = self.remaining_pieces if capture == '.' else self.remaining_pieces - 1
    
    # Copy variables and reset ep and kp
    board = self.board
    white_castle, black_castle, en_passant, king_passant = self.white_castle, self.black_castle, 0, 0
    score = self.score + self.lazy_value(move)
    
    # Update board
    board = put(board, end_pos, board[start_pos])
    board = put(board, start_pos, '.')
    
    # if king or rook are moving revoke castling rights
    if start_pos == A1: white_castle = (False, white_castle[1])
    if start_pos == H1: white_castle = (white_castle[0], False)
    if end_pos == A8: black_castle = (black_castle[0], False)
    if end_pos == H8: black_castle = (False, black_castle[1])
    # Castling
    if piece == 'K':
      white_castle = (False, False)
      if abs(end_pos - start_pos) == 2:
        king_passant = (start_pos + end_pos) // 2
        board = put(board, A1 if end_pos < start_pos else H1, '.')
        board = put(board, king_passant, 'R')
    
    # Pawn promotion, double move, and en passant
    if piece == 'P':
      if A8 <= end_pos <= H8: board = put(board, end_pos, 'Q')
      if end_pos - start_pos == N+N: en_passant = start_pos + N
      if end_pos == self.en_passant: board = put(board, end_pos + S, '.')
    
    return Position(board, score, white_castle, black_castle, en_passant, king_passant, remaining_pieces).rotate()
  
  #returns value +/- as result of move
  def lazy_value_phase(self, move, game_stage):
    start_pos, end_pos = move
    piece, capture = self.board[start_pos], self.board[end_pos]
    # Actual move
    score = piece_square_tables[piece][end_pos][game_stage] - piece_square_tables[piece][start_pos][game_stage]
    # WHEN IMPLEMENTED TABLES FOR POSITIONS DO THAT HERE
    # Capture
    if capture.islower():
      score += piece_square_tables[capture.upper()][119-end_pos][game_stage]
    # Castling
    if piece == 'K' and abs(start_pos-end_pos) == 2:
      score += piece_square_tables['R'][(start_pos+end_pos)//2][game_stage]
      score -= piece_square_tables['R'][A1 if end_pos < start_pos else H1][game_stage]
    
    if piece == 'P':
      if A8 <= end_pos <= H8:
        score += piece_square_tables['Q'][end_pos][game_stage] - piece_square_tables['P'][end_pos][game_stage]
      if end_pos == self.en_passant:
        score += piece_square_tables['P'][119-(end_pos + S)][game_stage]

    return score

  def lazy_value(self, move):
    start_pos, end_pos = move
    piece, capture = self.board[start_pos], self.board[end_pos]

    remaining_pieces = self.remaining_pieces if capture == '.' else self.remaining_pieces - 1

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

  def depth_one_best_move(self, position):
    pass

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

  def alpha_beta_memory(self, position : Position, root: bool, alpha=-MATE_UPPER, beta=MATE_UPPER, depth=8):
    
    
    
    
    # if already searched this just use the computed score
    entry = self.transposition_score.get((position, depth, root), Entry(-MATE_UPPER, MATE_UPPER))
    if entry.lower >= beta and (not root or self.transposition_move.get(position) is not None):
      return entry.lower
    elif entry.upper <= alpha:
      return entry.upper

    if depth == 0:
      return self.quiesce(position, alpha, beta)

    best_score = -MATE_UPPER

    king_take_global = False

    for move in sorted(position.gen_moves(), key=position.lazy_value, reverse=True):
      king_take = False
      if position.board[move[1]] == 'k': # if king is captured we dont need to go down further in the tree for trades this game is over so we are at a leaf
        logging.debug('taking k with ' + position.board[move[0]])
        score = MATE_UPPER + depth # mates that take longer worth less
        king_take = True
        #score = -position.move(move).score
      else: 
        score, mate = self.alpha_beta_memory(position.move(move), False, -beta, -alpha, depth - 1)
        score = -score
        if mate and position.board[move[0]] == 'K' and abs(move[0]-move[1]) == 2:
          pass
      if score >= beta:
        # if root: return move, beta, ()
        #return beta, king_take
        if root: 
          best_move = move
          self.transposition_move[position] = move
        alpha = beta
        king_take_global = king_take
        break
      if score > best_score:
        king_take_global = king_take
        if root: 
          best_move = move
          self.transposition_move[position] = move
        best_score = score
        if score > alpha:
          alpha = score
    
    if len(self.transposition_score) > TABLE_SIZE: self.transposition_score.clear()

    if best_score <= alpha:
      self.transposition_score[position, depth, root] = Entry(entry.lower, best_score)
    if beta > best_score > alpha: # found accurate minmax, this will never occur with zero window (alpha and beta are the same)
      self.transposition_score[position, depth, root] = Entry(best_score, best_score)
    if best_score >= beta:
      self.transposition_score[position, depth, root] = Entry(best_score, entry.upper)
    
    if root: return best_move, best_score, ()
    return best_score, king_take_global

  #alpha beta but alpha == beta
  def alpha_beta_zero_window(self, position: Position, root: bool, gamma, depth, history=()):
    
    depth = max(depth, 0)

    if depth == 0:
      #return self.quiesce_zero_window(position, gamma)
      return self.quiesce_zero_window(position, gamma, history=history)

    if position.score <= -MATE_LOWER:
      return -MATE_UPPER - depth, False

    #CHECK FOR DRAWS
    if not root and position in history:
      return 0, False

    entry = self.transposition_score.get((position, depth, root), Entry(-MATE_UPPER, MATE_UPPER))
    if entry.lower >= gamma and (not root or self.transposition_move.get(position) is not None):
      if root: return entry.lower
      else: return entry.lower, False
    elif entry.upper < gamma:
      if root: return entry.upper
      else: return entry.upper, False

    best_score = -MATE_UPPER
    rotated = position.nullmove()
    check = any(rotated.lazy_value(m) >= MATE_LOWER for m in rotated.gen_moves())

    # generator of moves to search
    def moves():

      if not root and any(c in position.board for c in 'RBNQ'):
        score, mate = self.alpha_beta_zero_window(position.nullmove(), False, 1-gamma, depth-3, history=history)
        yield None, -score

      killer = self.transposition_move.get(position)
      if killer:
        score, mate = self.alpha_beta_zero_window(position.move(killer), False, 1-gamma, depth - 1, history=history) #-(gamma - 1)
        if check and position.board[killer[0]] == 'K' and abs(killer[0]-killer[1]) == 2: #if the next move is a mate and we are trying to castle cancel that shit!
          pass
        else: yield killer, -score
        
      
      for move in sorted(position.gen_moves(), key=position.lazy_value, reverse=True):
        score, mate = self.alpha_beta_zero_window(position.move(move), False, 1-gamma, depth - 1, history=history)
        score = score
        if check and position.board[move[0]] == 'K' and abs(move[0]-move[1]) == 2:
          pass
        else: yield move, -score
      
    for move, score in moves():
      best_score = max(best_score, score)
      if best_score >= gamma:
        #clear transposition table first
        
        if len(self.transposition_move) > TABLE_SIZE: self.transposition_move.clear()
        
        self.transposition_move[position] = move
        
        break

    if best_score < gamma and best_score < 0:
      is_dead = lambda position: any(position.lazy_value(m) >= MATE_LOWER for m in position.gen_moves())
      if all(is_dead(position.move(m)) for m in position.gen_moves()):
        in_check = is_dead(position.nullmove())
        best_score = -MATE_UPPER - depth if in_check else 0

    if len(self.transposition_score) > TABLE_SIZE: self.transposition_score.clear()

    if best_score >= gamma:
      self.transposition_score[position, depth, root] = Entry(best_score, entry.upper) #fail high
    else:
      self.transposition_score[position, depth, root] = Entry(entry.lower, best_score) #fail low
    
    if root: return best_score
    else: return best_score, False

    


  def quiesce_zero_window(self, position : Position, gamma, history=()):
    if position.score <= -MATE_LOWER:
      return -MATE_UPPER, False

    #CHECK FOR DRAWS
    if position in history:
      return 0, False
    
    entry = self.transposition_score.get((position, 0, False), Entry(-MATE_UPPER, MATE_UPPER))
    if entry.lower >= gamma:
      return entry.lower, False
    elif entry.upper < gamma:
      return entry.upper, False

    best_score = -MATE_UPPER
    rotated = position.nullmove()
    check = any(rotated.lazy_value(m) >= MATE_LOWER for m in rotated.gen_moves())

    def moves(): #FIXME IF IN CHECK WHEN QUISCENCE HAPPENS BUT THERE ARE NO TAKES TO GET OUT OF THAT CHECK, THEN EVALUATION STOPS, IN CASE OF A FORK YOU WANT TO GO A LEVEL DEEPER
      yield None, position.score

      killer = self.transposition_move.get(position)
      if killer and position.board[killer[1]].islower():
        score, mate = self.quiesce_zero_window(position.move(killer), 1-gamma, history=history) #-(gamma - 1)
        if check and position.board[killer[0]] == 'K' and abs(killer[0]-killer[1]) == 2: #if the next move is a mate and we are trying to castle cancel that shit!
          pass
        else: yield killer, -score
      
      for move in sorted(position.gen_moves(), key=position.lazy_value, reverse=True):
        #if position.board[move[1]].islower(): 
        if position.lazy_value(move) >= 200:
          score, mate = self.quiesce_zero_window(position.move(move), 1-gamma, history=history)
          if check and position.board[move[0]] == 'K' and abs(move[0]-move[1]) == 2:
            pass
          else: yield move, -score
    
    for move, score in moves():
      best_score = max(best_score, score)
      if best_score >= gamma:
        #clear transposition table first
        
        if len(self.transposition_move) > TABLE_SIZE: self.transposition_move.clear()
        
        self.transposition_move[position] = move
        
        break
    
    if len(self.transposition_score) > TABLE_SIZE: self.transposition_score.clear()

    if best_score >= gamma:
      self.transposition_score[position, 0, False] = Entry(best_score, entry.upper) #fail high
    else:
      self.transposition_score[position, 0, False] = Entry(entry.lower, best_score) #fail low
    
    return best_score, False

    


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

    move = self.transposition_move.get(position)
    move_score = self.transposition_score.get((position, depth, True))
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
    move = self.transposition_move.get(position)
    move_score = self.transposition_score.get((position, finaldepth, True))
    return move, move_score.lower, move_score.upper

  

    



def parse(c):
  fil, rank = ord(c[0]) - ord('a'), int(c[1]) - 1
  return A1 + fil - 10*rank

def render(i):
  rank, fil = divmod(i - A1, 10)
  return chr(fil + ord('a')) + str(-rank + 1)



    
