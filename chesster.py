from itertools import count
from collections import namedtuple
from tokenize import endpats
from tracemalloc import start

from loggering import *

import utils



#------------------------------------------------------------
# PIECE VALUES
#------------------------------------------------------------

piece = { 'P': 136, 'N': 782, 'B': 830, 'R': 1289, 'Q': 2529, 'K':42000}

piece_square_tables = {
  'P' : ( 0, 0, 0, 0, 0, 0, 0, 0, # Last rank, no pawns
          15, 31, 20, 14, 23, 11, 37, 24, #
          -1, -3, 15, 26, 1, 10, -7, -9, #
          8, -1, -5, 13, 24, 11, -10, 3, #
          -9, -18, 8, 32, 43, 25, -4, -16, #
          -9, -13, -40, 22, 26, -40, 1, -22, #
          2, 0, 15, 3, 11, 22, 11, -1, #
          0, 0, 0, 0, 0, 0, 0, 0,),#1
#          A    B    C    D    E    F    G    H
  'B' : ( -48, -3, -12, -25, -25, -12, -3, -48, #
          -21, -19, 10, -6, -6, 10, -19, -21, #
          -17, 4, -1, 8, 8, -1, 4, -17, #
          -7, 30, 23, 28, 28, 23, 30, -7, #
          1, 8, 26, 37, 37, 26, 8, 1, #
          -8, 24, -3, 15, 15, -3, 24, -8, #
          -18, 7, 14, 3, 3, 14, 7, -18, #
          -44, -4, -11, -28, -28, -11, -4, -44,),#1
#          A    B    C    D    E    F    G    H
  'R' : ( -22, -24, -6, 4, 4, -6, -24, -22, #
          -8, 6, 10, 12, 12, 10, 6, -8, #
          -24, -4, 4, 10, 10, 4, -4, -24, #
          -24, -12, -1, 6, 6, -1, -12, -24, #
          -13, -5, -4, -6, -6, -4, -5, -13, #
          -21, -7, 3, -1, -1, 3, -7, -21, #
          -18, -10, -5, 9, 9, -5, -10, -18, #
          -24, -13, -7, 2, 2, -7, -13, -24,),#1
#          A    B    C    D    E    F    G    H
  'N' : ( -200, -80, -53, -32, -32, -53, -80, -200, #
          -67, -21, 6, 37, 37, 6, -21, -67, #
          -11, 28, 63, 55, 55, 63, 28, -11, #
          -29, 13, 42, 52, 52, 42, 13, -29, #
          -28, 5, 41, 47, 47, 41, 5, -28, #
          -64, -20, 4, 19, 19, 4, -20, -64, #
          -79, -39, -24, -9, -9, -24, -39, -79, #
          -169, -96, -80, -79, -79, -80, -96, -169,),#1
#          A    B    C    D    E    F    G    H
  'Q' : ( -2, -2, 1, -2, -2, 1, -2, -2, #
          -5, 6, 10, 8, 8, 10, 6, -5, #
          -4, 10, 6, 8, 8, 6, 10, -4, #
          0, 14, 12, 5, 5, 12, 14, 0, #
          4, 5, 9, 8, 8, 9, 5, 4, #
          -3, 6, 13, 7, 7, 13, 6, -3, #
          -3, 5, 8, 12, 12, 8, 5, -3, #
          3, -5, -5, 4, 4, -5, -5, 3,),#1
#          A    B    C    D    E    F    G    H
  'K' : ( 6, 8, 4, 0, 0, 4, 8, 6, #
          8, 12, 6, 2, 2, 6, 12, 8, #
          12, 15, 8, 3, 3, 8, 15, 12, #
          14, 17, 11, 6, 6, 11, 17, 15, #
          16, 19, 13, 10, 10, 13, 19, 16, #
          19, 25, 16, 12, 12, 16, 25, 19, #
          27, 30, 24, 18, 18, 24, 30, 27, #
          27, 32, 27, 19, 19, 27, 32, 27,) #1
#          A    B    C    D    E    F    G    H
}

# pad tables and add material value to pst dicts
for k, table in piece_square_tables.items():
  padrow = lambda row: (0,) + tuple(x+piece[k] for x in row) + (0,)
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
MATE_LOWER = piece['K'] - 10*piece['Q']
MATE_UPPER = piece['K'] + 10*piece['Q']

#------------------------------------------------------------
# CHESS GAME
#------------------------------------------------------------

class Position(namedtuple('Position', 'board score white_castle black_castle en_passant king_passant')):
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
    # For each piece, iterate through their directions and break based on captures or instantly for knights and pawns
    # for start_pos, piece in enumerate(self.board):
    #   if not piece.isupper(): continue
    #   for direction in directions[piece]:
    #     for end_pos in count(start_pos + direction, direction):
    #       end_space = self.board[end_pos]
    #       # Stay inside board and off friends
    #       if end_space.isspace() or end_space.isupper(): break
    #       # Pawn move double move and capture
    #       if piece == 'P' and direction in (N, N+N) and end_space != '.': break
    #       if piece == 'P' and direction == N+N and (start_pos < A1 + N or self.board[start_pos + N] != '.'): break
    #       if piece == 'P' and direction in (N+E, N+W) and end_space == '.' and end_pos not in (self.en_passant, self.king_passant, self.king_passant-1, self.king_passant+1): break
    #       # Yield move to generator
    #       yield (start_pos, end_pos)
    #       # Stop moves that only move once from flying
    #       if piece in 'PNK' or end_space.islower(): break
    #       # Castling, detected by rook movements, executed by moving king
    #       if start_pos == A1 and self.board[end_pos + E] == 'K' and self.white_castle[0] and self.board[end_pos + W] == '.': yield (end_pos+E, end_pos+W)
    #       if start_pos == H1 and self.board[end_pos + W] == 'K' and self.white_castle[1] and self.board[end_pos + E] == '.': yield (end_pos+W, end_pos+E)

  def rotate(self):
    ''' flips board, maintains enpassant '''
    return Position(
      self.board[::-1].swapcase(), -self.score, self.black_castle, self.white_castle, 
      119-self.en_passant if self.en_passant else 0, 
      119-self.king_passant if self.king_passant else 0
    )

  def nullmove(self):
    ''' Like rotate, but clear passant '''
    return Position(
      self.board[::-1].swapcase(), -self.score,
      self.black_castle, self.white_castle, 0, 0
    )

  def move(self, move):
    start_pos, end_pos = move
    piece, capture = self.board[start_pos], self.board[end_pos]
    put = lambda board, start_pos, piece: board[:start_pos] + piece + board[start_pos+1:]
    
    # Copy variables and reset ep and kp
    board = self.board
    white_castle, black_castle, en_passant, king_passant = self.white_castle, self.black_castle, 0, 0
    score = self.score + self.value(move)
    
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
    
    return Position(board, score, white_castle, black_castle, en_passant, king_passant).rotate()
  
  #returns value +/- as result of move
  def value(self, move):
    start_pos, end_pos = move
    piece, capture = self.board[start_pos], self.board[end_pos]
    # Actual move
    score = piece_square_tables[piece][end_pos] - piece_square_tables[piece][start_pos]
    # WHEN IMPLEMENTED TABLES FOR POSITIONS DO THAT HERE
    # Capture
    if capture.islower():
      score += piece_square_tables[capture.upper()][119-end_pos]
    # Castling
    if piece == 'K' and abs(start_pos-end_pos) == 2:
      score += piece_square_tables['R'][(start_pos+end_pos)//2]
      score -= piece_square_tables['R'][A1 if end_pos < start_pos else H1]
    
    if piece == 'P':
      if A8 <= end_pos <= H8:
        score += piece_square_tables['Q'][end_pos] - piece_square_tables['P'][end_pos]
      if end_pos == self.en_passant:
        score += piece_square_tables['P'][119-(end_pos + S)]

    return score

class Searcher():
  def __init__(self):
    self.top_score = {}
    self.top_move = {} # key: position object, object is tuple (move object (from pos.gen_moves()), )
    self.history = set()

  def depth_one_best_move(self, position):
    pass

  def negamax(self, position : Position, depth, verbose=False):
    max = -MATE_UPPER
    logging.debug('depth {} considering {}'.format(depth, position.board))
    for move in sorted(position.gen_moves(), key=position.value, reverse=True):
      if verbose: logging.debug(position.move(move).rotate().board)
      score = position.value(move)
      if depth != 1:
        score -= self.negamax(position.move(move), depth - 1, verbose)
      if score > max:
        max = score
        if verbose: logging.debug(score)
    return max


  def search(self, position : Position, depth):
    max = -MATE_UPPER
    best_move = None
    iter = 0
    for move in sorted(position.gen_moves(), key=position.value, reverse=True):
      iter += 1
      score = position.value(move)
      if depth != 1: 
        verbose = False
        score -= self.negamax(position.move(move), depth - 1, verbose)
      if score > max:
        max = score
        best_move = move
    return best_move, max

  def alpha_beta(self, position : Position, root: bool, alpha=-MATE_UPPER, beta=MATE_UPPER, depth=8):
    if root:
      best_move = None
    for move in sorted(position.gen_moves(), key=position.value, reverse=True):
      score = position.value(move)
      if depth > 1:
        score -= self.alpha_beta(position.move(move), False, -beta, -alpha, depth - 1)
      if score >= beta:
        if root: return move, beta
        return beta
      if score > alpha:
        alpha = score
        if root: best_move = move
    if root: return best_move, alpha
    return alpha

  def alpha_beta2(self, position : Position, root: bool, alpha=-MATE_UPPER, beta=MATE_UPPER, depth=8):
    if root:
      best_move = None
    if depth == 0:
      return self.quiesce(position, alpha, beta)
    for move in sorted(position.gen_moves(), key=position.value, reverse=True):
      if position.board[move[1]] == 'k': # if king is captured we dont need to go down further in the tree for trades this game is over so we are at a leaf
        logging.debug('taking k with ' + position.board[move[0]])
        score = -position.move(move).score
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
    
    for move in sorted(position.gen_moves(), key=position.value, reverse=True):
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

    



def parse(c):
  fil, rank = ord(c[0]) - ord('a'), int(c[1]) - 1
  return A1 + fil - 10*rank

def render(i):
  rank, fil = divmod(i - A1, 10)
  return chr(fil + ord('a')) + str(-rank + 1)



    
