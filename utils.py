import itertools
import re
import time
import sys

import chesster

from loggering import *

#This module has utils for running with UCI and integrating with lichess and such

WHITE, BLACK = range(2)

FEN_INITIAL = 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1'

def can_kill_king(pos):
  # If we just checked for opponent moves capturing the king, we would miss
  # captures in case of illegal castling.
  return any(pos.value(m) >= chesster.MATE_LOWER for m in pos.gen_moves())

################################################################################
# Parse and Render positions
################################################################################

def mrender(pos, m):
  # Sunfish always assumes promotion to queen
  p = 'q' if chesster.A8 <= m[1] <= chesster.H8 and pos.board[m[0]] == 'P' else ''
  m = m if get_color(pos) == WHITE else (119-m[0], 119-m[1])
  return chesster.render(m[0]) + chesster.render(m[1]) + p

def mparse(color, move):
  m = (chesster.parse(move[0:2]), chesster.parse(move[2:4]))
  return m if color == WHITE else (119-m[0], 119-m[1])

def get_color(pos):
  ''' A slightly hacky way to to get the color from a sunfish position '''
  return BLACK if pos.board.startswith('\n') else WHITE

def parseFEN(fen):
  """ Parses a string in Forsyth-Edwards Notation into a Position """

  logging.debug('parsing FEN')
  board, color, castling, enpas, _hclock, _fclock = fen.split()
  board = re.sub(r'\d', (lambda m: '.'*int(m.group(0))), board)
  board = list(21*' ' + '  '.join(board.split('/')) + 21*' ')
  board[9::10] = ['\n']*12
  #if color == 'w': board[::10] = ['\n']*12
  #if color == 'b': board[9::10] = ['\n']*12
  board = ''.join(board)
  wc = ('Q' in castling, 'K' in castling)
  bc = ('k' in castling, 'q' in castling)
  
  ep = chesster.parse(enpas) if enpas != '-' else 0
  # score = sum(chesster.pst[p][i] for i,p in enumerate(board) if p.isupper())
  # score -= sum(chesster.pst[p.upper()][119-i] for i,p in enumerate(board) if p.islower())
  score = 0
  pos = chesster.Position(board, score, wc, bc, ep, 0)
  return pos if color == 'w' else pos.rotate()

################################################################################
# Non chess related tools
################################################################################



