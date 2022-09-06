from __future__ import print_function
from __future__ import division
import importlib
from operator import truediv
import re
import sys
import time
import logging
import argparse

import random

import utils
import chesster

from loggering import *

from utils import WHITE, BLACK, Unbuffered

def main():
  parser = argparse.ArgumentParser()
  parser.add_argument('module', help='chesster.py file (without py)', type=str, default='chesster', nargs='?')
  parser.add_argument('--tables', metavar='pst', help='alternative pst table', type=str, default=None)
  args=parser.parse_args()

  chesster = importlib.import_module(args.module)
  if args.tables is not None:
    pst_module = importlib.import_module(args.table)
    chesster.pst = pst_module.pst

  
  pos = utils.parseFEN(utils.FEN_INITIAL)
  searcher=chesster.Searcher()
  our_time, opp_time = 1000, 1000
  show_thinking = True

  stack = []
  while True:
    if stack:
      smove = stack.pop()
    else:
      smove = input()

    logging.debug(f'>>> {smove}')

    if smove == 'quit':
      break
    elif smove == 'uci':
      output('id name Chesster')
      output('id author Mikhael BenZvi')
      output('uciok')

    elif smove == 'isready':
      output('readyok')

    elif smove == 'ucinewgame':
      stack.append('position fen ' + utils.FEN_INITIAL)

    # UCI syntax:
    # position [fen  | startpos ]  moves ....

    elif smove.startswith('position'):
      logging.debug('started parsing pos')
      params = smove.split(' ')
      idx = smove.find('moves')

      if idx >= 0:
        moveslist = smove[idx:].split()[1:]
      else:
        moveslist = []

      if params[1] == 'fen':
        if idx >= 0:
          fenpart = smove[:idx]
        else:
          fenpart = smove

        _, _, fen = fenpart.split(' ', 2)
      

      elif params[1] == 'startpos':
        fen = utils.FEN_INITIAL

      else:
        pass

      pos = utils.parseFEN(fen)
      
      color = WHITE if fen.split()[1] == 'w' else BLACK

      

      for move in moveslist:
        parse = utils.mparse(color, move)
        logging.debug(parse)
        pos = pos.move(parse)
        logging.debug('done parsing pos')
        color = 1 - color

      
      

    elif smove.startswith('go'): # make my move
      logging.debug(pos.board)
      depth = 1000
      movetime = -1
      
      _, *params = smove.split(' ')
      for param, val in zip(*2*(iter(params),)):
        if param == 'depth':
          depth = int(val)
        if param == 'movetime':
          movetime = int(val)
        if param == 'wtime':
          our_time = int(val)
        if param == 'btime':
          opp_time = int(val)
        
      
      
      moves_remain = 40

      start = time.time()

      # logging.debug('generate moves')
      
      # moves = pos.gen_moves()

      # movelist = list(moves)

      # logging.debug('finish generate moves')

      # logging.debug(movelist)

      # move = random.choice(movelist)

      # logging.debug(move)
      before = time.perf_counter()
      move = searcher.alpha_beta2(pos, True, depth=depth)
      after = time.perf_counter()

      logging.debug(pos.en_passant)

      logging.debug(pos.move(move[0]).rotate().board)

      print('did alpha-beta in {} seconds'.format(after - before))

      output('bestmove ' + utils.mrender(pos, move[0]))

      # before = time.perf_counter()
      # move = searcher.search(pos, depth)
      # after = time.perf_counter()

      # print('did minimax in {} seconds'.format(after - before))

      # logging.debug(move[1])

      # output('bestmove ' + utils.mrender(pos, move[0]))

    elif smove.startswith('time'):
            our_time = int(smove.split()[1])

    elif smove.startswith('otim'):
        opp_time = int(smove.split()[1])

    else:
        pass

if __name__ == '__main__':
  main()