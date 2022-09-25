from __future__ import print_function
from __future__ import division
import importlib
from operator import truediv
import re
import sys
import time
import logging
import argparse

import requests

import random
from urllib.parse import urljoin

import utils
import chesster
import quips

from loggering import *

from utils import WHITE, BLACK, Unbuffered

profane = False



def send_chat(game_id, message, room):
  url = "https://lichess.org/"
  endpoint = "/api/bot/game/{}/chat"
  header = {
    "Authorization": f"Bearer lip_lEBfFRyeKPZz2naVFbMP"
  }
  payload = {'room': room, 'text': message}
  url = urljoin(url, endpoint.format(game_id))
  response = requests.post(url, headers=header, data=payload)
  #print(message)

def resign(game_id):
  url = "https://lichess.org/"
  endpoint = "/api/bot/game/{}/resign"
  header = {
    "Authorization": f"Bearer lip_lEBfFRyeKPZz2naVFbMP"
  }
  url = urljoin(url, endpoint.format(game_id))
  response = requests.post(url, headers=header)

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
  # searcher=chesster.Searcher()
  color = WHITE
  searcher=chesster.TranspositionOptimizedSearcher()
  our_time, opp_time = 1000, 1000
  show_thinking = True

  stack = []

  game_id = None

  estimated_score = None

  openers = None
  after_my_move = None
  take_piece = None
  take_piece_specific = None
  up_in_score = None
  after_opponent_move = None
  lose_piece_good = None
  lose_piece_bad = None
  lose_piece_specific = None
  lose_to_piece = None

  move_history = set()

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
      if profane:
        openers = (quips.openers_mickey, quips.openers_profane)
        after_my_move = (quips.after_my_move_mickey, quips.after_my_move_profane)
        take_piece = (quips.take_piece_mickey, quips.take_piece_profane)
        take_piece_specific = (quips.take_piece_specific_mickey, quips.take_piece_specific_profane)
        up_in_score = (quips.up_in_score,)
        after_opponent_move = (quips.after_opponent_move,)
        lose_piece_good = (quips.lose_piece_good_mickey, quips.lose_piece_good_profane)
        lose_piece_bad = (quips.lose_piece_bad_mickey, quips.lose_piece_bad_profane)
        lose_piece_specific = (quips.lose_piece_specific_mickey, quips.lose_piece_specific_profane)
        lose_to_piece = (quips.lose_to_piece,)
      else:
        openers = (quips.openers_mickey,)
        after_my_move = (quips.after_my_move_mickey,)
        take_piece = (quips.take_piece_mickey,)
        take_piece_specific = (quips.take_piece_specific_mickey,)
        up_in_score = (quips.up_in_score,)
        after_opponent_move = (quips.after_opponent_move,)
        lose_piece_good = (quips.lose_piece_good_mickey,)
        lose_piece_bad = (quips.lose_piece_bad_mickey,)
        lose_piece_specific = (quips.lose_piece_specific_mickey,)
        lose_to_piece = (quips.lose_to_piece,)

    elif smove == 'isready':
      output('readyok')

    elif smove == 'ucinewgame':
      stack.append('position fen ' + utils.FEN_INITIAL)
    # UCI syntax:
    # position [fen  | startpos ]  moves ....

    elif smove.startswith('game_id'):
      
      game_id = smove.split(' ')[1]
      #get list of all opener quips
      opener_quips = ()
      for opener_quips_lists in openers:
        opener_quips = opener_quips + opener_quips_lists
      quip = random.choice(opener_quips)
      
      send_chat(game_id, quip, 'player')
      send_chat(game_id, quip, 'spectator')
      #print(quip)

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

      
      last_move = None
      last_position = None
      move_history = []
      idx = -1
      for move in moveslist:
        idx += 1
        parse = utils.mparse(color, move)
        
        last_move = parse
        last_position = pos
        pos = pos.move(parse)
        if move_history.__contains__(pos):
          pass
        else: move_history.append(pos)
        color = 1 - color
      

      move_history = set(move_history)

      if last_move and game_id:
        #funny quip time!
        opp_move_quips = ()
        for quip_list in after_opponent_move:
          opp_move_quips += quip_list
        if last_position.board[last_move[1]].islower(): #opponent has taken!
          if estimated_score and estimated_score > 0:
            for quip_list in lose_piece_good:
              opp_move_quips += quip_list
          elif estimated_score:
            for quip_list in lose_piece_bad:
              opp_move_quips += quip_list
          #lost a specific piece
          for quip_dict in lose_piece_specific:
            if last_position.board[last_move[1]].upper() in quip_dict.keys():
              opp_move_quips += quip_dict[last_position.board[last_move[1]].upper()]
          #lost to specific piece
          for quip_dict in lose_to_piece:
            if last_position.board[last_move[0]] in quip_dict.keys():
              opp_move_quips += quip_dict[last_position.board[last_move[0]]]
        quip = random.choice(opp_move_quips)
        send_chat(game_id, quip, 'player')
        send_chat(game_id, quip, 'spectator')
            

      
      

    elif smove.startswith('go'): # make my move
      logging.debug(pos.board)
      depth = 1000
      movetime = -1
      fallbacktime = -1
      movesremain = 45
      our_time = None
      _, *params = smove.split(' ')
      for param, val in zip(*2*(iter(params),)):
        if param == 'depth':
          depth = int(val)
        if param == 'movetime':
          movetime = int(val)
        if param == 'wtime':
          if color is WHITE: our_time = int(val)
        if param == 'btime':
          if color is BLACK: our_time = int(val)
        if param == 'fallbacktime':
          fallbacktime = int(val)
        if param == 'movesremain':
          movesremain = int(val)
        
      start = time.time()

      # logging.debug('generate moves')
      
      # moves = pos.gen_moves()

      # movelist = list(moves)

      # logging.debug('finish generate moves')

      # logging.debug(movelist)

      # move = random.choice(movelist)

      # logging.debug(move)
      before = time.perf_counter()
      # if our_time < 180000: #5 mins
      #   depth -= 1
      if our_time == None:
        our_time = fallbacktime

      move, score, upper = searcher.iterative_deepening_mtdbi(pos, True, depth, our_time / movesremain, history=move_history)
      after = time.perf_counter()

      estimated_score =  score - pos.score

      logging.debug(pos.en_passant)

      logging.debug(pos.move(move).rotate().board)

      # if game_id and move[2]:
      #   msg = random.choice(move[2])
      #   send_chat(game_id, msg, 'player')
      #   send_chat(game_id, msg, 'spectator')
      
      if game_id:
        my_move_quips = ()
        #funny quip time!
        for quip_list in after_my_move:
          my_move_quips += quip_list
        if pos.board[move[1]].islower():
          #capturing a piece!
          for quip_list in take_piece:
            my_move_quips += quip_list
          for quip_dict in take_piece_specific:
            if pos.board[move[1]].upper() in quip_dict.keys():
              my_move_quips += quip_dict[pos.board[move[1]].upper()]
        if score - pos.score > 200:
          for quip_list in up_in_score:
            my_move_quips += quip_list

        quip = random.choice(my_move_quips)
        send_chat(game_id, quip, 'player')
        send_chat(game_id, quip, 'spectator')

      # print('did alpha-beta in {} seconds'.format(after - before))

      output('info teehee')

      if score - depth < -chesster.MATE_UPPER and game_id:
        resign(game_id)
        break
      else:
        output('bestmove ' + utils.mrender(pos, move))

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