openers_mickey = ('I looked forward in time, I saw 4e37 futures. You win none of them.', 'I\'m not very good at checkers', 
  'Bow down before Chesster: your robotic master!', 'Ever seen terminator?', 
  'Դու ինձ կանչում ես իմ փոսից, որտեղ ես սողացել եմ մեռնելու։ Դու ինձ փչացնում ես այդ նուրբ գրկախառնությամբ։ Այդ սառը ներումը։ Եվ դու ինձ դրեցիր այստեղ: Ինչի համար? Տառապել Ձեր ոտքերի մոտ ես քեզ հայհոյում եմ ես քեզ հայհոյում եմ Դուք չեք կարող կանգնեցնել անեծքը, այն չափազանց ուժեղ է',
  "I'm Chesster. This will be light work.",
  "I'm Chesster. This will be easy."
  )
openers_profane = ("What's up fuckface, I'm Chesster. This will be light work.")

after_my_move_mickey = ("I'm 6 moves ahead of you', 'Freelo", 'Ok wait this is kind of easy can you start trying?'
  '/checkmate', '/capture queen', '/promote pawn', '/check', '/clear e', '/clear g', '/en_passant',
  'Mate in 24.', 'Mate in 2', 'Mate in 6', 'Mate in 4', 'Mate in 10', 'Mate in 7', 'Mate in 11', 'Mate in 22', 'Mate in 19')
after_my_move_profane = ('I checkmate your mother!', 'Bitch')

take_piece_mickey = (
  'Noob down.', ':]', 'HeeHeeHeeHaw!', 'Thanks.', 'Nice move!', 'Yeah okay I\'m better.', 'Ez', 'lol', 'lmao', 'ROFL', 'Get merked', 'Wow.', 'ez',
  'too ez', 'lol mickey mouse game', 'okay now THIS is ez', 'simple', 'light work.',
  '''⠀⠀⠀⠀⠀⠀⠀⢀⡔⠋⢉⠩⡉⠛⠛⠛⠉⣉⣉⠒⠒⡦⣄⠀⠀⠀⠀⠀⠀⠀
   ⠀⠀⠀⠀⠀⠀⢀⠎⠀⠀⠠⢃⣉⣀⡀⠂⠀⠀⠄⠀⠀⠀⠀⢱⠀⠀⠀⠀⠀⠀ 
   ⠀⠀⠀⠀⠀⡰⠟⣀⢀⣒⠐⠛⡛⠳⢭⠆⠀⠤⡶⠿⠛⠂⠀⢈⠳⡀⠀⠀⠀⠀ 
   ⠀⠀⠀⠀⢸⢈⢘⢠⡶⢬⣉⠉⠀⠀⡤⠄⠀⠀⠣⣄⠐⠚⣍⠁⢘⡇⠀⠀⠀⠀
    ⠀⠀⠀⠀⠈⢫⡊⠀⠹⡦⢼⣍⠓⢲⠥⢍⣁⣒⣊⣀⡬⢴⢿⠈⡜⠀⠀⠀⠀⠀ 
    ⠀⠀⠀⠀⠀⠀⠹⡄⠀⠘⢾⡉⠙⡿⠶⢤⣷⣤⣧⣤⣷⣾⣿⠀⡇⠀⠀⠀⠀⠀ 
    ⠀⠀⠀⠀⠀⠀⠀⠘⠦⡠⢀⠍⡒⠧⢄⣀⣁⣀⣏⣽⣹⠽⠊⠀⡇⠀⠀⠀⠀⠀ 
    ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠑⠪⢔⡁⠦⠀⢀⡤⠤⠤⠄⠀⠠⠀⡇⠀⠀⠀⠀⠀ 
    ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠑⠲⠤⠤⣀⣀⣀⣀⣀⠔⠁''')
take_piece_profane = ('Get shit on!', "Yeah okay you're trash", 'Get fucked.', 'Get railed', 'Get pwnd', 'Bitch.', 'Dumbass.')

#said after taking a specific piece
take_piece_specific_mickey = {
  'P': ('lemme pawn that for you', 'ill take pawns', 'pawn is gawn'),
  'Q': ("That's the most valuable piece btw", "Wait really gonna give up the queen for free okay."),
  'N': ("We take these knights.", 'That knight just saw the light'),
  'B': ("",),
  'R': ('Read that rook like a book.')
}

take_piece_specific_profane = {
  'P': ('',),
  'Q': ('Captured your queen now time to sell her into slavery!',)
}

#this move is forecasted to put us up in the next lookahead
up_in_score = ('Watch this','Hehe watch this', 'Teehee!', 'Teehee...', 'lol', 'Be careful!', 'Prepare', 'Me: cooking something', 'LOL!')

#all after opponents move

after_opponent_move = (
  '???', '????', '?', '?!', 'Huh?', 'tf..?', '...okay', 'Okay.', 'lol', 'lmao', 'rofl', 'wait wut',
  'What a move!', 'Nice move!', 'Brilliant move!', 'Freelo lol', 'Seethe lol',
  "They don't know what they're doing! They don't know what they're doing!", 'I checkmate your mother!')

lose_piece_bad_mickey = (
  'Ok wait stop cheating', 'Wow okay.', "Yeah i don't want that it's fine. No really it is!", 
  'ես քեզ հայհոյում եմ։ ես քեզ հայհոյում եմ։ ես քեզ հայհոյում եմ։ ես քեզ հայհոյում եմ։ ես քեզ հայհոյում եմ։ ես քեզ հայհոյում եմ։ ես քեզ հայհոյում եմ։ ես քեզ հայհոյում եմ։'
)

lose_piece_bad_profane = (
  'Fuck off.', 'Fuck you.', 'Dumbass.'
)

#losing a piece but still predicting that we'll be in a good position in the next 6 moves
lose_piece_good_mickey = (
  "Yeah I didn't want that anyways", 'Alr lol', 'Hehe free', 'We trade these!', 'Crinkling the paper...', 'Aaand you fall for the trap.'
)

lose_piece_good_profane = (
  "lol you fell for my trap dumbass", '*yawwn* too easy idiot.'
)

#lost this specific piece

lose_piece_specific_mickey = {
  'P': ('Ok yeah pawns are useless',)
}

lose_piece_specific_profane = {
  'P': ('Shove that pawn up your ass.',)
}

#this specific piece took our piece
lose_to_piece = {
  'P': ('/capture p',),
  'N': ('/capture n',),
  'B': ('/capture b',),
  'R': ('/capture r',),
  'Q': ('/capture q',)
}