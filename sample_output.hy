('do'
  ('import' ['sys'])
  (('quote' 'defn') 'game' ['arguments' '\ufdd0:args' [] '\ufdd0:kwonlyargs' [] '\ufdd0:kw_defaults' [] '\ufdd0:defaults' []]
   ('setv' 'args' [])
   ('setv' 'inventory' [])
   (('quote' 'defn') 'setargs' ['arguments' '\ufdd0:args' [('arg' '\ufdd0:arg' 'l')] '\ufdd0:kwonlyargs' [] '\ufdd0:kw_defaults' [] '\ufdd0:defaults' []]
    ('nonlocal' 'args')
    ('setv' 'args' (('get' 'l' 'rstrip'))))
   (('quote' 'defn') 'getPromptObj' ['arguments' '\ufdd0:args' [('arg' '\ufdd0:arg' 'l')] '\ufdd0:kwonlyargs' [] '\ufdd0:kw_defaults' [] '\ufdd0:defaults' []]
    ('return' (',' 'p' 'l' 'setargs')))
   (('quote' 'defn') 'getNextObj' ['arguments' '\ufdd0:args' [] '\ufdd0:kwonlyargs' [] '\ufdd0:kw_defaults' [] '\ufdd0:defaults' []]
    ('return' (',' 'n')))
   ('print' 'You wake up to find yourself laying down inside a dark cave.')
   ('print' 'What will you do?')
   'None'
   (('quote' 'if') ('=' 'args' '0') ('do' ('print' 'You found a lighter inside your bag.') (('get' 'inventory' 'append') 'lighter')) ('do' (('quote' 'if') ('=' 'args' '1') ('do' ('print' 'Your voice echos through the cave...')) ('do'))))
   'None'
   ('while' 'True'
            ('print' 'What will you do next?')
            'None'
            (('quote' 'if') ('=' 'args' '0')
             ('do'
               ('print' 'You find a way leading outside of the cave.')
               ('break'))
             ('do'
               ('print' 'You spend several minutes in the dark, but no help seems to arrive...'))))
   ('print' 'To be continued...'))
  (('quote' 'defn') 'gameloop' ['arguments' '\ufdd0:args' [('arg' '\ufdd0:arg' 'game')] '\ufdd0:kwonlyargs' [] '\ufdd0:kw_defaults' [] '\ufdd0:defaults' []]
   ('for*' ['o' 'game']
           ('do'
             (('quote' 'if') ('not' 'o')
              ('do' ('break'))
              ('do'))
             (('quote' 'if') ('=' ('get' 'o' 0) 'n')
              ('do'
                ('print' 'Hit enter to continue...')
                (('get' ('get' 'sys' 'stdin') 'readline')) 'None')
              ('do'
                (('quote' 'if') ('=' ('get' 'o' 0) 'p')
                 ('do'
                   ('setv' 'd' ('get' 'o' 1))
                   ('for*' ['k' 'd']
                           ('do'
                             ('print' ('get' 'k' 0) ('get' 'k' 1))))
                   ('print' '> ' ('end' '') ('flush' 'True'))
                   ('setv' 'choice' (('get' ('get' 'sys' 'stdin') 'readline')))
                   (('get' 'o' 2) 'choice')
                   'None')
                 ('do'))))))) 
  (('quote' 'defn') 'trynext' ['arguments' '\ufdd0:args' [('arg' '\ufdd0:arg' 'g')] '\ufdd0:kwonlyargs' [] '\ufdd0:kw_defaults' [] '\ufdd0:defaults' []]
   ('try'
     ('next' 'g')
     ('except' 'None')
     ('else')
     ('finally'))) 
  ('setv' 'game1' ('gameloop' ('game')))
  ('setv' 'game2' ('gameloop' ('game')))
  ('trynext' 'game1')
  ('trynext' 'game2')
  ('trynext' 'game1')
  ('trynext' 'game2')
  ('trynext' 'game1') 
  ('trynext' 'game2')
  ('trynext' 'game1')
  ('trynext' 'game2')
  ('trynext' 'game1')
  ('trynext' 'game2'))
