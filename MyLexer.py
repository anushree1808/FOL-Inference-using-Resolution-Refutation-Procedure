import ply.lex as lex

tokens = ('CONSTANT',
        'VARIABLE',
        'LPAREN',
        'RPAREN',
        'IMPLIES',
        'AND',
        'OR',
        'NOT',
        'COMMA')

# Regular expression rules for simple tokens
t_IMPLIES = r'=>'
t_AND = r'\&'
t_OR = r'\|'
t_NOT = r'~'
t_CONSTANT = r'[A-Z][A-Za-z]*'
t_VARIABLE = r'[a-z]'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_COMMA = r'\,'

t_ignore = ' \t'

def t_error(t):
  print("Illegal character '%s'" % t.value[0])
  t.lexer.skip(1)

# Build the lexer
def build(**kwargs):
  self.lexer = lex.lex()

# Test it output
def test(data):
  lexer = lex.lex()
  lexer.input(data)
  while True:
	  tok = lexer.token()
	  if not tok: 
	  	break
	  print(tok)

lexer = lex.lex()
#test('((~(An(x) & B(y)))=>C(Jon))')