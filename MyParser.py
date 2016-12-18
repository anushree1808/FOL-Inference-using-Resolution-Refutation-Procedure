import ply.yacc as yacc
import MyLexer

tokens = MyLexer.tokens
count = -1
#print tokens
def not_func(lis):
	if lis[0] =='NOT':
		return lis[1]
	elif lis[0] == 'AND':
		p1 = not_func(lis[1])
		p2 = not_func(lis[2])
		return ['OR',p1,p2]
	elif lis[0] == 'OR':
		p1 = not_func(lis[1])
		p2 = not_func(lis[2])
		return ['AND',p1,p2]
	else :
		return ['NOT',lis]

def distributer(e1,e2):
	#print e1 , e2
	if e1[0]=='Predicate' or e1[0] == 'NOT':
		return [['OR', e1, e2]]
	elif e1[0] == 'OR':
		return [['OR',e1,e2]]
	else :
		return distributer(e1[1],e2) + distributer(e1[2],e2)

def distribute(e1,e2):
	#print e1 
	#print e2
	if (e1[0]=='Predicate' or e1[0] == 'NOT' or e1[0] == 'OR') and (e2[0]=='Predicate' or e2[0] == 'NOT' or e2[0] == 'OR') :
		return [['OR', e1, e2]]
	elif e1[0]=='Predicate' or e1[0] == 'NOT' or e1[0] == 'OR':
		return distribute(e1,e2[1]) + distribute(e1,e2[2])
	elif e2[0]=='Predicate' or e2[0] == 'NOT' or e2[0] == 'OR':
		return distribute(e1[1],e2) + distribute(e1[2],e2)
	else :
		return distribute(e1[1],e2[1]) + distribute(e1[2],e2[1])+distribute(e1[1],e2[2]) + distribute(e1[2],e2[2])

def p_parensentence(p):
	'''
	sentence : LPAREN sent RPAREN 
	'''
	p[0] = p[2]

def p_assentence(p):
	'''
	sentence : atomic_sentence
	'''
	p[0] = p[1]

def p_notsent(p):
	'''
	sent : NOT sentence
	'''
	#print len(p[2])
	#print p[2]
	if p[2][0] == 'NOT':
		#print p[2]
		p[0] = p[2][1]
	elif p[2][0] == 'OR' :
		#print p[2]
		p1 = not_func(p[2][1])
		p2 = not_func(p[2][2])
		p[0] = ['AND',p1,p2]
	elif p[2][0] == 'AND':
		p1 = not_func(p[2][1])
		p2 = not_func(p[2][2])
		p[0] = ['OR',p1,p2]
	else:
		p[0] = ['NOT',p[2]]

def p_connectivesentence(p):
	'''
	sent : sentence connective sentence
	'''
	if p[2] == "IMPLIES":
		#print p[2]
		#p[1] = ['NOT', p[1]]
		p[1] = not_func(p[1])
		p[2] = 'OR'
		if p[1][0] =='AND':
			p[0] = distributer(p[1],p[3])
		else:
			p[0] = [p[2],p[1],p[3]]
	elif p[2] == 'AND':
		p[0] = [p[2],p[1],p[3]]
	else :
		#print p[1]
		#print p[3]
		if p[1][0] == 'AND' and p[3][0] == 'AND':
			p[0] = distribute(p[1],p[3])
		elif p[1][0] == 'AND':
			#p[0] = [['OR',p[1][1],p[3]],['OR',p[1][2],p[3]]]
			print "hai"
			p[0] = distributer(p[1],p[3])
		elif p[3][0] == 'AND':
			#p[0] = [['OR',p[1],p[3][1]],['OR',p[1],p[3][2]]]
			print "bai"
			p[0] = distributer(p[3],p[1])
		else:
			p[0] = [p[2],p[1],p[3]]

def p_atomicsentence(p):
	'''
	atomic_sentence : CONSTANT LPAREN term_list RPAREN
	'''
	p[0] = ['Predicate', p[1], p[3]]

def p_sintermlist(p):
	'''
	term_list : term
	'''
	p[0] = [p[1]]

def p_multitermlist(p):
	'''
	term_list : term COMMA term_list
	'''
	p[0] = [p[1]] + p[3]

def p_constterm(p):
	'''
	term : CONSTANT
	'''
	p[0] = ('Constant',p[1])

def p_varterm(p):
	'''
	term : VARIABLE
	'''
	p[0] = ('Variable',p[1]+str(count))

def p_andconnective(p):
	'''
	connective : AND
	'''
	p[0] = 'AND'

def p_orconnective(p):
	'''
	connective : OR
	'''
	p[0] = 'OR'

def p_impliesconnective(p):
	'''
	connective : IMPLIES
	'''
	p[0] = 'IMPLIES'

def p_error(p):
    print("Syntax error in input!")

parser = yacc.yacc()

def parse_sent(s):
	#parser = yacc.yacc()
	#print parser.parse(s)
	global count
	count += 1
	return parser.parse(s)

#print parser.parse('(~(~An(x,y)))')
#print parser.parse('(B(x) | ((A(x) | D(x)) & C(x)))')
#print parser.parse('(~((~An(x)) | (~B(x))))')
#print parser.parse('(A(x)|B(x))')
#print parser.parse('((~(~(~A(x))))=>(~(~B(x))))')
#print parser.parse('((A(x) & B(x))|C(x))')
#print parser.parse('(C(x)|(A(x)&(~B(Anu))))')
#print parser.parse('(((A(x)&B(x))&C(x))|D(x))')
#print parser.parse('(E(x)|((A(x)&B(x))&(C(x)&D(x))))')
#print parser.parse('(((A(x)|E(x))&(B(x) & G(x)))|(C(x) & (D(x)&F(x))))')
#print parser.parse('((A(x)=>B(x))=>C(x))')
#print parser.parse('(((A(x)=>B(x))=>C(x))&D(x))')