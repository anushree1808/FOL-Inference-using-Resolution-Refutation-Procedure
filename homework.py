import MyParser
from collections import defaultdict
from sets import Set
import copy
from collections import deque

class Homework:
    def __init__(self):
        self.content = []
        self.num_query = 0
        self.num_sent = 0
        self.queries = []
        self.sent = []
        self.cnf_sent = defaultdict(list)
        self.predicates = defaultdict(list)
        self.pos_predicates = defaultdict(list)
        self.neg_predicates = defaultdict(list)
        self.KB = deque()
        self.q_sent = defaultdict(list)
        self.q = defaultdict(list)
        self.output = []
        self.KB_len=0
        self.univ = 0
        
    def readinput(self,filename):
        fo = open(filename,'rU')
        with fo as f:
            self.content = [line.strip() for line in f.readlines()]
        self.num_query = int(self.content[0])
        self.queries = self.content[1:self.num_query+1]
        i = 1+self.num_query
        self.num_sent = int(self.content[i])
        i+=1
        self.sent = self.content[i:i+self.num_sent]

    def disjuncts(self,s,j):
        if s[0] == 'Predicate':
            #self.predicates[s[1]].append((0,j,s[2]))
            self.pos_predicates[s[1]].append(j)
            self.cnf_sent[j].append((0,s[1],s[2]))
        elif s[0]== 'NOT':
            #self.predicates[s[1][1]].append((1,j,s[1][2]))
            self.neg_predicates[s[1][1]].append(j)
            self.cnf_sent[j].append((1,s[1][1],s[1][2]))
        elif s[0] == 'OR':
            self.disjuncts(s[1],j)
            self.disjuncts(s[2],j)

    def ands(self,s,j):
        if s[0] == 'Predicate':
            #self.predicates[s[1]].append((0,j,s[2]))
            self.pos_predicates[s[1]].append(j)
            t = s[2]
            for i in range(len(t)):
                if t[i][0]=='Variable':
                    t[i]=('Variable',t[i][1]+str(self.univ))
                    self.univ +=1
            self.cnf_sent[j].append((0,s[1],t))
            j += 1
            return j
        elif s[0]== 'NOT':
            #self.predicates[s[1][1]].append((1,j,s[1][2]))
            self.neg_predicates[s[1][1]].append(j)
            t = s[1][2]
            for i in range(len(t)):
                if t[i][0]=='Variable':
                    t[i]=('Variable',t[i][1]+str(self.univ))
                    self.univ +=1
            self.cnf_sent[j].append((1,s[1][1],t))
            j+=1
            return j
        elif s[0] == 'AND':
            k = self.ands(s[1],j)
            m = self.ands(s[2],k)
            return m
        elif s[0] == 'OR':
            self.disjuncts(s[1],j)
            self.disjuncts(s[2],j)
            j += 1
            return j
        else:
            self.conjuncts(s,j)
            return j+len(s)

    def conjuncts(self,sents,j):
        for s in sents:
            if s[0] == 'Predicate':
                #self.predicates[s[1]].append((0,j,s[2]))
                self.pos_predicates[s[1]].append(j)
                t = s[2]
                for i in range(len(t)):
                    if t[i][0]=='Variable':
                        t[i]=('Variable',t[i][1]+str(self.univ))
                        self.univ +=1
                    self.cnf_sent[j].append((0,s[1],t))
            elif s[0]== 'NOT':
                #self.predicates[s[1][1]].append((1,j,s[1][2]))
                self.neg_predicates[s[1][1]].append(j)
                t = s[1][2]
                for i in range(len(t)):
                    if t[i][0]=='Variable':
                        t[i]=('Variable',t[i][1]+str(self.univ))
                        self.univ +=1
                self.cnf_sent[j].append((1,s[1][1],t))
            elif s[0] == 'OR':
                self.disjuncts(s[1],j)
                self.disjuncts(s[2],j)
            j += 1

    def parse(self):
        j = 0
        for i in range(self.num_sent):
            #print s
            #print MyParser.parse_sent(self.sent[i])
            #print "\n"
            s = MyParser.parse_sent(self.sent[i])
            print s
            if s[0] == 'Predicate':
                #self.predicates[s[1]].append((0,j,s[2]))
                self.pos_predicates[s[1]].append(j)
                self.cnf_sent[j].append((0,s[1],s[2]))
                j += 1
            elif s[0]== 'NOT':
                #self.predicates[s[1][1]].append((1,j,s[1][2]))
                self.neg_predicates[s[1][1]].append(j)
                self.cnf_sent[j].append((1,s[1][1],s[1][2]))
                j += 1
            elif s[0]=='AND':
                '''for t in s[1:]:
                    if t[0] == 'Predicate':
                        self.predicates[t[1]].append((0,j,t[2]))
                        self.cnf_sent[j].append((0,t[1]))
                    elif t[0]== 'NOT':
                        self.predicates[t[1][1]].append((1,j,t[1][2]))
                        self.cnf_sent[j].append((1,t[1][1]))
                    j += 1
                '''
                j = self.ands(s[1],j)
                j = self.ands(s[2],j)
            elif s[0] == 'OR':
                self.disjuncts(s[1],j)
                self.disjuncts(s[2],j)
                j += 1
            else :
                self.conjuncts(s,j)
                #print j
                j += len(s)
        '''print self.pos_predicates
        print "\n"
        print self.neg_predicates
        print "\n"
        print self.cnf_sent
        '''
        for i in self.cnf_sent.keys():
            self.KB.append(self.cnf_sent[i])
        #print self.KB
        self.KB_len = j
        for i in range(self.num_query):
            temp = self.queries[i].split('~')
            #print temp
            if len(temp)==1:
                q = MyParser.parse_sent(temp[0])
                self.q[i].append((0,q[1],q[2]))
            else :
                q = MyParser.parse_sent(temp[1])
                self.q[i].append((1,q[1],q[2]))
        #print "\n"
        #print self.q

    def verify(self):
        self.pos_copy = copy.deepcopy(self.pos_predicates)
        self.neg_copy = copy.deepcopy(self.neg_predicates)
        self.cnf_copy = copy.deepcopy(self.cnf_sent)
        for i in self.q:
            query = self.q[i][0]
            query = (1-query[0],query[1],query[2])
            self.cnf_copy[self.KB_len] = [query]
            if query[0]==0:
                self.pos_copy[query[1]].append(self.KB_len)
            else:
                self.neg_copy[query[1]].append(self.KB_len)
            #print query
            '''print self.cnf_copy
            print self.pos_copy
            print self.neg_copy'''
            k = self.resolution([query])
            self.output.append(k)
            self.pos_copy = copy.deepcopy(self.pos_predicates)
            self.neg_copy = copy.deepcopy(self.neg_predicates)
            self.cnf_copy = copy.deepcopy(self.cnf_sent)
        print self.output
        self.write_output()

    def sentences(self,query):
        if query[0]== 0:
            sent = self.pos_copy[query[0]]
        else:
            sent = self.neg_copy[query[0]]
        yield sent
        
    def resolution(self,query):
        p_copy = copy.deepcopy(self.pos_copy)
        n_copy = copy.deepcopy(self.neg_copy)
        flag = True
        for q in query:
            '''print "query"
            print q
            print p_copy
            print n_copy'''
            if q[0]== 1:
                sent_num = p_copy[q[1]]
            else:
                sent_num = n_copy[q[1]]
                flag = False
            #print sent_num
            for j in sent_num:
                #print "here"
                #print q
                c_copy = copy.deepcopy(self.cnf_copy)
                #print j
                clause = self.cnf_copy[j]
                flag = True
                #print "here"
                #print q
                sent = self.resolve(clause,q)
                if sent==True:
                    #print "Fail"
                    continue
                elif len(sent)==0 :
                    return 'TRUE'
                else :
                    self.cnf_copy[j] = sent[2]
                    query_copy = copy.deepcopy(query)
                    query_copy.remove(q)
                    subst = sent[1]
                    sent = sent[0]
                    for c in query_copy:
                        args = c[2]
                        for i in range(len(args)):
                            args[i]=subst.get(args[i][1],args[i])
                    
                    '''print "substituted clause\t"
                    print sent+query_copy'''
                    if len(sent+query_copy)==0:
                        return 'TRUE'
                    k = self.resolution(sent+query_copy)
                    if k=='TRUE':
                        return 'TRUE'
                self.cnf_copy = copy.deepcopy(c_copy)
                
        return 'FALSE'
            
    def resolve(self,clause,q):
        '''print clause
        print q'''
        subst = {}
        for c in clause:
            '''print "hi"
            print c[1] , q[1]
            print q[0]
            print c[0] , 1-q[0]'''
            if c[1]==q[1] and c[0]==1-q[0]:

                subst = self.unify(c[2],q[2])
                if len(subst.keys())>0:
                    break
        if len(subst.keys())== 0:
            return True
        clause.remove(c)
        for c in clause:
            args = c[2]
            for i in range(len(args)):
                args[i]=subst.get(args[i][1],args[i])
        cl = copy.deepcopy(clause)
        cl.append(c)
        #print "substituted clause\t"
        #print clause
        return [clause,subst,cl]

    def unify(self, arg1, arg2):
        #print arg1, arg2
        subst = dict()
        for i in range(len(arg1)):
            if arg1[i][0]=='Variable' and arg2[i][0]=='Variable':
                subst[arg1[i][1]]= ('Variable',arg1[i][1])
                subst[arg2[i][1]]= ('Variable',arg1[i][1])
            elif arg1[i][0]=='Constant' and arg2[i][0]=='Variable':
                subst[arg2[i][1]]= ('Constant',arg1[i][1])
            elif arg1[i][0]=='Variable' and arg2[i][0]=='Constant':
                subst[arg1[i][1]]= ('Constant',arg2[i][1])
            elif arg1[i][1]== arg2[i][1] :#and arg1[i][0]=='Constant' and arg2[i][0]=='Constant':
                subst[arg1[i][1]]= ('Constant',arg2[i][1])
            elif arg1[i][1]!= arg2[i][1]:
                #print "\n"
                return {}
        #print subst
        return subst

    def write_output(self):
        fname = "output.txt"
        fo = open(fname, 'w')
        for i in range(len(self.output)):
            self.output[i]+="\n"
        fo.writelines(self.output)
        fo.close()
        
obj = Homework()
obj.readinput("input9.txt")
obj.parse()
obj.verify()
