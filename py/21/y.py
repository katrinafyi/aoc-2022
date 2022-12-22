v = set()
with open('21.txt') as f:
    for l in f:
        if '=' not in l:
            print(l)
        else:
            l = l.split()
            if l[0] == 'root':
                print('(assert (=', l[2], l[4], '))')
            elif l[0] == 'humn':
                v |= {'humn'}
            elif any(x in l for x in '+*-/'):
                a,b = l[2], l[4]
                op  = l[3]
                v |= {l[0], a,b}
                print('(assert', '(=', l[0], '(' + op, a, b, ')))')
            else:
                print('(assert', '(=', l[0], l[2], '))')
for x in v:
    print(' (declare-fun', x, '()', 'Int)')

print('(check-sat)\n(get-model)')
# SORT OUTPUT THEN GIVE TO Z3
