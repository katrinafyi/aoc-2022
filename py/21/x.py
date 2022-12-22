with open('21.txt') as f:
    for l in f:
        if '=' not in l:
            print(l)
        else:
            l = l.split()
            if any(x in l for x in '+*-/'):
                print(l[0], '=', 'pure', '('+l[3]+')', l[2], l[4])
            else:
                print(l[0], '=', 'liftA2', l[2])
print('main = print root')
# EXECUTE OUTPUT WITH HASKELL
