import sys
c = open(sys.argv[1], 'r', encoding='utf-8', errors='ignore').read()
o = c.count('(')
cl = c.count(')')
print(f'Open: {o}  Close: {cl}  Diff: {o-cl}')
sys.exit(0 if o==cl else 1)
