import os
path = os.path.join(os.environ['TEMP'], 'old_draw.lsp')
data = open(path, 'rb').read()
text = data.decode('gbk', errors='replace')
lines = text.split('\n')

funcs = ['sub1_mlpoint', 'sub1_block_name', 'block_base', 'block_dist', 
         'sxj_dist', 'break_line', 'breakobj', 'join_line', 'sub10_dxd', 
         'sub10_draw', 'sx', 'gbtc', 'dktc', 'Berni_Start', 'Berni_end', 
         'read-parameter', 'drawkong', 'clean_creen', 'drawHJX']
for f in funcs:
    found = False
    for i, l in enumerate(lines):
        if f in l and ('(defun' in l):
            print(f'{f}: Line {i+1}: {l[:150]}')
            found = True
            break
    if not found:
        print(f'{f}: NOT FOUND')
