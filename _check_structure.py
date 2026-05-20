import os
path = os.path.join(os.environ['TEMP'], 'old_draw.lsp')
data = open(path, 'rb').read()
text = data.decode('gbk', errors='replace')
lines = text.split('\n')

# Check around line 3295 - start of drawCCTV
print('=== Lines 3289-3310 ===')
for i in range(3289, 3310):
    if i < len(lines):
        print(f'{i+1}: {lines[i][:150]}')

# Check around line 3494
print('\n=== Lines 3489-3510 ===')
for i in range(3489, 3510):
    if i < len(lines):
        print(f'{i+1}: {lines[i][:150]}')

# Check what is between 3320 and 3490
print('\n=== Lines 3330-3490 (samples every 10 lines) ===')
for i in range(3330, 3490, 10):
    if i < len(lines):
        print(f'{i+1}: {lines[i][:100]}')
