import os
path = os.path.join(os.environ['TEMP'], 'old_draw.lsp')
data = open(path, 'rb').read()
text = data.decode('gbk', errors='replace')
# Lines 2480-2610 - OpenDCL form loading/definition
lines = text.split('\n')
for i in range(2479, 2611):
    print(f"L{i+1}: {lines[i]}")
