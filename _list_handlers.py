import os
path = os.path.join(os.environ['TEMP'], 'old_draw.lsp')
data = open(path, 'rb').read()
text = data.decode('gbk', errors='replace')
lines = text.split('\n')

# Find ALL defun c:drawCCTV/Form1/ lines, list them
for i in range(2611, len(lines)):
    line = lines[i].strip()
    if line.startswith("(defun c:drawCCTV/Form1/"):
        print(f"L{i+1}: {line[:120]}")
