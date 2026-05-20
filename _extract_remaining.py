import os
path = os.path.join(os.environ['TEMP'], 'old_draw.lsp')
data = open(path, 'rb').read()
text = data.decode('gbk', errors='replace')
lines = text.split('\n')

# Read handlers from line 3494 onwards
in_func = False
func_name = ""
func_lines = []
output = []

for i in range(3494, min(len(lines), 4430)):
    line = lines[i]
    stripped = line.strip()
    
    # Start of a function
    if stripped.startswith("(defun ") and not in_func:
        if func_name and func_lines:
            output.append({'name': func_name, 'lines': func_lines.copy()})
        func_name = stripped[:80]
        func_lines = [line]
        in_func = True
        depth = line.count('(') - line.count(')')
        continue
    
    if in_func:
        func_lines.append(line)
        depth += line.count('(') - line.count(')')
        if depth <= 0:
            # Check next non-empty line
            j = i + 1
            while j < len(lines) and lines[j].strip() == '':
                j += 1
            if j >= len(lines) or lines[j].strip().startswith("(defun ") or lines[j].strip().startswith("(dcl-"):
                output.append({'name': func_name, 'lines': func_lines.copy()})
                func_name = ""
                func_lines = []
                in_func = False

# Flush
if func_name and func_lines:
    output.append({'name': func_name, 'lines': func_lines.copy()})

print(f"Found {len(output)} post-drawCCTV functions")
for h in output:
    print(f"\n{'='*80}")
    print(f"HANDLER: {h['name'][:100]}")
    print(f"{'='*80}")
    for l in h['lines']:
        print(l)
