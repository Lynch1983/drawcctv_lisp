import os
path = os.path.join(os.environ['TEMP'], 'old_draw.lsp')
data = open(path, 'rb').read()
text = data.decode('gbk', errors='replace')
lines = text.split('\n')

# Extract OpenDCL button handler functions - from line 2612 onwards
# Find all defun c:drawCCTV/Form1/ lines and their content
in_handler = False
handler_name = ""
handler_lines = []
brace_count = 0
output = []

for i in range(2611, len(lines)):
    line = lines[i]
    
    # Check for handler function start
    if line.strip().startswith("(defun c:drawCCTV/Form1/") and not in_handler:
        if handler_lines:
            output.append({"name": handler_name, "lines": handler_lines.copy()})
        handler_name = line.strip()
        handler_lines = [line]
        in_handler = True
        # Count braces in this line
        brace_count = line.count('(') - line.count(')')
        continue
    
    if in_handler:
        handler_lines.append(line)
        brace_count += line.count('(') - line.count(')')
        # Check if function ends (brace_count <= 0 and we're at a new defun)
        if brace_count <= 0:
            # Check if next non-empty line starts a new defun
            j = i + 1
            while j < len(lines) and lines[j].strip() == '':
                j += 1
            if j < len(lines) and (lines[j].strip().startswith("(defun c:drawCCTV/Form1/") or 
                                   lines[j].strip().startswith("(defun ") or
                                   j >= len(lines) - 1):
                output.append({"name": handler_name, "lines": handler_lines.copy()})
                handler_lines = []
                in_handler = False
                brace_count = 0
            # If braces balanced but next line is another defun, close this handler
    
# Don't forget last one
if handler_lines:
    output.append({"name": handler_name, "lines": handler_lines.copy()})

print(f"Found {len(output)} OpenDCL button handlers")
for h in output:
    print(f"\n{'='*80}")
    print(f"HANDLER: {h['name'][:120]}")
    print(f"{'='*80}")
    for l in h['lines']:
        print(l)
