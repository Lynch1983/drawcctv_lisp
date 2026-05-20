import os
path = os.path.join(os.environ['TEMP'], 'old_draw.lsp')
data = open(path, 'rb').read()
text = data.decode('gbk', errors='replace')
all_lines = text.split('\n')
total = len(all_lines)

# Find all handler functions and their line ranges
# We'll iterate from line 2612 to find all handlers
handlers = {}
i = 2595  # start a bit before to catch any stragglers
in_handler = False
handler_name = None
handler_start = -1
handler_text = []
brace_depth = 0

# Also find drawCCTV and other main functions
main_funcs = {}

while i < min(total, 4400):
    line = all_lines[i]
    stripped = line.strip()
    
    # Check for new function definition
    if stripped.startswith("(defun c:drawCCTV/Form1/") and not in_handler:
        if handler_name and handler_text:
            handlers[handler_name] = {'start': handler_start, 'end': i-1, 'text': '\n'.join(handler_text)}
        handler_name = stripped
        handler_start = i
        handler_text = [line]
        in_handler = True
        brace_depth = line.count('(') - line.count(')')
    elif stripped.startswith("(defun ") and ("drawCCTV" in stripped or "drawkong" in stripped or "drawHJX" in stripped or "draw_singlerec" in stripped or "draw_" in stripped or "split-string" in stripped or "hc-string" in stripped or "clean_creen" in stripped) and not in_handler:
        if handler_name and handler_text:
            handlers[handler_name] = {'start': handler_start, 'end': i-1, 'text': '\n'.join(handler_text)}
        handler_name = stripped
        handler_start = i
        handler_text = [line]
        in_handler = True
        brace_depth = line.count('(') - line.count(')')
    elif in_handler:
        handler_text.append(line)
        brace_depth += line.count('(') - line.count(')')
        if brace_depth <= 0 and (i+1 >= total or all_lines[i+1].strip().startswith("(defun ") or all_lines[i+1].strip().startswith(";;") or all_lines[i+1].strip().startswith("(dcl-") or all_lines[i+1].strip() == ''):
            # Check next non-empty line
            j = i + 1
            while j < total and all_lines[j].strip() == '':
                j += 1
            if j >= total or all_lines[j].strip().startswith("(defun ") or all_lines[j].strip().startswith(";;") or all_lines[j].strip().startswith("(dcl-Control-GetEventInvoke"):
                handlers[handler_name] = {'start': handler_start, 'end': i, 'text': '\n'.join(handler_text)}
                handler_name = None
                handler_text = []
                in_handler = False
                brace_depth = 0
    
    i += 1

# Flush last
if handler_name and handler_text:
    handlers[handler_name] = {'start': handler_start, 'end': i-1, 'text': '\n'.join(handler_text)}

# Also get the drawCCTV main function
print(f"Found {len(handlers)} functions total")
for name, info in sorted(handlers.items(), key=lambda x: x[1]['start']):
    lines_count = info['text'].count('\n') + 1
    print(f"L{info['start']+1}-L{info['end']+1} ({lines_count}L): {name[:80]}")
