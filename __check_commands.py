import os
os.chdir(r'D:\TX_opitimizer\CCTV_Reloaded')

# Read loader
with open('CCTV_Loader.lsp', 'r', encoding='latin-1') as f:
    print('=== CCTV_Loader.lsp ===')
    print(f.read()[:3000])

print('\n\n')

# Check old file for DRAWCCTV
with open('drawCCTV(界面版)2.lsp', 'r', encoding='latin-1') as f:
    content = f.read()
    import re
    # Find C:DRAWCCTV definition
    m = re.search(r'\(defun\s+C:DRAWCCTV\b.*?\(princ\s*\)\s*\)', content, re.DOTALL)
    if m:
        print('=== C:DRAWCCTV in old file ===')
        print(m.group()[:2000])
    else:
        print('No C:DRAWCCTV found in old file')
        # Search for any C: commands in old file
        cmds = re.findall(r'\(defun\s+(C:[\w-]+)', content)
        print(f'Commands in old file: {cmds[:20]}')
