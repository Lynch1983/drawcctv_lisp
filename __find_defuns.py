import re, glob

# Find all defuns in M12_main.lsp
with open('M12_main.lsp', 'r', encoding='latin-1') as f:
    c = f.read()
funcs = re.findall(r'\(defun[ ]+([\w:-]+)', c)
print("=== M12_main.lsp functions ===")
for f in funcs:
    print(f)

# Also check old file
with open('drawCCTV(界面版)2.lsp', 'r', encoding='latin-1') as f:
    c = f.read()
funcs_old = re.findall(r'\(defun[ ]+([\w:-]+)', c)
print(f"\n=== drawCCTV(界面版)2.lsp functions ({len(funcs_old)}) ===" )
for f in funcs_old:
    print(f)
