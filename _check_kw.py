import os
path = os.path.join(os.environ['TEMP'], 'old_draw.lsp')
data = open(path, 'rb').read()
text = data.decode('gbk', errors='replace')
# Search for OpenDCL related keywords
keywords = ['opendcl', 'dcl_', 'Odcl_', 'dcl-project', 'load_dialog', 'new_dialog', 'start_dialog', 'done_dialog', 'dcl_project_import', 'c:CCTV', 'defun c:', 'defun draw', '主程序', '界面', '对话框', '窗体', '按钮']
lines = text.split('\n')
for i, line in enumerate(lines):
    line_lower = line.lower()
    for kw in keywords:
        if kw.lower() in line_lower:
            print(f"Line {i+1}: {line[:200]}")
            break
