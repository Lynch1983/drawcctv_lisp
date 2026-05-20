import os
path = os.path.join(os.environ['TEMP'], 'old_draw.lsp')
data = open(path, 'rb').read()
text = data.decode('gbk', errors='replace')
lines = text.split('\n')

# Extract the base64 dcl data
print("=== DCL_BASE64_DATA ===")
for i in range(2497, 2560):
    print(lines[i])
print("=== END_DCL_DATA ===")

# Extract key function index  
# First, let's find all defun at top level
print("\n=== ALL_FUNCTIONS ===")
for i in range(2611, min(len(lines), 4400)):
    line = lines[i].strip()
    if line.startswith("(defun ") and ("c:drawCCTV" in line or "drawCCTV" in line or line.startswith("(defun c:drawCCTV")):
        # Get the handler name and the closing brace
        print(f"LINE:{i+1}|{line}")
