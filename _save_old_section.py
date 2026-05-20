import os
path = os.path.join(os.environ['TEMP'], 'old_draw.lsp')
data = open(path, 'rb').read()
text = data.decode('gbk', errors='replace')
lines = text.split('\n')

# Save the complete relevant section: lines 2493-4430
out = []
out.append(f";; Extracted from old drawCCTV -- OpenDCL section")
out.append(f";; Lines 2494-4430")
out.append("")

for i in range(2493, min(len(lines), 4440)):
    out.append(lines[i])

output_path = os.path.join(os.getcwd(), '_old_opendcl_section.lsp')
with open(output_path, 'w', encoding='utf-8') as f:
    f.write('\n'.join(out))

print(f"Saved {len(out)} lines to {output_path}")
print(f"File size: {os.path.getsize(output_path)} bytes")
