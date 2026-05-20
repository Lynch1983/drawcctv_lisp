import os
path = os.path.join(os.environ['TEMP'], 'old_draw.lsp')
data = open(path, 'rb').read()
print('Length:', len(data))
print('Hex:', ' '.join(f'{b:02X}' for b in data[:64]))
print()
# Try to decode as UTF-8 with error handling
print('First 200 chars (UTF-8 errors replaced):')
print(data[:200].decode('utf-8', errors='replace'))
