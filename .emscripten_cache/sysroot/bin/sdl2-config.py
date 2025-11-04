#!/nix/store/fqm9bqqlmaqqr02qbalm1bazp810qfiw-python3-3.12.9/bin/python3

import sys

print('emscripten sdl2-config called with', ' '.join(sys.argv), file=sys.stderr)

args = sys.argv[1:]

if '--cflags' in args or '--libs' in args:
  print('-sUSE_SDL=2')
elif '--version' in args:
  print('2.0.10')

