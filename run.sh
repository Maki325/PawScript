./compile.sh

if [ "$1" = "s" ]
then
  # ./build/pawscript int ./tests/pawscript/main.ps
  # ./build/pawscript int ./tests/pawscript/scopes.ps
  # ./build/pawscript int ./tests/pawscript/ifs.ps
  # ./build/pawscript int ./tests/pawscript/bool.ps
  ./build/pawscript int ./tests/pawscript/if-else.ps
elif [ "$1" = "c" ]
then
  # ./build/pawscript com ./tests/pawscript/main.ps -r -s -o ./tests/pawscript/out/main
  # ./build/pawscript com ./tests/pawscript/scopes.ps -r -s -o ./tests/pawscript/out/scopes
  # ./build/pawscript com ./tests/pawscript/ifs.ps -r -s -o ./tests/pawscript/out/ifs
  # ./build/pawscript com ./tests/pawscript/bool.ps -r -s -o ./tests/pawscript/out/bool
  ./build/pawscript com ./tests/pawscript/if-else.ps -r -s -o ./tests/pawscript/out/if-else
else
  ./build/pawscript "$@"
fi
