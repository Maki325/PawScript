./compile.sh

if [ "$1" = "s" ]
then
  # ./build/pawscript int ./tests/pawscript/main.ps
  # ./build/pawscript int ./tests/pawscript/scopes.ps
  # ./build/pawscript int ./tests/pawscript/ifs.ps
  # ./build/pawscript int ./tests/pawscript/bool.ps
  # ./build/pawscript int ./tests/pawscript/if-else.ps
  # ./build/pawscript int ./tests/pawscript/else-if.ps
  # ./build/pawscript int ./tests/pawscript/equals.ps
  ./build/pawscript int ./tests/pawscript/start.ps
elif [ "$1" = "c" ]
then
  # ./build/pawscript com ./tests/pawscript/main.ps -r -s -o ./tests/pawscript/out/main
  # ./build/pawscript com ./tests/pawscript/scopes.ps -r -s -o ./tests/pawscript/out/scopes
  # ./build/pawscript com ./tests/pawscript/ifs.ps -r -s -o ./tests/pawscript/out/ifs
  # ./build/pawscript com ./tests/pawscript/bool.ps -r -s -o ./tests/pawscript/out/bool
  # ./build/pawscript com ./tests/pawscript/if-else.ps -r -s -o ./tests/pawscript/out/if-else
  # ./build/pawscript com ./tests/pawscript/else-if.ps -r -s -o ./tests/pawscript/out/else-if
  # ./build/pawscript com ./tests/pawscript/equals.ps -r -s -o ./tests/pawscript/out/equals
  ./build/pawscript com ./tests/pawscript/start.ps -r -s -o ./tests/pawscript/out/start
else
  ./build/pawscript "$@"
fi
