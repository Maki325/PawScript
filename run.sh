# ./compile.sh release 2> /dev/null
./compile.sh

if [ "$1" = "s" ]
then
  # ./build/pawscript int ./tests/pawscript/main.ps
  ./build/pawscript int ./tests/pawscript/scopes.ps
elif [ "$1" = "c" ]
then
  # ./build/pawscript com ./tests/pawscript/main.ps -r -o ./tests/pawscript/out/main
  ./build/pawscript com ./tests/pawscript/scopes.ps -r -o ./tests/pawscript/out/scopes
else
  ./build/pawscript "$@"
fi
