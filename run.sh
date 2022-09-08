./compile.sh

if [ "$1" = "s" ]
then
  # ./build/pawscript int ./tests/pawscript/start.ps
  ./build/pawscript int ./tests/pawscript/main.ps
elif [ "$1" = "c" ]
then
  # ./build/pawscript com ./tests/pawscript/start.ps -r -s -o ./tests/pawscript/out/start
  # ./build/pawscript com ./tests/pawscript/main.ps -r -s -o ./tests/pawscript/out/main
  # ./build/pawscript com ./tests/pawscript/001-simple-variables.ps -r -s -o ./tests/pawscript/out/001-simple-variables
  ./build/pawscript com ./tests/pawscript/002-arithmetics.ps -r -s -o ./tests/pawscript/out/002-arithmetics
else
  ./build/pawscript "$@"
fi
