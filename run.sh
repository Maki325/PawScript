./compile.sh

if [ "$1" = "s" ]
then
  # ./build/pawscript int ./tests/pawscript/start.ps
  ./build/pawscript int ./tests/pawscript/main.ps
elif [ "$1" = "c" ]
then
  # ./build/pawscript com ./tests/pawscript/start.ps -r -s -o ./tests/pawscript/out/start
  ./build/pawscript com ./tests/pawscript/main.ps -r -s -o ./tests/pawscript/out/main
else
  ./build/pawscript "$@"
fi
