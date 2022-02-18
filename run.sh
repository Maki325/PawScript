./compile.sh 2> /dev/null

if [ "$1" = "s" ]
then
  ./build/pawscript int ./tests/pawscript/main.ps
elif [ "$1" = "c" ]
then
  ./build/pawscript com ./tests/pawscript/main.ps -r -o ./tests/pawscript/out/main
else
  ./build/pawscript "$@"
fi
