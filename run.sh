./compile.sh

if [ "$1" = "r" ]
then
  nasm -g -felf64 ./tests/pawscript/out/005-add-function.asm
  ld -o ./tests/pawscript/out/005-add-function ./tests/pawscript/out/005-add-function.o
  ././tests/pawscript/out/005-add-function
elif [ "$1" = "s" ]
then
  # ./build/pawscript int ./tests/pawscript/start.ps
  ./build/pawscript int ./tests/pawscript/main.ps
elif [ "$1" = "c" ]
then
  # ./build/pawscript com ./tests/pawscript/start.ps -r -s -o ./tests/pawscript/out/start
  # ./build/pawscript com ./tests/pawscript/main.ps -r -s -o ./tests/pawscript/out/main
  # ./build/pawscript com ./tests/pawscript/001-simple-variables.ps -r -s -o ./tests/pawscript/out/001-simple-variables
  # ./build/pawscript com ./tests/pawscript/002-arithmetics.ps -r -s -o ./tests/pawscript/out/002-arithmetics
  # ./build/pawscript com ./tests/pawscript/003-functions.ps -r -s -o ./tests/pawscript/out/003-functions
  # ./build/pawscript com ./tests/pawscript/004-function-variables.ps -r -s -o ./tests/pawscript/out/004-function-variables
  ./build/pawscript com ./tests/pawscript/005-add-function.ps -r -s -o ./tests/pawscript/out/005-add-function
else
  ./build/pawscript "$@"
fi
