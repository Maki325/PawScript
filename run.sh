./compile.sh

if [ "$1" = "r" ]
then
  nasm -g -felf64 ./tests/pawscript/out/009-chars.test.asm
  ld -o ./tests/pawscript/out/009-chars ./tests/pawscript/out/009-chars.test.o
  ././tests/pawscript/out/009-chars
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
  # ./build/pawscript com ./tests/pawscript/005-add-function.ps -r -s -o ./tests/pawscript/out/005-add-function
  # ./build/pawscript com ./tests/pawscript/006-scopes.ps -r -s -o ./tests/pawscript/out/006-scopes
  # ./build/pawscript com ./tests/pawscript/007-if.ps -r -s -o ./tests/pawscript/out/007-if
  # ./build/pawscript com ./tests/pawscript/008-recursion-number-print.ps -r -s -o ./tests/pawscript/out/008-recursion-number-print
  # ./build/pawscript com ./tests/pawscript/009-chars.ps -r -s -o ./tests/pawscript/out/009-chars
  ./build/pawscript com ./tests/pawscript/010-arrays.ps -r -s -o ./tests/pawscript/out/010-arrays
else
  ./build/pawscript "$@"
fi
