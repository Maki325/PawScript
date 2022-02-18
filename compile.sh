if [ ! -d "./build" ]
then
  mkdir build
fi

if [[ "$*" == *"echo"* ]]
then
  set -x
fi

if [[ "$*" == *"dw"* ]]
then
  DisableWarnings="-w"
fi

gcc $(find ./src -name '*.c') -lm -g $DisableWarnings -o build/pawscript
