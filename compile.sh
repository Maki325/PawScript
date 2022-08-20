if [ ! -d "./build" ]
then
  mkdir build
fi

if [[ "$*" == *"echo"* ]]
then
  set -x
fi

# DisableWarnings="-Wall -Wextra"
DisableWarnings=""
if [[ "$*" == *"dw"* ]]
then
  DisableWarnings="-w"
fi

DefineMacros="-D PS_DEBUG"
if [[ "$*" == *"release"* ]]
then
  DefineMacros=""
fi

gcc $(find ./src -name '*.c') -ggdb -lm -g $DisableWarnings $DefineMacros -o build/pawscript
