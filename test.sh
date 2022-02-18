./compile.sh

if [ ! -d ./tests/outputs ]; then
  mkdir ./tests/outputs
fi

if [[ "$*" == *"dw"* ]]
then
  DisableWarnings="-w"
fi

gcc $(find ./src -name '*.c') -lm -g $DisableWarnings -o build/pawscript

# if [ "$1" = "all" ];
# then
#   if [ "$2" = "clean" ];
#   then
#     rm outputs/all.out
#     touch outputs/all.out
#   fi

#   for filename in $(find ./inputs -name '*.in')
#   do
#     echo $'Input:' >> outputs/all.out
#     cat "$filename" >> outputs/all.out
#     echo $'\n\nOutput:' >> outputs/all.out
#     cat "$filename" | ./build/pawscript >> outputs/all.out
#     echo $'' >> outputs/all.out
#     cat "$filename" | time -o outputs/all.out --append ./build/pawscript >> /dev/null
#     echo $'-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-' >> outputs/all.out
#   done
# else
#   if [ ! -f ./inputs/$1.in ]; then
#     echo $"Input file \"inputs/$1.in\" doesn't exist!"
#     exit 0
#   fi

#   cat inputs/$1.in | time -o outputs/$1.out --append ./build/pawscript >> outputs/$1.out
# fi
