#!bin/bash
if [ $1 == "linux" ]
	then
	ghc -o climbu -XParallelListComp --make src/Uc src/Expressions src/Token src/Ast src/Inst src/Parser src/Translator -odir tmp/ -hidir tmp/
fi
if [ $1 == "windows" ]
	then
	wine /home/thelost/.wine/drive_c/Program\ Files/Haskell\ Platform/2014.2.0.0/bin/ghc -o climbu -XParallelListComp --make src/Uc src/Expressions src/Token src/Ast src/Inst src/Parser src/Translator -odir tmp/ -hidir tmp/
fi
