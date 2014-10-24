#!bin/sh
ghc -o climbu -XParallelListComp --make src/Uc src/Expressions src/Parser src/Translator -odir tmp/ -hidir tmp/
