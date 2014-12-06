SRC_DIR = src
GHC = ghc
GHC_OPTS = --make
GHC_FLAGS = -o climbu -XTypeSynonymInstances -XFlexibleInstances
GHC_HIOFILES = -odir tmp/ -hidir tmp/
SOURCE = $(SRC_DIR)/Uc $(SRC_DIR)/Expressions $(SRC_DIR)/Token $(SRC_DIR)/Ast $(SRC_DIR)/Inst $(SRC_DIR)/Parser $(SRC_DIR)/Translator $(SRC_DIR)/ErrorHandler


all:
	$(GHC) $(GHC_FLAGS) $(GHC_OPS) $(SOURCE) $(GHC_HIOFILES)