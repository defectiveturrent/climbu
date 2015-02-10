SRC_DIR = src
GHC = ghc
GHC_OPTS = --make
GHC_FLAGS = -o climbu
GHC_HIOFILES = -odir tmp/ -hidir tmp/
SOURCE = $(SRC_DIR)/*.hs


all:
	$(GHC) $(GHC_FLAGS) $(GHC_OPTS) $(SOURCE) $(GHC_HIOFILES)