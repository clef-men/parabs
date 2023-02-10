.PHONY : all
all : build

.PHONY : build
build :
	@ dune build

.PHONY : test
test :
	@ dune runtest

.PHONY : clean
clean :
	@ dune clean
