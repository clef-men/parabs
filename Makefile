.PHONY : all
all : build

.PHONY : build
build :
	@ dune build

.PHONY : test
test :
	@ dune runtest

.PHONY : top
top :
	@ dune utop . -- -init top.ml

.PHONY : clean
clean :
	@ dune clean
