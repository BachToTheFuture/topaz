
.PHONY: src

all: src

src:
	make -C src all

ir: examples/test.cpp
	clang++ -S -emit-llvm $^

clean:
	make -C src clean