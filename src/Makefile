all: tpzc TopazParser.cpp TopazParser.hpp TopazParser TopazTokens.cpp TopazCLib.cpp

OBJS = TopazParser.o  \
       TopazLLVM.o \
       Topaz.o    \
       TopazTokens.o  \
       TopazBuiltin.o  \
	   TopazCLib.o  \

LLVMCONFIG = /opt/homebrew/opt/llvm/bin/llvm-config
CPPFLAGS = `$(LLVMCONFIG) --cppflags` -std=c++14
LDFLAGS = `$(LLVMCONFIG) --ldflags` -lpthread -ldl -lz -lncurses -rdynamic
LIBS = `$(LLVMCONFIG) --libs`

clean:
	$(RM) -rf TopazParser.cpp TopazParser.hpp TopazParser TopazTokens.cpp $(OBJS)

TopazParser.cpp: TopazParser.y
	bison -d -o $@ $^
	
TopazParser.hpp: TopazParser.cpp

TopazTokens.cpp: TopazTokens.l TopazParser.hpp
	flex -o $@ $^

%.o: %.cpp
	clang++ -c $(CPPFLAGS) -o $@ $<

TopazParser: $(OBJS)
	clang++ -ggdb -o ../tpz2obj $(OBJS) $(LIBS) $(LDFLAGS)

tpzc:
	clang++ -o ../tpzc TopazCompiler.cpp

