CC=clang++
CPPFLAGS= `/usr/local/opt/llvm/bin/llvm-config --cxxflags` -I/usr/local/opt/llvm/include -g -O0 -Wall -Wextra -Werror -pedantic -Wno-missing-braces -Wno-nested-anon-types -Wno-unused-function -Wno-unused-parameter -Wno-c++11-compat-deprecated-writable-strings -Wno-writable-strings
LDFLAGS=`/usr/local/opt/llvm/bin/llvm-config --ldflags`
LDLIBS=`/usr/local/opt/llvm/bin/llvm-config --libs core native --system-libs`
SOURCES=compiler.cpp lex.cpp parser.cpp codegen.cpp ast.cpp helpers.cpp memoryarena.cpp environment.cpp
OBJECTS=$(SOURCES:.cpp=.o)

# TODO: Actually think through dependencies properly.
# I've had a fair few bugs due to poor Makefile dependencies so far.

phosphor: clean $(OBJECTS)
	$(CC) $(LDFLAGS) -o phosphor $(OBJECTS) $(LDLIBS) 

compiler.o: compiler.cpp parser.h lex.cpp lex.h hashmap.hpp helpers.h memoryarena.h memorylist.hpp
	$(CC) $(CPPFLAGS) -c compiler.cpp

codegen.o: codegen.cpp codegen.h environment.h hashmap.hpp ast.h helpers.h

parser.o: parser.cpp parser.h environment.h hashmap.hpp ast.h helpers.h memoryarena.h memorylist.hpp
	$(CC) $(CPPFLAGS) -c parser.cpp

lex.o: lex.cpp lex.h hashmap.hpp helpers.h memoryarena.h
	$(CC) $(CPPFLAGS) -c lex.cpp

helpers.o: helpers.cpp helpers.h
	$(CC) $(CPPFLAGS) -c helpers.cpp

ast.o: ast.cpp ast.h helpers.h
	$(CC) $(CPPFLAGS) -c ast.cpp

memoryarena.o: memoryarena.cpp memoryarena.h memorylist.hpp helpers.h
	$(CC) $(CPPFLAGS) -c memoryarena.cpp

clean:
	$(RM) $(OBJECTS)
