CC=clang++
CPPFLAGS= `/usr/local/opt/llvm/bin/llvm-config --cxxflags` -I/usr/local/opt/llvm/include -g -O0 -Wall -Wextra -Werror -pedantic -Wno-missing-braces -Wno-nested-anon-types -Wno-unused-function -Wno-unused-parameter -Wno-c++11-compat-deprecated-writable-strings -Wno-writable-strings
LDFLAGS=`/usr/local/opt/llvm/bin/llvm-config --ldflags`
LDLIBS=`/usr/local/opt/llvm/bin/llvm-config --libs core native bitwriter --system-libs`
SRCDIR=src
DESTDIR=src
SOURCES=$(wildcard $(SRCDIR)/*.cpp)
OBJECTS=$(SOURCES:$(SRCDIR)/%.cpp=$(DESTDIR)/%.o)

# TODO: Actually think through dependencies properly.
# I've had a fair few bugs due to poor Makefile dependencies so far.

phosphor: clean $(OBJECTS)
	$(CC) $(LDFLAGS) -o phosphor $(OBJECTS) $(LDLIBS) 

compiler.o: compiler.cpp parser.h lex.cpp lex.h hashmap.hpp helpers.h memoryarena.h memorylist.hpp
	$(CC) $(CPPFLAGS) -c $< -o $@

codegen.o: codegen.cpp codegen.h environment.h hashmap.hpp ast.h helpers.h
	$(CC) $(CPPFLAGS) -c $< -o $@

parser.o: parser.cpp parser.h environment.h hashmap.hpp ast.h helpers.h memoryarena.h memorylist.hpp
	$(CC) $(CPPFLAGS) -c $< -o $@

lex.o: lex.cpp lex.h hashmap.hpp helpers.h memoryarena.h
	$(CC) $(CPPFLAGS) -c $< -o $@

helpers.o: helpers.cpp helpers.h
	$(CC) $(CPPFLAGS) -c $< -o $@

ast.o: ast.cpp ast.h helpers.h
	$(CC) $(CPPFLAGS) -c $< -o $@

memoryarena.o: memoryarena.cpp memoryarena.h memorylist.hpp helpers.h
	$(CC) $(CPPFLAGS) -c $< -o $@

clean:
	$(RM) $(OBJECTS)
