CC=clang++
CPPFLAGS= `/usr/local/opt/llvm/bin/llvm-config --cxxflags` -I/usr/local/opt/llvm/include -g -O0 -Wall -Wextra -Werror -pedantic -Wno-missing-braces -Wno-nested-anon-types -Wno-unused-function -Wno-unused-parameter -Wno-c++11-compat-deprecated-writable-strings -Wno-writable-strings
LDFLAGS=`/usr/local/opt/llvm/bin/llvm-config --ldflags`
LDLIBS=`/usr/local/opt/llvm/bin/llvm-config --libs core native bitwriter --system-libs`
SRCDIR=src
DESTDIR=src
SOURCES=$(wildcard $(SRCDIR)/*.cpp)
OBJECTS=$(SOURCES:$(SRCDIR)/%.cpp=$(DESTDIR)/%.o)

# NOTE: These dependencies are probably wrong, so if in doubt about some weird error, try running 'clean' before building.

phosphor: $(OBJECTS)
	$(CC) $(LDFLAGS) -o phosphor $(OBJECTS) $(LDLIBS) 

compiler.o: compiler.cpp compiler.h helpers.h memoryarena.h memorylist.hpp hashmap.hpp lex.h parser.h
	$(CC) $(CPPFLAGS) -c $< -o $@

codegen.o: codegen.cpp codegen.h helpers.h memoryarena.h memorylist.hpp hashmap.hpp environment.h ast.h
	$(CC) $(CPPFLAGS) -c $< -o $@

parser.o: parser.cpp parser.h helpers.h memoryarena.h memorylist.hpp hashmap.hpp lex.h ast.h environment.h
	$(CC) $(CPPFLAGS) -c $< -o $@

lex.o: lex.cpp lex.h helpers.h memoryarena.h hashmap.hpp
	$(CC) $(CPPFLAGS) -c $< -o $@

helpers.o: helpers.cpp helpers.h
	$(CC) $(CPPFLAGS) -c $< -o $@

ast.o: ast.cpp ast.h helpers.h memorylist.hpp environment.h
	$(CC) $(CPPFLAGS) -c $< -o $@

memoryarena.o: memoryarena.cpp memoryarena.h helpers.h memorylist.hpp
	$(CC) $(CPPFLAGS) -c $< -o $@

clean:
	$(RM) $(OBJECTS)
