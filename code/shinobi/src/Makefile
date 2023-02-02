CC = erlc
FLAGS = -Wall

all: compile_all test

compile_all:
	$(CC) $(FLAGS) shinobi.erl &&\
	$(CC) $(FLAGS) test_shinobi.erl &&\
	$(CC) $(FLAGS) operation.erl

test:
	erl -noshell -s test_shinobi test_all -s init stop

.PHONY: clean

clean:
	rm -rf *.beam *.dump 

