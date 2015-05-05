all:
	raco exe pylex.rkt
clean:
	`which rm` -rf compiled/
	`which rm` pylex

test:
	raco exe pylex.rkt
	./pylex test.py
