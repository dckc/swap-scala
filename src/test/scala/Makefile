RST2HTML=rst2html

all: examples.html examples.scala

examples.html: examples.rst
	$(RST2HTML) $< >$@.out
	mv $@.out $@

examples.scala: examples.rst
	sh ../../../doctester org.w3.swap.test $< >$@.out
	mv $@.out $@
