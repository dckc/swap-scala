# This is just for emacs flymake-mode
# anybody wanna clue me in on how to get fsc to compile just what's changed?

SCALA_HOME=$(HOME)/src/scala
FSC=$(SCALA_HOME)/bin/fsc

SERVLET=../../../lib_managed/scala_2.8.0.Beta1-RC5/provided/servlet-api-2.5.jar

.PHONY: check-syntax

check-syntax:
	$(FSC) -unchecked -dependencyfile ,dep-test -d /tmp \
		-classpath $(SERVLET) *.scala
