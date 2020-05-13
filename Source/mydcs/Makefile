CLASSDIR = classes
PACKAGE = mydcs
TARGET = $(PACKAGE).Main
SRCS = $(wildcard *.scala)
CLASS = $(CLASSDIR)/$(PACKAGE)/*.class

#compile
CC = scalac
CFLAGS = -d $(CLASSDIR)
#execute
EXEC = scala
EFLAGS = -cp $(CLASSDIR)
#input from command line
#IN = ../../Database/geography
#OUT = ../../Output/output

ARG = $(IN) $(OUT)

all: compile

run: $(CLASS)
	$(EXEC) $(EFLAGS) $(TARGET) $(ARG)

compile: $(SRCS)
	$(CC) $(CFLAGS) $(SRCS)

clean:
	rm -r $(CLASSDIR)/$(PACKAGE)

.PHONY: clean run
