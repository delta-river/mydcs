CC = g++
CFLAGS = -Wall -std=c++14
TARGET = mydcs
SRCS = $(wildcard *.scala)
OBJS = $(SRCS:%.cpp=%.o)
DEPS = $(SRCS:%.cpp=%.d)

all: $(TARGET)

-include $(DEPS)

$(TARGET): $(OBJS)
	$(CC) -g -o $@ $^

%.o: %.cpp
	$(CC) $(CFLAGS) -c $< 

#password for mysql
database:
	@echo "mysql password:"; stty -echo; read PAS;stty echo;cd Database; make PAS="$$PAS"

clean:
	rm -f $(TARGET) $(OBJS) $(DEPS);\
		cd Database; make clean

.PHONY: clean database
