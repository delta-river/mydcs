#IN input tree base_path
#DATA database base_path
#OUT output base_path
all: prepare

prepare:
	cd mydcs; make

run: query mydcs

query:
	cd query; make IN=../$(IN) OUT=../$(OUT)

mydcs:
	cd mydcs; make run IN=../$(DATA) OUT=../$(OUT)

clean:
	cd mydcs; make clean

.PHONY: clean query mydcs
