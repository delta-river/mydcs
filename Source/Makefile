#IN input tree base_path
#DATA database base_path
#OUT output base_path
all: query mydcs

query:
	cd query; make IN=$(IN) OUT=$(OUT)

mydcs:
	cd mydcs; make IN=$(DATA) OUT=$(OUT)

clean:
	cd mydcs; make clean

.PHONY: clean
