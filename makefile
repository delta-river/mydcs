DATABASE = geography

#IN input query file
#DATA database base_path
DATA = Database/$(DATABASE)
#OUT output base_path
OUT = ./Output/output

all: main

main:
	cd Source; make IN=$(IN) DATA=$(DATA) OUT=$(OUT)

#password for mysql
database:
	@echo "mysql password:"; stty -echo; read PAS;stty echo;cd Database; make DATABASE=$(DATABASE) PAS="$$PAS"

clean:
	cd Database; make clean; cd Source; make clean

.PHONY: clean database
