PAS = invalid
DATABASE = geography

all: database

database: database.sh
	chmod +x database.sh; ./database.sh $(PAS) $(DATABASE)

clean:
	rm -r $(DATABASE)

.PHONY: clean
