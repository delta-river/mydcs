TARGET = database
PAS = invalid
DATABASE = geography

all: $(TARGET)

database: database.sh
	chmod +x database.sh; ./database.sh $(PAS) $(DATABASE)

clean:
	rm -r $(DATABASE) database.txt

.PHONY: clean
