#!/bin/bash

PASSWORD=$1
DATABASE=$2
SOURCE_ADDRESS=https://raw.githubusercontent.com/jkkummerfeld/text2sql-data/master/data/
SOURCE[0]=${DATABASE}-db.sql
SOURCE[1]=${DATABASE}-fields.txt
SOURCE[2]=${DATABASE}-schema.csv
SOURCE[3]=${DATABASE}.json
MAIN_SOURCE=${SOURCE[0]}

function expect_pass(){
	expect -c "
	spawn bash -c \"$1\"
	expect \"Enter password:\"
	send \"$2\n\"
	interact
	"
}

#reset database
if [ -e database.txt ]; then
	rm database.txt
fi
touch database.txt

#create a directory for the specific database
mkdir -p ${DATABASE}
cd ${DATABASE}
for s in ${SOURCE[@]}
do
	if [ ! -e $s ]; then
		wget ${SOURCE_ADDRESS}$s
	fi
done

#create database
echo "drop database if exists ${DATABASE}; create database ${DATABASE};" > tmp.sql
expect_pass "mysql -u root -p < tmp.sql" ${PASSWORD}

#create database tables
expect_pass "mysql -u root -p ${DATABASE} < ${MAIN_SOURCE}" ${PASSWORD}

#extracting tables name
echo "show tables;" > tmp.sql
expect_pass "mysql -u root -p ${DATABASE} < tmp.sql > table_name_tmp" ${PASSWORD}

i=0
tables=()
while read line 
do
	tables[i]=${line}
	i=${i}+1
done < table_name_tmp

rm table_name_tmp

#add tables to database
#execpt the database name on the top
for ((i=1; i<${#tables[@]}; i++))
do
	echo "select * from ${tables[$i]};" > tmp.sql
	echo "${tables[$i]}:" >> ../database.txt
	expect_pass "mysql -u root -p ${DATABASE} < tmp.sql >> ../database.txt" ${PASSWORD}
done

#remove database
echo "drop database ${DATABASE};" > tmp.sql
expect_pass "mysql -u root -p ${DATABASE} < tmp.sql" ${PASSWORD}

rm tmp.sql
