#!/bin/bash

PASSWORD=$1
DATABASE=$2
SOURCE_ADDRESS=https://raw.githubusercontent.com/jkkummerfeld/text2sql-data/master/data/
SOURCE[0]=${DATABASE}-db.sql
SOURCE[1]=${DATABASE}-fields.txt
SOURCE[2]=${DATABASE}-schema.csv
SOURCE[3]=${DATABASE}.json
MAIN_SOURCE=${SOURCE[0]}
#tag file
TAG=../${DATABASE}.tag

function expect_pass(){
	expect -c "
	spawn bash -c \"$1\"
	expect \"Enter password:\"
	send \"$2\n\"
	interact
	"
}

tag=()
function tag_insert(){
	#$1 out file
	#$2 table name
	#$3 number of columns
	j=0
	for ((k=0; k<${#tag[@]}-1; k=$k+2))
	do
		if [ "${tag[$k]}" = "$2:" ]; then
			#count number of tags
			counttag=`echo -e "${tag[$k+1]}" | tr '\t' '\n' | wc -l`
			#check if the number of columns of the table and the number of the tags are the same
			if [ ${counttag} = $3 ]; then
				echo "${tag[$k+1]}" >> $1
				j=${j}+1
				break
			else
				echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!number of tags missmatched for table $2!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
			fi
		fi
	done
	if [ ${j} = 0 ]; then
		echo "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!no tag is set for table $2!!!!!!!!!!!!!!!!!!!!!!!!"
	fi
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

i=0
while read line
do 
	tag[i]=${line}
	i=${i}+1
done < ${TAG}

#add tables to database
#execpt the database name on the top
for ((i=1; i<${#tables[@]}; i++))
do
	#count the number of columns
	echo "select count(*) from information_schema.columns where table_name ='${tables[$i]}';" > tmp.sql
	expect_pass "mysql -N -u root -p ${DATABASE} < tmp.sql > tmp" ${PASSWORD}
	count=`cat tmp`

	echo "${tables[$i]}:" >> ../database.txt
	tag_insert "../database.txt" ${tables[$i]} ${count}

	echo "select * from ${tables[$i]};" > tmp.sql
	expect_pass "mysql -u root -p ${DATABASE} < tmp.sql >> ../database.txt" ${PASSWORD}
done

#remove database
echo "drop database ${DATABASE};" > tmp.sql
expect_pass "mysql -u root -p ${DATABASE} < tmp.sql" ${PASSWORD}

rm tmp
rm tmp.sql
