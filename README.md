# mydcs
dcs implementation with only join relation

[geography](https://github.com/jkkummerfeld/text2sql-data/tree/master/data) is used for data

## requirements
### for runnning main part
* python3
* spaCy
* scala
### for rebuilding database
* mysql

## how to build
compiling scala program

```
make
```

## how to build database
password for mysql will be asked
```
make database
```

## how to run
`IN`is inpu query txt file

`DATABASE` is database name

`OUT` is Output base path (files such as Output/output.answer will be created)

Beisdes, for a database D, `Database/D.tag`, `Database/D.pred`, `Database/D.trig`, `Database/D.data` is required before run.

```example
make run IN=Input/query.txt DATABASE=geography OUT=Output/output
```

## style of each files

### *.tag

```
table_name:\n
tag1\ttag2...\n
...
```
### *.pred

```
pred1_name:table_name:column1\tcolumn2...\n
...
```
### *.trig

```
pred_name1:word1\tword2\tword3...\n
...
```

### *.data

```
table1:\n
tag1\ttag2...\n
column1\tcolumn2...\n
value0.1\tvalue0.2...\n
value1.1\tvalue1.2...\n
...
```

### *.tree

```
({root.text|root.lemma};\tchild1\tchild2...)\n
...
```

### *.dcs

```
("root_name";\t{Rel1Child1}....)\n
...
```

### *.answer

```
{answer}\n
...
```
