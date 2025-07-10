# Advanced OS and Virtualization

```sh
for i in {1..7}; do echo "---- $i: ----"; ./mmvm "test_programs/$i.out" foo bar baz; echo; done
```

```output
---- 1: ----
hello

---- 2: ----
a
---- 3: ----
hello

---- 4: ----
a=1234

---- 5: ----
argv[0]=test_programs/5.out
argv[1]=foo
argv[2]=bar
argv[3]=baz

---- 6: ----
012345
---- 7: ----
long = 4
short = 2
int = 2

```

---

```sh
./make_test.sh all
MODE=-m ./make_test.sh all
```

---

```sh
A=test_programs/1.out MODE=-d ./make_test.sh all
A=test_programs/1.out MODE=-m ./make_test.sh all
A=test_programs/1.out MODE=-m LIMIT=200 ./make_test.sh all
```
