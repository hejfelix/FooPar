FooPar
======

Functional Object Oriented Parallel Framework in Scala


Installation
======
Requires sbt >=0.13. On ubuntu 14.04, 
this can be installed as such:

sudo apt-get install sbt


To install and run the tests in FooPar, use the following commands:

```
git clone https://github.com/hejfelix/FooPar.git

cd FooPar/

sbt

test:runMain FooParChecks -fpppn 8 -fpnp 8 
```


This will clone the repository, open the sbt project and run the scalacheck 
test suite with 8 FooPar processes per node and FooPar-Number-Processes= 8
