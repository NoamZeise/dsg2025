# Duck Sauce Games Jam 2025

## dependanices

all:
- SBCL
- quicklisp

linux only:
check Dockerfile for all debian packages needed

## sbcl build

run make in this repo 

## docker build for debian

in this repo run
```
docker build . -t dsg2025
docker run -it --rm -v=.:/dsg2025 dsg2025
make
exit
```
then the binary should be in bin
