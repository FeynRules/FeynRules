### Compilers ###
CC = g++

### Set the path of the different directories ###
SRCDIR=${PWD}/src
INCDIR=${PWD}/inc
LIBDIR=${PWD}/lib
BLDDIR=${PWD}/bld

### Flags ###
CFLAG= -O3 -I${INCDIR} -L${LIBDIR} -ansi

### Create a string containing all the sourcefiles ###
CFILES=$(wildcard ${SRCDIR}/*.cpp)

### Create a string containing all the object files ###
COBJS=$(subst .cpp,.o,$(subst ${SRCDIR},${BLDDIR},${CFILES}))

### Libraries ###
STLIB=-lm
LIB=${LIBDIR}/lib.a
HEADGSL=`gsl-config --cflags`
LIBGSL=`gsl-config --libs`

### Commands ###
all: ASperGe
lib: ${LIB}

ASperGe: main.cpp ${LIB}
	${CC} ${CFLAG} -o $@ main.cpp ${LIB} ${STLIB}  ${LIBGSL} ${HEADGSL}

#RUN: main.cpp ${COBJS}
#	${CC} ${CFLAG} -o $@ main.cpp ${COBJS} ${GSLIB} ${STLIB} -L${LIBGSL} 

${LIB}:	${COBJS}
	ar -ruc $@ ${BLDDIR}/*.o

${BLDDIR}/%.o: ${INCDIR}/%.hpp ${SRCDIR}/%.cpp
	cd ${BLDDIR}; ${CC} -c ${CFLAG} -L${INCDIR} ${CFILES} ${HEADGSL}; 

### Cleaning ###
clean:
	rm -f ASperGe ${LIBDIR}/*.a ${BLDDIR}/*.o *.log output/*; clear;

