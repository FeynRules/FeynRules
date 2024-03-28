### Compilers ###
CC = g++

### Set the path of the different directories ###
SRCDIR=${PWD}/src
INCDIR=${PWD}/inc
LIBDIR=${PWD}/lib
BLDDIR=${PWD}/bld

### Flags ###
CFLAG= -O3 -I${INCDIR} -L${LIBDIR} -std=c++11 -fPIC

### Create a string containing all the sourcefiles ###
CFILES=$(wildcard ${SRCDIR}/*.cpp)
HFILES=$(wildcard ${SRCDIR}/*.hpp)

### Create a string containing all the object files ###
COBJS=$(subst .cpp,.o,$(subst ${SRCDIR},${BLDDIR},${CFILES}))

### Libraries ###
ARMADILLO=$(shell pkg-config --list-all | grep armadillo -m1 | cut -d " " -f1)
ifeq ($(ARMADILLO), )
	ARMADILLO=armadillo
endif

STLIB=-lm -l${ARMADILLO}
LIB=${LIBDIR}/librunning.a
HEADGSL=`gsl-config --cflags`
LIBGSL=`gsl-config --libs`

### Commands ###
all: ${BLDDIR} ${LIBDIR} ${LIB}
      
lib: ${LIB}

${BLDDIR}:
	mkdir ${BLDDIR}

${LIBDIR}:
	mkdir ${LIBDIR}


runDiag.so: ${LIB} ${LIBDIR}
	${CC} ${CFLAG} -shared -o $@ ${LIB} ${STLIB} ${LIBGSL} ${HEADGSL}

${LIB}:	${COBJS} ${BLDDIR}
	ar -ruc $@ ${BLDDIR}/*.o

${BLDDIR}/%.o: ${SRCDIR}/%.cpp ${HFILES}
	${CC} -o $@ -c ${CFLAG} -L${INCDIR} $<  ${HEADGSL};


### Cleaning ###
clean:
	rm -f runDiag.o runDiag.so ${LIBDIR}/*.a ${BLDDIR}/*.o *.log output/*;
