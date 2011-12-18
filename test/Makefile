#example Makefile for compilation of test_MOSAICO
#assume all source files are in the same directory of Makefile
#example for Gfortran compiler
PROG =	test_MOSAICO

SRCS =	Chronos.f90 DataTypeSizes.f90 ErrorCodes.f90 GeoLib.f90 GridLib.f90 \
	GridOperations.f90 IniLib.f90 LogLib.f90 Morphology.f90 \
	StringManipulation.f90 TableLib.f90 test_MOSAICO.f90 Units.f90 \
	Utilities.f90

OBJS =	Chronos.o DataTypeSizes.o ErrorCodes.o GeoLib.o GridLib.o \
	GridOperations.o IniLib.o LogLib.o Morphology.o StringManipulation.o \
	TableLib.o test_MOSAICO.o Units.o Utilities.o

LIBS = /usr/lib

NCMOD = /usr/include	

CC = cc
CFLAGS = -O
FC = f77
FFLAGS = -O
F90 = gfortran
F90FLAGS = -O3
LDFLAGS = -O3

all: $(PROG)

$(PROG): $(OBJS)
	$(F90) $(LDFLAGS) -o $@ $(OBJS) -L$(LIBS) -lnetcdff
clean:
	rm -f $(PROG) $(OBJS) *.mod

.SUFFIXES: $(SUFFIXES) .f90

.f90.o:
	$(F90) $(F90FLAGS) -c -I$(NCMOD) $<

Chronos.o: DataTypeSizes.o ErrorCodes.o LogLib.o StringManipulation.o Units.o
GeoLib.o: DataTypeSizes.o ErrorCodes.o IniLib.o LogLib.o StringManipulation.o \
	Units.o
GridLib.o: Chronos.o DataTypeSizes.o ErrorCodes.o GeoLib.o LogLib.o \
	StringManipulation.o Units.o Utilities.o
GridOperations.o: Chronos.o DataTypeSizes.o ErrorCodes.o GeoLib.o GridLib.o \
	IniLib.o LogLib.o StringManipulation.o Units.o
IniLib.o: DataTypeSizes.o ErrorCodes.o LogLib.o Utilities.o
LogLib.o: DataTypeSizes.o ErrorCodes.o
Morphology.o: DataTypeSizes.o GeoLib.o GridLib.o GridOperations.o LogLib.o \
	StringManipulation.o
StringManipulation.o: DataTypeSizes.o ErrorCodes.o LogLib.o
TableLib.o: DataTypeSizes.o ErrorCodes.o LogLib.o StringManipulation.o \
	Utilities.o
Units.o: DataTypeSizes.o
Utilities.o: DataTypeSizes.o
