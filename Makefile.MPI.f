 ##############################################################################
# FILE: Makefile.MPI.f
# DESCRIPTION:
#   Makefile for all IBM MPI Fortran example codes
# AUTHOR: Blaise Barney
# LAST REVISED:  06/13/13
##############################################################################

#Uncomment your choice of compiler
#F90    =    mpiifort
F90    =    mpif90
#F90    =    mpipgf90
CC 	=   icc

#Add your choice of flags
FLAGS   =   -O1 -w

all:    mpi_latency1     \
   

clean:  
	/bin/rm -rf     \
	mpi_latency     \
	*.o


mpi_latency:  mpi_latency1.f
	$(F90) $(FLAGS) mpi_latency1.f -o mpi_latency1

