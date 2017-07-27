CF         = mpif90
FFLAGS     = -O3 -Wall -fbounds-check -g -fdollar-ok
LD         = mpif90
LDFLAGS    = 
PREPROC    = 

OBJS =  type_abstract_buggy.o \
        lib_abstract_buggy.o \
        type_buggy.o  \
        IFORT_BUG.o  \


.SUFFIXES: .o .f90 .f
.f90.o:
	$(LD) -c $(FFLAGS) $<
.f.o:
	$(LD) -c $(FFLAGS) $<

ifort_bug.exe :$(OBJS) 
	$(LD) $(LDFLAGS) -o $@ $(OBJS) $(MPILIBDIR) $(MPILIBS)

clean :
	rm -f ifort_bug.exe *.o core *.mod

