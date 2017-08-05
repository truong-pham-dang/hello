# Start of the makefile
# Defining variables
objects = type_1.o type_1_extends.o main.o
f90comp = gfortran
switch = -O3 -std=f2008
# Makefile
execname: $(objects)
	$(f90comp) -o execname $(switch) $(objects)
type_1.mod: type_1.o type_1.f08
	$(f90comp) -c $(switch) type_1.f08
type_1_extends.mod: type_1_extends.o type_1_extends.f08
	$(f90comp) -c $(switch) type_1_extends.f08
main.o: type_1.mod main.f08
	$(f90comp) -c $(switch) main.f08
%.o: %.f08
	$(f90comp) -c $(switch) $<
# Cleaning everything
clean:
	rm type_1.mod
	rm type_1_extends.mod
	rm execname 
	rm $(objects)
# End of the makefile
