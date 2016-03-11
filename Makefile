FC = gfortran

FFLAGS = -O2 -I. -I/usr/include -I/usr/lib/fortran/modules/plplot

LDFLAGS = -L. -L/usr/lib -lnetcdff -lnetcdf -lplplotf95cd -lplplotf95d
LIBS = Types_mod.o RHS_mod.o CFL_mod.o IO_mod.o Solver_mod.o

fd1d_heat_explicit.exe: fd1d_heat_explicit.o $(LIBS)
			$(FC) $^ $(LDFLAGS) -o $@
fd1d_heat_explicit.o: 	fd1d_heat_explicit.f90 $(LIBS)
		    	$(FC) $(FFLAGS) -c $<
Types_mod.o: 		Types_mod.f90
			$(FC) $(FFLAGS) -c $<
RHS_mod.o: 		RHS_mod.f90 Types_mod.o
			$(FC) $(FFLAGS) -c $<
CFL_mod.o: 		CFL_mod.f90 Types_mod.o RHS_mod.o
			$(FC) $(FFLAGS) -c $<
IO_mod.o: IO_mod.f90 Types_mod.o
			$(FC) $(FFLAGS) -c $<
Solver_mod.o: Solver_mod.f90 Types_mod.o RHS_mod.o
			$(FC) $(FFLAGS) -c $<
diff: fd1d_heat_explicit.exe
			./fd1d_heat_explicit.exe
			diff h_test01.txt h_test01.txt_bak
			if [ $$? -eq 0 ]; then echo "fine"; else echo "not fine"; fi 
.PHONY: clean
clean: 
			rm -rf *.o *.mod *.exe			
