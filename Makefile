FFLAGS = -O2 -I.
fd1d_heat_explicit.exe: fd1d_heat_explicit.o Types_mod.o RHS_mod.o CFL_mod.o IO_mod.o Solver_mod.o
			gfortran $^ -o $@
fd1d_heat_explicit.o: 	fd1d_heat_explicit.f90 Types_mod.o RHS_mod.o CFL_mod.o IO_mod.o Solver_mod.o
		    	gfortran $(FFLAGS) -c $<
Types_mod.o: 		Types_mod.f90
			gfortran $(FFLAGS) -c $<
RHS_mod.o: 		RHS_mod.f90 Types_mod.o
			gfortran $(FFLAGS) -c $<
CFL_mod.o: 		CFL_mod.f90 Types_mod.o RHS_mod.o
			gfortran $(FFLAGS) -c $<
IO_mod.o: IO_mod.f90 Types_mod.o
			gfortran $(FFLAGS) -c $<
Solver_mod.o: Solver_mod.f90 Types_mod.o RHS_mod.o
			gfortran $(FFLAGS) -c $<
.PHONY: clean
clean: 
			rm -rf *.o *.mod *.exe
