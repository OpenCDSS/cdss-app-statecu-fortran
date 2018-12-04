# A simple hand-made makefile for a package including applications
# built from Fortran 90 sources, taking into account the usual
# dependency cases.
# ======================================================================
# the declarations
# ======================================================================
# The compiler
FC = gfortran
# Determine the StateCU version, to use in the executable
STATECU_VERSION := $(shell cat gcommon.inc | grep 'PARAMETER' | grep 'VERS' | cut -d '(' -f 2 | cut -d '=' -f 2 | tr -d ' ' | tr -d ')')
# The flags for debugging or for maximum performance, comment as necessary
#FCFLAGS = -g -fbounds-check -fno-align-commons
# use -static especially for windows compiling 
#FCFLAGS = -g -cpp -fbounds-check -fno-align-commons -static
FCFLAGS = -g -cpp -fno-align-commons -static -fbounds-check -fno-automatic -finit-local-zero
#FCFLAGS = -O2
# The flags for all cases (e.g. look for system .mod files, required in gfortran)
FCFLAGS += -I/usr/include
# The libraries needed for linking
#LDFLAGS = -li_need_this_lib
# The list of executables to be built within the package
PROGRAMS = statecu

# ======================================================================
# The rules
# ======================================================================
# The rule for making "all"
#   the default if make is called without arguments
#   note it only has the dependency and no actions
all: $(PROGRAMS)

# The rule for object files that depend on more than
#  the same named source (.for) file.  A general rule
#  below handles that simple case.
#  For example, include files.
#bin/bintop.o: src/common.inc
#bin/bomsec.o: src/common.inc
statecu.o: gcommon.inc pmcommon.inc pmdata.inc
annuacrp.for: xccommon.inc gcommon.inc
calpcrop.for: xccommon.inc gcommon.inc
clndr.for: gcommon.inc
dread.for: pmcommon.inc gcommon.inc xccommon.inc
dsum.for: gcommon.inc pmcommon.inc
dxcrain.for: gcommon.inc xccommon.inc
etascer.for: pmdata.inc pmcommon.inc gcommon.inc
etref.for: pmdata.inc gcommon.inc pmcommon.inc
etrref.for: pmdata.inc pmcommon.inc gcommon.inc
fall.for: gcommon.inc
findsta.for: gcommon.inc
flyfilld.for: gcommon.inc
flyfillf.for: gcommon.inc
flyfillp.for: gcommon.inc
flyfillt.for: gcommon.inc
foutput.for: gcommon.inc pmcommon.inc
frost.for: gcommon.inc
frostall.for: gcommon.inc
growth.for: gcommon.inc pmcommon.inc
indcrop.for: gcommon.inc
init.for: gcommon.inc
interkc.for: xccommon.inc gcommon.inc
intertd.for: gcommon.inc xccommon.inc
julian.for: gcommon.inc
kbasal.for: pmcommon.inc gcommon.inc xccommon.inc
kcpm.for: pmcommon.inc gcommon.inc
kcpm2.for: pmcommon.inc
lw_ccu.for: gcommon.inc
lw_init.for: gcommon.inc
lw_update.for: gcommon.inc
lw_write.for: gcommon.inc
mainxc.for: gcommon.inc xccommon.inc bindat.inc
myexit.for: gcommon.inc
perencrp.for: gcommon.inc xccommon.inc
pmclim.for: gcommon.inc pmcommon.inc
proj.for: gcommon.inc
proto.for: pmcommon.inc gcommon.inc xccommon.inc bindat.inc
rain.for: pmcommon.inc gcommon.inc
readin.for: xccommon.inc gcommon.inc pmdata.inc
readrcr.for: gcommon.inc
sb_final.for: gcommon.inc
sb_init.for: gcommon.inc
sbgetdist.for: gcommon.inc
sbupdate.for: gcommon.inc
slimit.for: gcommon.inc
spring.for: gcommon.inc
stable.for: gcommon.inc
summary.for: gcommon.inc pmcommon.inc
table.for: gcommon.inc
wbuild.for: gcommon.inc pmcommon.inc xccommon.inc
wbuildall.for: gcommon.inc pmcommon.inc xccommon.inc
wsupply.for: gcommon.inc
wsupsum.for: gcommon.inc bindat.inc
xcrain.for: gcommon.inc xccommon.inc

# The rule for building the statemod executable
#   The following is a carefully constructed list created by starting with statem.o
#   and adding files as needed to reconcile missing subroutine error messages.  
statecu: \
	statecu.o \
		dread.o \
		dsum.o \
		flyfillf.o \
		init.o \
		getcommandline_gfortran.o \
		julian.o \
		lw_init.o \
		lw_update.o \
		lw_ccu.o \
		lw_write.o \
		mainxc.o \
		myexit.o \
		pmclim.o \
		proj.o \
		proto.o \
		readfn.o \
		readin.o \
		readrcr.o \
		sb_init.o \
		sb_final.o \
		sbupdate.o \
		skipn.o \
		slimit.o \
		summary.o \
		wsupply.o \
		wsupsum.o \
			annuacrp.o \
			calpcrop.o \
			clndr.o \
			dayhrs.o \
			dxcrain.o \
			etascer.o \
			etref.o \
			etrref.o \
			fall.o \
			findsta.o \
			flyfilld.o \
			flyfillp.o \
			flyfillt.o \
			foutput.o \
			frost.o \
			frostall.o \
			growth.o \
			indcrop.o \
			interkc.o \
			intertd.o \
			kbasal.o \
			kcpm.o \
			kcpm2.o \
			perencrp.o \
			rain.o \
			sbgetdist.o \
			spring.o \
			stable.o \
			table.o \
			wbuildall.o \
			wbuild.o \
			xcrain.o
ifdef OS
	# Windows...
	$(FC) $(FCFLAGS) -o statecu-$(STATECU_VERSION)-gfortran-32bit.exe $^ $(LDFLAGS)
	@echo "-----------------------------------------------------------------------"
	@echo "Executable is statecu-$(STATECU_VERSION)-gfortran-32bit.exe"
	@echo "-----------------------------------------------------------------------"

else
	# Linux...
	$(FC) $(FCFLAGS) -o statecu-$(STATECU_VERSION) $^ $(LDFLAGS)
	@echo "-----------------------------------------------------------------------"
	@echo "Executable is statecu-$(STATECU_VERSION)-gfortran-32bit"
	@echo "-----------------------------------------------------------------------"
endif

# Using Fortran MODULES: prog4.f90 USEs a Fortran module defined
# inside mod.f90, this is similar to the include case (prog2), but,
# since there is no standard naming convention for compiled module
# files in f90, the dependency is more easily built on the object
# files, because when mod.o is generated, one is sure that
# any_module_inside_mod.mod has been newly generated as well; mod.o
# must also be linked in when building the executable, so the
# dependency on mod.o is added also for prog4, as in the external
# procedure case (prog3):
#prog4.o: mod.o
#prog4: mod.o

# ======================================================================
# The general rules.  These generally should not require modification
# ======================================================================

# General rule for building prog from prog.o; $^ (GNU extension) is
# used in order to list additional object files on which the
# executable depends
#%: bin/%.o
#	$(FC) $(FCFLAGS) -o bin/$@ $^ $(LDFLAGS)
%: %.o
	$(FC) $(FCFLAGS) -o $@ $^ $(LDFLAGS)

# General rules for building prog.o from prog.f90 or prog.F90; $< is
# used in order to list only the first prerequisite (the source file)
# and not the additional prerequisites such as module or include files
#bin/%.o: src/%.f90
%.o: %.f90
	$(FC) $(FCFLAGS) -o $@ -c $<

#bin/%.o: src/%.F90
%.o: src/%.F90
	$(FC) $(FCFLAGS) -o $@ -c $<

#added by jhb for the statemod files from ray b after changing everything to lower case
#bin/%.o: src/%.for
%.o: %.for
	$(FC) $(FCFLAGS) -o $@ -c $<

# Utility targets
.PHONY: clean veryclean

clean:
	rm -f bin/*.o *.mod *.MOD *.o

# help: print the targets that are available
help:
	@echo "--------------------------------------------------------------------------------"
	@echo "StateCU makefile targets:"
	@echo ""
	@echo "clean      Remove dynamically created files (but not final executable)."
	@echo "help       Print this message."
	@echo "statecu    Compile the StateCU executable, recompiling any .o if .for modified."
	@echo "veryclean  Make the 'clean' target, and also remove the final executable."
	@echo ""
	@echo "file.o     Compile the source file.for file into object file file.o,"
	@echo "           useful to check syntax for a single file."
	@echo "--------------------------------------------------------------------------------"
	@echo "Important makefile variables that are used:"
	@echo ""
	@echo "FC (compiler) = $(FC)"
	@echo "STATECU_VERSION (from gcommon.inc) = $(STATECU_VERSION)"
	@echo "--------------------------------------------------------------------------------"
	@echo "Important environment variables that are used:"
	@echo ""
	@echo "OS (to determine operating system) = $(OS)"
	@echo "--------------------------------------------------------------------------------"

veryclean: clean
	rm -f *~ $(PROGRAMS) *.exe
