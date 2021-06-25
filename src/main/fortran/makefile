# Makefile for compiling StateCU using gfortran
#
# ======================================================================
# Use 'make' or 'make help' to see usage.
# ======================================================================
#
# - initial version taken from Jim Brannon's work
# - significant updates to add documentation, help target, make portable
# - tested with gfortran
# - operating system is determined by checking for OS environment variable,
#   which indicates Windows, otherwise Linux variant is assumed
#
# ======================================================================
# Declarations
# ======================================================================
# The compiler
FC = gfortran

# Determine the StateCU version
# - used in the executable name
# - also used for installer upload folder, etc.
# - "dev" at the end of the version (e.g., 15.00.14.dev) indicates the software is under development
STATECU_VERSION := $(shell cat gcommon.inc | grep 'PARAMETER' | grep 'VERS' | cut -d '(' -f 2 | cut -d '=' -f 2 | tr -d ' ' | tr -d ')')

# Number of bytes for executable
# - used because executable has been 32-bit until 2021
# - use in filename to clearly differentiate between 32-bit and 64-bit versions until transition to 64-bit is complete
# - TODO smalers 2021-03-11 focus on Windows and deal with Linux when there is time
ifdef OS
	# Windows
	EXE_BITS = 32
	ifeq ($(MSYSTEM),MINGW32)
		EXE_BITS = 32
		FFLAGS_ARCH =
		# String to check for in 'file *.o'
		OBJ_FILE_TYPE = 80386
		OBJ_WRONG_FILE_TYPE = amd64
	endif
	ifeq ($(MSYSTEM),MINGW64)
		EXE_BITS = 64
		# The following is needed for 64-bit
		# - see: https://github.com/actions/virtual-environments/issues/2721
		FFLAGS_ARCH = -Wl,--default-image-base-low
		# String to check for in 'file *.o'
		OBJ_FILE_TYPE = amd64
		OBJ_WRONG_FILE_TYPE = 80386
	endif
else
	# Linux
	# - always compile 64-bit version
	EXE_BITS = 64
endif

BAD_OBJ_COUNT := $(shell file *.o | grep $(OBJ_WRONG_FILE_TYPE) | wc -l)

# Below are the flags for debugging or for maximum performance, commented to explain use.

# The flags for debugging or for maximum performance, comment as necessary.
#FCFLAGS = -g -fbounds-check -fno-align-commons
# use -static especially for windows compiling 
#FCFLAGS = -g -cpp -fbounds-check -fno-align-commons -static
# Original flgs
#FCFLAGS = -g -cpp -fno-align-commons -static -fbounds-check -fno-automatic -finit-local-zero
# The following is used to track down an IEEE_INVALID_FLAG message in gdb
#FCFLAGS = -g -cpp -fno-align-commons -static -fbounds-check -fno-automatic -finit-local-zero -fbacktrace -ffpe-trap=zero,overflow,underflow
FFLAGS_EXTRA_CHECKS = -fbacktrace -ffpe-trap=invalid,zero,overflow,underflow,denormal
# The following works but unfortunately uses -fno-automatic and -finit-local-zero.
# FCFLAGS = -g -cpp -DCOMPILER=$(COMPILER_ENUM) -fno-align-commons -static -fbounds-check -fno-automatic -finit-local-zero $(FFLAGS_ARCH) $(FFLAGS_EXTRA_CHECKS)
# The following removes -fno-automatic and -finit-local-zero and turns on all warnings
FCFLAGS = -g -O1 -Wall -cpp -DCOMPILER=$(COMPILER_ENUM) -fno-align-commons -static -fbounds-check $(FFLAGS_ARCH) $(FFLAGS_EXTRA_CHECKS)
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
#all: $(PROGRAMS)
all: help

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

# The rule for building the statecu executable
#   The following is a carefully constructed list created by starting with statecu.o
#   and adding files as needed to reconcile missing subroutine error messages.  
statecu: checkarch statecu_compile
	echo "In statecu"

# This target is used internally to allow 'checkarch' to be executed.
statecu_compile: \
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
	$(FC) $(FCFLAGS) -o statecu-$(STATECU_VERSION)-gfortran-win-$(EXE_BITS)bit.exe $^ $(LDFLAGS)
	@echo "-----------------------------------------------------------------------"
	@echo "Executable is statecu-$(STATECU_VERSION)-gfortran-win-$(EXE_BITS)bit.exe"
	@echo "-----------------------------------------------------------------------"

else
	# Linux...
	$(FC) $(FCFLAGS) -o statecu-$(STATECU_VERSION)-gfortran-lin-$(EXE_BITS)bit $^ $(LDFLAGS)
	@echo "-----------------------------------------------------------------------"
	@echo "Executable is statecu-$(STATECU_VERSION)-gfortran-lin-$(EXE_BITS)bit"
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

# Check the architecture to make sure are not trying to mix 32-bit and 64-bit object files.
ifneq ($(BAD_OBJ_COUNT),0)
ifdef OS
checkarch:
	@echo "Count of files compiled with wrong compiler: $(BAD_OBJ_COUNT)"
	$(error Detected $(BAD_OBJ_COUNT) object files of wrong bit count.  Run 'make veryclean' and 'make statecu' to force recompile with correct compiler for current shell.)
else
checkarch:
	@echo "Assuming 64-bit on Linux."
endif
else
ifdef OS
checkarch:
	@echo "Count of files compiled with wrong compiler: $(BAD_OBJ_COUNT)"
else
checkarch:
	@echo "Assuming 64-bit on Linux."
endif
endif

# Remove intermediate files used when compiling:
# - Do not remove *.obj because that is used with Lahey and want gfortran compile to be separate.
# - normal artifacts are *.o, ec.
# - additional artifacts may be created due to compiler problem, for example as listed below.
clean: printenv
	@echo "-----------------------------------------------------------------------"
	@echo "clean"
	@echo ""
	@echo "Removing compile artifacts but not final executable."
	@echo "-----------------------------------------------------------------------"
	rm -f bin/*.o *.mod *.MOD *.o
	rm -f *.for.*r.*

# help: print the targets that are available
help:
	@echo "--------------------------------------------------------------------------------"
	@echo "StateCU makefile targets:"
	@echo ""
	@echo "checkarch  Check that object files are consistent with shell 32 or 64 bits."
	@echo "clean      Remove dynamically created files (but not final executable)."
	@echo "help       Print this message."
	@echo "printenv   Helper to print which operating system is used, controls configuration."
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
	@echo "MSYSTEM (to determine EXE_BITS on Windows) = $(MSYSTEM)"
	@echo "--------------------------------------------------------------------------------"
	@echo "To force compile of files even if errors:  make -k statecu"
	@echo "To force compile and save output to file:  make -k statecu 2>&1 | tee somefile"
	@echo "--------------------------------------------------------------------------------"

# Print important environment information to help understand setup.
printenv:
	@echo "-----------------------------------------------------------------------"
	@echo "Environment information:"
ifdef OS
	@echo "Windows detected from OS environment variable:  yes"
else
	@echo "Windows detected from OS environment variable:  no"
	@echo "...assuming Linux"
endif
ifdef MSYSTEM
	@echo "MINGW (gfortran) detected from MSYSTEM environment variable:  yes"
else
	@echo "MINGW (gfortran) detected from MSYSTEM environment variable:  no"
endif
	@echo "-----------------------------------------------------------------------"

# Remove all intermediate files and the final executable.
# - only remove the executable for current architecture in case want to run both
veryclean: printenv clean
	@echo "-----------------------------------------------------------------------"
	@echo "veryclean"
	@echo ""
	@echo "Removing compile artifacts (make clean) and final executable."
	@echo "Only executable for current StateCU $(EXE_BITS)-bit version is removed."
	@echo "-----------------------------------------------------------------------"
ifdef OS
	# Windows
	# - only remove the executable for the OS architecture of interest
	rm -f *~ $(PROGRAMS) statecu-$(STATECU_VERSION)-gfortran-win-$(EXE_BITS)bit.exe
else
	# Linux
	# - only remove the executable for the OS architecture of interest
	# - no file extension
	rm -f *~ $(PROGRAMS) statecu-$(STATECU_VERSION)-gfortran-lin-$(EXE_BITS)bit
endif
