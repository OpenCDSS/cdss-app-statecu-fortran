# Makefile for compiling StateCU using gfortran
#
# ======================================================================
# Use 'make' or 'make help' to see usage.
# ======================================================================
#
# - has been tested with gfortran in MSys2 environment
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
# - example code line:
#      PARAMETER (VERS = 13.12)
# - "dev" at the end of the version (e.g., 15.00.14.dev) indicates the software is under development
STATECU_VERSION := $(shell cat gcommon.inc | grep 'PARAMETER' | grep 'VERS' | cut -d '(' -f 2 | cut -d '=' -f 2 | tr -d ' ' | tr -d ')')

# Number of bytes for executable
# - used because executable has been 32-bit until 2021
# - use in filename to clearly differentiate between 32-bit and 64-bit versions until transition to 64-bit is complete
# - TODO smalers 2021-03-11 focus on Windows and deal with Linux when there is time
ifdef OS
	# Windows, including MSys2
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

# Compiler options.

# Extra checks that can be added for deep check of code logic
# - do not use -all because 'array-temps' seems unnecessary
# - do include all the other checks
FFLAGS_EXTRA_CHECKS = -fcheck=bits,bounds,do,mem,pointer,recursion

# TODO smalers 2021-06-24 need to remove these old combinations when optimized and check versions are tested out.
# The flags for debugging or for maximum performance, comment as necessary.
#FFFLAGS = -g -fbounds-check -fno-align-commons
# use -static especially for windows compiling 
#FFFLAGS = -g -cpp -fbounds-check -fno-align-commons -static
# Original flgs
#FFFLAGS = -g -cpp -fno-align-commons -static -fbounds-check -fno-automatic -finit-local-zero
# The following is used to track down an IEEE_INVALID_FLAG message in gdb
#FFFLAGS = -g -cpp -fno-align-commons -static -fbounds-check -fno-automatic -finit-local-zero -fbacktrace -ffpe-trap=zero,overflow,underflow
# The following is helpful for tracking down runtime errors.
#FFLAGS_EXTRA_CHECKS = -fbacktrace -ffpe-trap=invalid,zero,overflow,underflow,denormal

# The following compiler options result in all runtime checks but is slower to run.
# - this is used by statecu_check target and 'check' variant
FFLAGS_CHECK = -g -O1 -Wall -cpp -DCOMPILER=$(COMPILER_ENUM) -fno-align-commons -static $(FFLAGS_ARCH) $(FFLAGS_EXTRA_CHECKS) -I/usr/include

# The following compiler options result in fewer runtime checks but is faster to run.
# -fcheck=bounds is retained because it catches major issues in array boundary issues
# - this is used by statecu_o3 target and 'o3' variant
FFLAGS_O3 = -Og -O3 -cpp -DCOMPILER=$(COMPILER_ENUM) -fno-align-commons -static -fcheck=bounds $(FFLAGS_ARCH) -I/usr/include

# Compiler flags for all cases (e.g. look for system .mod files, required in gfortran)
# - TODO smalers 2021-06-13 include above so it is easily included in variant FFLAGS
#FFLAGS += -I/usr/include

# The libraries needed for linking:
# - currently do not need because only core Fortran is used
#LDFLAGS = -li_need_this_lib

# The list of executables to be built within the package
PROGRAMS = statecu

# Set the source modules based on the operating system and compiler:
# - this allows this single makefile to work in multiple development environments (as long as GNU make is used)
# - currently the focus is gfortran on Windows but could get from the environment and change modules

# Compiler-specific routines to handle command line parsing and date/time processing.
# - this is an example similar to used in StateMod, in case it needs to be used for StateCU
# - set COMPILER_CHAR to use with C preprocessor
#   which can be used as an alternate way to support compiler-specific libraries
#   (see test.for) for example code
# - set COMPILER_ENUM to use with C preprocessor, because COMPILER_CHAR was having issues
ifeq ($(FC),gfortran)
	# gfortran code version
	#parse_o_file = parse_gfortran.o
	#dattim_o_file = dattim_gfortran.o
	COMPILER_ENUM = 1
else
	# Legacy Lahey code version (or other compiler that might be used)
	#parse_o_file = parse.o
	#dattim_o_file = dattim.o
	COMPILER_ENUM= 2
endif

# Operating-system-specific code:
# - Currently not needed for StateCU but include as an example
# - If needed use $(getpath_o_file) or similar where needed
# - Although there are other options, initially use code from Jim Brannon
# - OS environment variable is specific to Windows (assuming the compile occurs
#   in a Windows command shell) so if set assume Windows

#ifdef OS
#	# Assume Windows legacy naming convention
#	getpath_o_file = getpath.o
#	putpath_o_file = putpath.o
#else
#	# Assume Linux
#	getpath_o_file = getpath_linux.o
#	putpath_o_file = putpath_linux.o
#endif

# ======================================================================
# Main targets
# ======================================================================

# all
#
# - the default if make is called without arguments
# - use default of 'help' to force intentional actions such as 'make statecu'
# - uncomment line with 'printenv' if troubleshooting the environment
# ======================================================================
#all: printenv $(PROGRAMS)
all: help

# Dependencies for object files:
# - because statecu is the main output (not library, etc.) only concern is the include files
# - do `grep -i gcommon.inc *.for` to find files that use an include file
# - list alphabetically, ignoring case in order
statecu.o: gcommon.inc pmcommon.inc pmdata.inc
annuacrp.o: xccommon.inc gcommon.inc
calpcrop.o: xccommon.inc gcommon.inc
clndr.o: gcommon.inc
dread.o: pmcommon.inc gcommon.inc xccommon.inc
dsum.o: gcommon.inc pmcommon.inc
dxcrain.o: gcommon.inc xccommon.inc
etascer.o: pmdata.inc pmcommon.inc gcommon.inc
etref.o: pmdata.inc gcommon.inc pmcommon.inc
etrref.o: pmdata.inc pmcommon.inc gcommon.inc
fall.o: gcommon.inc
findsta.o: gcommon.inc
flyfilld.o: gcommon.inc
flyfillf.o: gcommon.inc
flyfillp.o: gcommon.inc
flyfillt.o: gcommon.inc
foutput.o: gcommon.inc pmcommon.inc
frost.o: gcommon.inc
frostall.o: gcommon.inc
growth.o: gcommon.inc pmcommon.inc
indcrop.o: gcommon.inc
init.o: gcommon.inc
interkc.o: xccommon.inc gcommon.inc
intertd.o: gcommon.inc xccommon.inc
julian.o: gcommon.inc
kbasal.o: pmcommon.inc gcommon.inc xccommon.inc
kcpm.o: pmcommon.inc gcommon.inc
kcpm2.o: pmcommon.inc
lw_ccu.o: gcommon.inc
lw_init.o: gcommon.inc
lw_update.o: gcommon.inc
lw_write.o: gcommon.inc
mainxc.o: gcommon.inc xccommon.inc bindat.inc
myexit.o: gcommon.inc
perencrp.o: gcommon.inc xccommon.inc
pmclim.o: gcommon.inc pmcommon.inc
proj.o: gcommon.inc
proto.o: pmcommon.inc gcommon.inc xccommon.inc bindat.inc
rain.o: pmcommon.inc gcommon.inc
readin.o: xccommon.inc gcommon.inc pmdata.inc
readrcr.o: gcommon.inc
sb_final.o: gcommon.inc
sb_init.o: gcommon.inc
sbgetdist.o: gcommon.inc
sbupdate.o: gcommon.inc
slimit.o: gcommon.inc
spring.o: gcommon.inc
stable.o: gcommon.inc
summary.o: gcommon.inc pmcommon.inc
table.o: gcommon.inc
wbuild.o: gcommon.inc pmcommon.inc xccommon.inc
wbuildall.o: gcommon.inc pmcommon.inc xccommon.inc
wsupply.o: gcommon.inc
wsupsum.o: gcommon.inc bindat.inc
xcrain.o: gcommon.inc xccommon.inc

# Below are the rules for building the statecu executable:
# - generally alphabetize
# - all source modules are listed
# - interdependencies are not accounted for, just the main program requirements on all code

# copy_o3_for_release
#
# Copy the o3 (optimized) variant to plain name for release.
copy_o3_for_release:
ifdef OS
	@# Windows...
	@cp statecu-$(STATECU_VERSION)-gfortran-win-$(EXE_BITS)bit-o3.exe statecu-$(STATECU_VERSION)-gfortran-win-$(EXE_BITS)bit.exe
	@echo "-----------------------------------------------------------------------"
	@echo "Release executable (copy of o3 variant) is statecu-$(STATECU_VERSION)-gfortran-win-$(EXE_BITS)bit.exe"
	@echo "-----------------------------------------------------------------------"
else
	@# Linux...
	@cp statecu-$(STATECU_VERSION)-gfortran-win-$(EXE_BITS)bit-o3 statecu-$(STATECU_VERSION)-gfortran-win-$(EXE_BITS)bit
	@echo "-----------------------------------------------------------------------"
	@echo "Release executable (copy of o3 variant) is statecu-$(STATECU_VERSION)-gfortran-lin-$(EXE_BITS)bit"
	@echo "-----------------------------------------------------------------------"
endif

# help
#
# Print the targets that are available.
help:
	@echo "-------------------------------------------------------------------------------------------"
	@echo "StateCU makefile targets:"
	@echo ""
	@echo "all               Default target that prints this help message."
	@echo "checkarch         Check that object files are consistent with shell (32 or 64 bit)."
	@echo "clean             Remove dynamically created files (but not final executable)."
	@echo "help              Print this message."
	@echo "printenv          Helper to print which operating system is used, controls configuration."
	@echo "statecu_prog      Compile the StateCU executable, recompiling any .o if .for modified."
	@echo "                  Same as 'statecu_check' to compile with all checks for development testing."
	@echo "                  USE FOR NORMAL DEVELOPMENT."
	@echo "statecu_check     Compile the StateCU executable including all runtime checks."
	@echo "statecu_o3        Compile the StateCU executable for optimization level 3 and limited runtime checks."
	@echo "                  USE FOR PRODUCTION RELEASE AND FULL TESTING."
	@echo "statecu_release   Do clean compile on each release variant and copy o3 variant to plain name."
	@echo "veryclean         Make the 'clean' target, and also remove the final executable."
	@echo "veryclean_check   Needed by statemod_release."
	@echo "veryclean_o3      Needed by statemod_release."
	@echo ""
	@echo "file.o            Compile the source file.for file into object file file.o,"
	@echo "                  useful to check syntax for a single file."
	@echo "-------------------------------------------------------------------------------------------"
	@echo "Important makefile variables that are used:"
	@echo ""
	@echo "FC (compiler) = $(FC)"
	@echo "STATECU_VERSION (from gcommon.inc) = $(STATECU_VERSION)"
	@echo "EXE_BITS (from environment) = $(EXE_BITS)"
	@echo "-------------------------------------------------------------------------------------------"
	@echo "Important environment variables that are used:"
	@echo ""
	@echo "OS (to determine operating system) = $(OS)"
	@echo "MSYSTEM (to determine EXE_BITS on Windows) = $(MSYSTEM)"
	@echo "-------------------------------------------------------------------------------------------"
	@echo "To force compile of files even if errors:  make -k statecu_prog"
	@echo "To force compile and save output to file:  make -k statecu_prog 2>&1 | tee statecu-compile.log"
	@echo "To count warning types from above:         ./count-warnings.bash"
	@echo "-------------------------------------------------------------------------------------------"

# statecu_check
#
# The default rule for building StateCU executable.
# For developers, the full check version is used to ensure that issues don't slip through.
# For production testing, the optimized version is used because it is faster.
# Automated tests can be run on check and optimized variants to compare results.
# Automated tests should be run on the optimized version prior to release.
statecu_check: FFLAGS = $(FFLAGS_CHECK)
statecu_check: VARIANT = -check
statecu_check: statecu_compile

# statecu_o3
#
# The rule for building StateCU optimized executable:
# - see statecu_check comments.
# - use multiple rules because ran into problems trying to list under one target
statecu_o3: FFLAGS = $(FFLAGS_O3)
statecu_o3: VARIANT = -o3
statecu_o3: statecu_compile copy_o3_for_release

# statecu_prog
#
# Same as statecu_check, to compile the check executable used during development (all checks).
# - TODO smalers 2021-06-30 can't use 'statecu' because it interferes with statecu.o, etc.
statecu_prog: statecu_check

# statecu_release
#
# Do a full clean compile for release.
# - do the optimized version first because normal work uses the full-check version and want
#   those .o files to be in working files for incremental make
# - use separate calls to 'make' to ensure variables are set correctly
statecu_release:
	make veryclean_o3
	make statecu_o3
	make veryclean_check
	make statecu_check

# statecu_compile
#
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
	@# Windows...
	$(FC) $(FFLAGS) -o statecu-$(STATECU_VERSION)-gfortran-win-$(EXE_BITS)bit$(VARIANT).exe $^ $(LDFLAGS)
	@echo "-----------------------------------------------------------------------"
	@echo "Executable is statecu-$(STATECU_VERSION)-gfortran-win-$(EXE_BITS)bit$(VARIANT).exe"
	@echo "-----------------------------------------------------------------------"
else
	@# Linux...
	$(FC) $(FFLAGS) -o statecu-$(STATECU_VERSION)-gfortran-lin-$(EXE_BITS)bit$(VARIANT) $^ $(LDFLAGS)
	@echo "-----------------------------------------------------------------------"
	@echo "Executable is statecu-$(STATECU_VERSION)-gfortran-lin-$(EXE_BITS)bit$(VARIANT)"
	@echo "-----------------------------------------------------------------------"
endif

# TODO smalers 2021-06-30 Not sure what 'prog4' is and whether this can be removed.
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
# The general rules.  These generally should not require modification.
# ======================================================================

# General rule for building prog from prog.o; $^ (GNU extension) is
# used in order to list additional object files on which the
# executable depends
%: %.o
	$(FC) $(FFLAGS) -o $@ $^ $(LDFLAGS)

# General rules for building prog.o from prog.f90 or prog.F90; $< is
# used in order to list only the first prerequisite (the source file)
# and not the additional prerequisites such as module or include files
%.o: %.f90
	$(FC) $(FFLAGS) -c $<

%.o: %.F90
	$(FC) $(FFLAGS) -c $<

# Compile files with extension .for into .o object files.
%.o: %.for
	$(FC) $(FFLAGS) -c $<

# Utility targets
.PHONY: clean veryclean

# checkarch
#
# Check the architecture to make sure that the compile does not attempt to mix 32-bit and 64-bit object files.
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

# clean
#
# Remove intermediate files used when compiling:
# - Do not remove *.obj because that is used with Lahey and want gfortran compile to be separate.
# - normal artifacts are *.o, ec.
# - additional artifacts may be created due to compiler problem, for example as listed below.
#     statecu.for.236r.expand
#     statecu.for.237r.vregs
#     statecu.for.238r.into_cfglayout
#     statecu.for.239r.jump
#     statecu.for.251r.reginfo
#     statecu.for.274r.outof_cfglayout
#     statecu.for.275r.split1
#     statecu.for.277r.dfinit
#     statecu.for.278r.mode_sw
#     statecu.for.279r.asmcons
#     statecu.for.284r.ira
#     statecu.for.285r.reload
#     statecu.for.292r.pro_and_epilogue
#     statecu.for.295r.jump2
#     statecu.for.306r.split4
#     statecu.for.307r.stack
#     statecu.for.308r.alignments
#     statecu.for.310r.mach
#     statecu.for.311r.barriers
#     statecu.for.316r.shorten
#     statecu.for.317r.nothrow
#     statecu.for.318r.dwarf2
#     statecu.for.319r.final
#     statecu.for.320r.dfinish
clean: printenv
	@echo "-----------------------------------------------------------------------"
	@echo "clean"
	@echo ""
	@echo "Removing compile artifacts but not final executable."
	@echo "-----------------------------------------------------------------------"
	rm -f bin/*.o *.mod *.MOD *.o
	rm -f *.for.*r.*

# printenv
#
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

# veryclean
#
# Remove all intermediate files and the final executable.
# - only remove the executable for current architecture in case want to run both
veryclean: printenv clean
	@echo "-----------------------------------------------------------------------"
	@echo "veryclean"
	@echo ""
	@echo "Removing compile artifacts (make clean) and final executable."
	@echo "Only executables for current StateCU $(EXE_BITS)-bit version are removed."
	@echo "-----------------------------------------------------------------------"
ifdef OS
	# Windows
	# - only remove the executable for the OS architecture of interest
	rm -f *~ $(PROGRAMS) statecu-$(STATECU_VERSION)-gfortran-win-$(EXE_BITS)bit*.exe
else
	# Linux
	# - only remove the executable for the OS architecture of interest
	# - no file extension
	rm -f *~ $(PROGRAMS) statecu-$(STATECU_VERSION)-gfortran-lin-$(EXE_BITS)bit*
endif

# veryclean_check
#
# Remove all intermediate files and the final executable.
# - only remove the executable for current architecture and 'check' variant in case want to run both
veryclean_check: printenv clean
	@echo "-----------------------------------------------------------------------"
	@echo "veryclean_check"
	@echo ""
	@echo "Removing compile artifacts (make clean) and final executable."
	@echo "Only executable for current StateCU $(EXE_BITS)-bit $(VARIANT) variant is removed."
	@echo "-----------------------------------------------------------------------"
ifdef OS
	# Windows
	# - only remove the executable for the OS architecture of interest
	rm -f *~ $(PROGRAMS) statecu-$(STATECU_VERSION)-gfortran-win-$(EXE_BITS)bit-check.exe
else
	# Linux
	# - only remove the executable for the OS architecture of interest
	# - no file extension
	rm -f *~ $(PROGRAMS) statecu-$(STATECU_VERSION)-gfortran-lin-$(EXE_BITS)bit-check
endif

# veryclean_o3
#
# Remove all intermediate files and the final executable.
# - only remove the executable for current architecture and 'o3' variant in case want to run both
veryclean_o3: printenv clean
	@echo "-----------------------------------------------------------------------"
	@echo "veryclean_o3"
	@echo ""
	@echo "Removing compile artifacts (make clean) and final executable."
	@echo "Only executable for current StateCU $(EXE_BITS)-bit $(VARIANT) variant is removed."
	@echo "-----------------------------------------------------------------------"
ifdef OS
	# Windows
	# - only remove the executable for the OS architecture of interest
	rm -f *~ $(PROGRAMS) statecu-$(STATECU_VERSION)-gfortran-win-$(EXE_BITS)bit-o3.exe
else
	# Linux
	# - only remove the executable for the OS architecture of interest
	# - no file extension
	rm -f *~ $(PROGRAMS) statecu-$(STATECU_VERSION)-gfortran-lin-$(EXE_BITS)bit-o3
endif
