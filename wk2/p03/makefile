#==============================#
# Makefile                     #
# CSI 501                      #
#------------------------------#
# Fernando Camelli             #
# Fall 2011                    #
#==============================#
# makefiel for vector examples #
#==============================#
#compilers
COMP=gnu

# variables for mod00
#-------------------------
SRCMOD01=my_func.f90
OBJMOD01=my_func.o

# variables for mod01
#-------------------------
SRCMOD02=my_mod.f90
OBJMOD02=my_mod.o

# variables for main
#-------------------------
EXE00=p03
SRC00=p03.f90
OBJ00=p03.o


#--------------------------------------------
# FLAGS for Fortran  compiler
#--------------------------------------------
ifdef $(DBG)
   OPT= -g
else
   OPT= -O3
endif

FF=gfortran
CFLAGS= -c -Wall -pedantic $(OPT)
ifeq ($(COMP),gnu)
   # gnu compilers are used
   FF=gfortran
   CFLAGS= -c -Wall -pedantic $(OPT)
else
   # intel compilers are used
   FF=ifort
   CFLAGS= -c -warn all $(OPT)
endif

LFLAGS= $(OPT) 

# more variables
#---------------
EXES= $(EXE00)


#=======================================================================
#=======================================================================
#=======================================================================

#
# targets
#
all: $(EXES)

#-------------------------------------------------------------------
#-------------------------------------------------------------------
# example: 1 main and 2 modules
$(EXE00): $(OBJ00) $(OBJMOD01) $(OBJMOD02)
	@echo "-----------------------------------------------------"
	@echo "[FF] linking"
	$(FF)  $(LFLAGS) $(OBJ00) $(OBJMOD01) $(OBJMOD02) -o $(EXE00) 
	@echo "[done]"
	@echo "-----------------------------------------------------"

$(OBJ00): $(OBJMOD01) $(OBJMOD02) $(SRC00) 
	@echo "-----------------------------------------------------"
	@echo "[FF] compiling objects"
	$(FF) $(CFLAGS)  $(SRC00)

$(OBJMOD02): $(SRCMOD02) $(OBJMOD01)
	@echo "-----------------------------------------------------"
	@echo "[FF] compiling objects"
	$(FF) $(CFLAGS)  $(SRCMOD02)

$(OBJMOD01): $(SRCMOD01) 
	@echo "-----------------------------------------------------"
	@echo "[FF] compiling objects"
	$(FF) $(CFLAGS)  $(SRCMOD01)

#-------------------------------------------------------------------
#===========================================================
# misc targets
#===========================================================
clean:
	@echo "------------------------------------------"
	@echo "removing object, executable, and tar files"
	rm -f *.o $(EXES)  *.mod *.mat
	@echo "[done]"
	@echo "------------------------------------------"

