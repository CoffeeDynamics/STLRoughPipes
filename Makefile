### Makefile

# Fortran compiler
FC = gfortran

# Fortran compilation flags
FCFLAGS = -Wall -Wextra -Werror -fmax-errors=1 -O3 -g -J$(MODDIR)

# Executable name
EXE = STL_bend.exe

# Directories
SRCDIR = src
MODDIR = mod
OBJDIR = obj
BINDIR = bin

# Special targets
.PHONY: clean


all: mkdir $(EXE)

mkdir:
	mkdir -p $(MODDIR) $(OBJDIR) $(BINDIR)

$(EXE):
	$(FC) $(FCFLAGS) -I$(OBJDIR) -o $(OBJDIR)/number_precision_m.o -c $(SRCDIR)/number_precision_m.f90
	$(FC) $(FCFLAGS) -I$(OBJDIR) -o $(OBJDIR)/globals_m.o -c $(SRCDIR)/globals_m.f90
	$(FC) $(FCFLAGS) -I$(OBJDIR) -o $(OBJDIR)/topology_m.o -c $(SRCDIR)/topology_m.f90
	$(FC) $(FCFLAGS) -I$(OBJDIR) -o $(OBJDIR)/STL_format_m.o -c $(SRCDIR)/STL_format_m.f90
	$(FC) $(FCFLAGS) -I$(OBJDIR) -o $(OBJDIR)/STL_read_m.o -c $(SRCDIR)/STL_read_m.f90
	$(FC) $(FCFLAGS) -I$(OBJDIR) -o $(OBJDIR)/STL_write_m.o -c $(SRCDIR)/STL_write_m.f90
	$(FC) $(FCFLAGS) -I$(OBJDIR) -o $(OBJDIR)/STL_bend.o -c $(SRCDIR)/STL_bend.f90
	$(FC) $(FCFLAGS) -I$(OBJDIR) -o $(BINDIR)/$(EXE) $(OBJDIR)/*.o

clean:
	rm -f $(OBJDIR)/*.o $(MODDIR)/*.mod $(BINDIR)/$(EXE)
