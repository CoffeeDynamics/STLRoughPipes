FC = gfortran
EXE1 = STL_bend.exe
SRCDIR = src
MODDIR = mod
EXE1OBJDIR = obj1
FCFLAGS = -Wall -Wextra -Werror -fmax-errors=1 -O3 -g -J$(MODDIR)

.PHONY: clean

all: mkdir $(EXE1)

mkdir:
	mkdir -p $(MODDIR) $(EXE1OBJDIR)

$(EXE1):
	$(FC) $(FCFLAGS) -I$(EXE1OBJDIR) -o $(EXE1OBJDIR)/number_precision_m.o -c $(SRCDIR)/number_precision_m.f90
	$(FC) $(FCFLAGS) -I$(EXE1OBJDIR) -o $(EXE1OBJDIR)/globals_m.o -c $(SRCDIR)/globals_m.f90
	$(FC) $(FCFLAGS) -I$(EXE1OBJDIR) -o $(EXE1OBJDIR)/topology_m.o -c $(SRCDIR)/topology_m.f90
	$(FC) $(FCFLAGS) -I$(EXE1OBJDIR) -o $(EXE1OBJDIR)/STL_format_m.o -c $(SRCDIR)/STL_format_m.f90
	$(FC) $(FCFLAGS) -I$(EXE1OBJDIR) -o $(EXE1OBJDIR)/STL_read_m.o -c $(SRCDIR)/STL_read_m.f90
	$(FC) $(FCFLAGS) -I$(EXE1OBJDIR) -o $(EXE1OBJDIR)/STL_write_m.o -c $(SRCDIR)/STL_write_m.f90
	$(FC) $(FCFLAGS) -I$(EXE1OBJDIR) -o $(EXE1OBJDIR)/STL_bend.o -c $(SRCDIR)/STL_bend.f90
	$(FC) $(FCFLAGS) -I$(EXE1OBJDIR) -o $(EXE1) $(EXE1OBJDIR)/*.o

clean:
	rm -f $(EXE1OBJDIR)/*.o $(MODDIR)/*.mod $(EXE1)
