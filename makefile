# --- Compiler and Tools ---
CXX ?= g++
FPC ?= fpc

# --- Directories ---
SRCDIR := src
BINDIR := bin

# --- Version Info ---
PARSECHANGELOG := $(shell command -v dpkg-parsechangelog 2> /dev/null)
ifdef PARSECHANGELOG
  VERSION := $(shell dpkg-parsechangelog -l debian/changelog --show-field version)
  YEAR    := $(shell date +%Y -d@$(shell dpkg-parsechangelog -l debian/changelog --show-field timestamp))
else
  VERSION := 12.0.0
  YEAR    := 2026
endif

MAJORVERSION := $(shell echo $(VERSION) | cut -d. -f1)
MINORVERSION := $(shell echo $(VERSION) | cut -d. -f2)
PATCHVERSION := $(shell echo $(VERSION) | cut -d. -f3)

# --- C++ (Liesel) ---
CXXFLAGS     := -Wall -Wextra -std=c++23 -O2 -s -MMD -MP
INCLUDEFLAGS := $(shell pkg-config --cflags poppler-cpp GraphicsMagick++)
LDFLAGS      := $(shell pkg-config --libs poppler-cpp GraphicsMagick++) -lhpdf

# Build shared objects with PIC
CXXFLAGS_PIC := $(CXXFLAGS) -fPIC

SRCS_LIESEL      := $(wildcard $(SRCDIR)/liesel/*.cpp)
SRCS_LIESEL_CORE := $(filter-out $(SRCDIR)/liesel/main.cpp,$(SRCS_LIESEL))
SRCS_LIESEL_ABI  := $(wildcard $(SRCDIR)/liesel/abi/*.cpp)

# CLI (includes main.cpp)
OBJS_LIESEL  := $(patsubst $(SRCDIR)/liesel/%.cpp,$(BINDIR)/liesel-objs/%.o,$(SRCS_LIESEL))
LIESEL       := $(BINDIR)/liesel

# Shared library (core + ABI, no main.cpp)
OBJS_LIESEL_CORE := $(patsubst $(SRCDIR)/liesel/%.cpp,$(BINDIR)/liesel-lib-objs/%.o,$(SRCS_LIESEL_CORE))
OBJS_LIESEL_ABI  := $(patsubst $(SRCDIR)/liesel/abi/%.cpp,$(BINDIR)/liesel-lib-objs/abi/%.o,$(SRCS_LIESEL_ABI))
LIESEL_SOVERSION := 0
LIESEL_REALNAME  := $(BINDIR)/libliesel.so.$(LIESEL_SOVERSION).$(VERSION)
LIESEL_SO        := $(BINDIR)/libliesel.so.$(LIESEL_SOVERSION)

LIESEL_SO_BASENAME := $(shell basename $(LIESEL_REALNAME))

# --- Pascal (BookThief) ---
FPC_LINKFLAGS ?= -k--as-needed -k-pie -k-z -krelro -k-z -know
FPCFLAGS      ?= -MObjFPC -Scghi -CX -Cg -O3 -Xs -XX -l -vewnhibq -dLCL -dLCLgtk2 $(FPC_LINKFLAGS)
FPCINCLUDES   := -Fi$(BINDIR)/lib/x86_64-linux \
	-Fu$(SRCDIR)/bookthief \
	-Fu/usr/lib/lazarus/4.0/lcl/units/x86_64-linux/gtk2 \
	-Fu/usr/lib/lazarus/4.0/lcl/units/x86_64-linux \
	-Fu/usr/lib/lazarus/4.0/components/freetype/lib/x86_64-linux \
	-Fu/usr/lib/lazarus/4.0/components/lazutils/lib/x86_64-linux \
	-Fu/usr/lib/lazarus/4.0/packager/units/x86_64-linux \
	-FU$(BINDIR)/lib/x86_64-linux

BOOKTHIEF    := $(BINDIR)/bookthief

# --- Targets ---
all: $(LIESEL) $(LIESEL_SO) $(BOOKTHIEF) debian/liesel.1 debian/bookthief.1

# --- Liesel CLI Build ---
$(LIESEL): $(OBJS_LIESEL)
	@mkdir -p "$(BINDIR)"
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)
	@echo "Built Liesel (CLI) version $(VERSION)"

$(BINDIR)/liesel-objs/%.o: $(SRCDIR)/liesel/%.cpp $(SRCDIR)/liesel/version.h
	@mkdir -p "$(BINDIR)/liesel-objs"
	$(CXX) $(CXXFLAGS) $(INCLUDEFLAGS) -c $< -o $@

# --- Liesel Shared Library Build ---
$(LIESEL_SO): $(LIESEL_REALNAME)
	ln -sf $(LIESEL_SO_BASENAME) $@

$(LIESEL_REALNAME): $(OBJS_LIESEL_CORE) $(OBJS_LIESEL_ABI)
	@mkdir -p "$(BINDIR)"
	$(CXX) -shared -Wl,-soname,$(LIESEL_SO_BASENAME) -o $@ $^ $(LDFLAGS)
	@echo "Built $(LIESEL_SO_BASENAME)"

$(BINDIR)/liesel-lib-objs/%.o: $(SRCDIR)/liesel/%.cpp $(SRCDIR)/liesel/version.h
	@mkdir -p "$(BINDIR)/liesel-lib-objs"
	$(CXX) $(CXXFLAGS_PIC) $(INCLUDEFLAGS) -c $< -o $@

$(BINDIR)/liesel-lib-objs/abi/%.o: $(SRCDIR)/liesel/abi/%.cpp $(SRCDIR)/liesel/version.h
	@mkdir -p "$(BINDIR)/liesel-lib-objs/abi"
	$(CXX) $(CXXFLAGS_PIC) $(INCLUDEFLAGS) -c $< -o $@

# --- Version Headers ---
version-headers: $(SRCDIR)/liesel/version.h $(SRCDIR)/bookthief/VersionInfoUnit.pas
	@echo "Version: $(VERSION) ($(YEAR))"

$(SRCDIR)/liesel/version.h: debian/changelog
	@echo "#define VERSION \"$(VERSION)\"" > $@
	@echo "#define MAJOR_VERSION $(MAJORVERSION)" >> $@
	@echo "#define MINOR_VERSION $(MINORVERSION)" >> $@
	@echo "#define PATCH_VERSION $(PATCHVERSION)" >> $@
	@echo "#define COPYRIGHT_YEAR \"$(YEAR)\"" >> $@

$(SRCDIR)/bookthief/VersionInfoUnit.pas: debian/changelog
	@echo "unit VersionInfoUnit;" > $@
	@echo "" >> $@
	@echo "interface" >> $@
	@echo "" >> $@
	@echo "const" >> $@
	@echo "	VERSION = '$(VERSION)';" >> $@
	@echo "	MAJOR_VERSION = $(MAJORVERSION);" >> $@
	@echo "	MINOR_VERSION = $(MINORVERSION);" >> $@
	@echo "	PATCH_VERSION = $(PATCHVERSION);" >> $@
	@echo "	COPYRIGHT_YEAR = '$(YEAR)';" >> $@
	@echo "" >> $@
	@echo "implementation" >> $@
	@echo "end." >> $@

# --- BookThief Build ---
$(BOOKTHIEF): $(SRCDIR)/bookthief/bookthief.lpr $(SRCDIR)/bookthief/*.pas $(SRCDIR)/bookthief/*.lfm $(LIESEL_SO) $(SRCDIR)/bookthief/VersionInfoUnit.pas
	@mkdir -p "$(BINDIR)/lib/x86_64-linux"
	$(FPC) $(FPCFLAGS) $(FPCINCLUDES) -o$@ $<
	@echo "Built BookThief version $(VERSION)"

# --- Manuals ---
manuals: debian/liesel.1 debian/bookthief.1
	@echo "Built manual pages"

debian/liesel.1: src/manuals/liesel.md
	@cp $< debian/liesel.md
	@sed -i '1s/^/% liesel(1) Version $(VERSION) | Manual for Liesel\n/' debian/liesel.md
	@pandoc --standalone --to man debian/liesel.md -o $@
	@rm -f debian/liesel.md

debian/bookthief.1: src/manuals/bookthief.md
	@cp $< debian/bookthief.md
	@sed -i '1s/^/% bookthief(1) Version $(VERSION) | Manual for BookThief\n/' debian/bookthief.md
	@pandoc --standalone --to man debian/bookthief.md -o $@
	@rm -f debian/bookthief.md

# --- Clean ---
clean:
	$(RM) -r "$(BINDIR)/"*
	$(RM) $(SRCDIR)/liesel/version.h
	$(RM) $(SRCDIR)/bookthief/VersionInfoUnit.pas
	$(RM) debian/liesel.1 debian/bookthief.1
	$(RM) debian/liesel.md debian/bookthief.md

# --- Dependencies ---
-include $(BINDIR)/*.d
-include $(BINDIR)/liesel-objs/*.d
-include $(BINDIR)/liesel-lib-objs/*.d
-include $(BINDIR)/liesel-lib-objs/abi/*.d

.PHONY: all clean version-headers manuals
