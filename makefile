# --- Compiler and Tools ---
CXX      ?= g++
FPC      ?= fpc

# --- Directories ---
SRCDIR   := src
BINDIR   := bin

# --- C++ (Liesel) ---
CXXFLAGS     ?= -Wall -Wextra -std=c++23 -O2 -s -MMD -MP
INCLUDEFLAGS := $(shell pkg-config --cflags poppler-cpp GraphicsMagick++)
LDFLAGS      := $(shell pkg-config --libs poppler-cpp GraphicsMagick++) -lhpdf

SRCS_LIESEL  := $(wildcard $(SRCDIR)/liesel/*.cpp)
OBJS_LIESEL  := $(patsubst $(SRCDIR)/liesel/%.cpp,$(BINDIR)/liesel-objs/%.o,$(SRCS_LIESEL))
LIESEL       := $(BINDIR)/liesel

# --- Pascal (BookThief) ---
FPCFLAGS     ?= -MObjFPC -Scghi -CX -Cg -O3 -Xs -XX -l -vewnhibq -dLCL -dLCLgtk2
FPCINCLUDES  := -Fi$(BINDIR)/lib/x86_64-linux \
	-Fu$(SRCDIR)/bookthief \
	-Fu/usr/lib/lazarus/4.0/lcl/units/x86_64-linux/gtk2 \
	-Fu/usr/lib/lazarus/4.0/lcl/units/x86_64-linux \
	-Fu/usr/lib/lazarus/4.0/components/freetype/lib/x86_64-linux \
	-Fu/usr/lib/lazarus/4.0/components/lazutils/lib/x86_64-linux \
	-Fu/usr/lib/lazarus/4.0/packager/units/x86_64-linux \
	-FU$(BINDIR)/lib/x86_64-linux

BOOKTHIEF    := $(BINDIR)/bookthief

# --- Version Info ---
PARSECHANGELOG := $(shell command -v dpkg-parsechangelog 2> /dev/null)
ifdef PARSECHANGELOG
  VERSION := $(shell dpkg-parsechangelog -l debian/changelog --show-field version)
  YEAR    := $(shell date +%Y -d@$(shell dpkg-parsechangelog -l debian/changelog --show-field timestamp))
else
  VERSION := 12.0.0
  YEAR    := 2026
endif

# --- Targets ---
all: $(LIESEL) $(BOOKTHIEF)

# --- Liesel Build ---
$(LIESEL): $(OBJS_LIESEL)
	@mkdir -p "$(BINDIR)"
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)
	@echo "Built Liesel version $(VERSION)"

$(BINDIR)/liesel-objs/%.o: $(SRCDIR)/liesel/%.cpp $(SRCDIR)/common/version.h
	@mkdir -p "$(BINDIR)/liesel-objs"
	$(CXX) $(CXXFLAGS) $(INCLUDEFLAGS) -c $< -o $@

# --- BookThief Build ---
$(BOOKTHIEF): $(SRCDIR)/bookthief/bookthief.lpr $(SRCDIR)/bookthief/*.pas $(SRCDIR)/bookthief/*.lfm
	@mkdir -p "$(BINDIR)/lib/x86_64-linux"
	$(FPC) $(FPCFLAGS) $(FPCINCLUDES) -o$@ $<
	@echo "Built BookThief version $(VERSION)"

# --- Version Header ---
$(SRCDIR)/common/version.h: debian/changelog
	@echo "#define VERSION \"$(VERSION)\"" > $@
	@echo "#define COPYRIGHT_YEAR \"$(YEAR)\"" >> $@
	@echo "Updated version information to $(VERSION) ($(YEAR))"

# --- Clean ---
clean:
	$(RM) -r "$(BINDIR)/"*

# --- Dependencies ---
-include $(BINDIR)/*.d
-include $(BINDIR)/liesel-objs/*.d

.PHONY: all clean
