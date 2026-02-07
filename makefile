CXX = g++
CXXFLAGS = -Wall -Wextra -std=c++23 -O2 -s -MMD -MP
INCLUDEFLAGS = $(shell pkg-config --cflags poppler-cpp GraphicsMagick++)
LDFLAGS = $(shell pkg-config --libs poppler-cpp GraphicsMagick++) -lhpdf

# Can we parse d/changelog?
PARSECHANGELOG := $(shell command -v dpkg-parsechangelog 2> /dev/null)

ifdef PARSECHANGELOG
	VERSION = $(shell dpkg-parsechangelog -l debian/changelog --show-field version)
	YEAR = $(shell date +%Y -d@$(shell dpkg-parsechangelog -l debian/changelog --show-field timestamp))
else
	VERSION = 12.0.0
	YEAR = 2026
endif

SRCDIR = src
BINDIR = bin

LIESEL = $(BINDIR)/liesel
BOOKTHIEF = $(BINDIR)/bookthief

SRCS_LIESEL = $(wildcard $(SRCDIR)/liesel/*.cpp)
OBJS_LIESEL = $(patsubst $(SRCDIR)/liesel/%.cpp,$(BINDIR)/liesel-objs/%.o,$(SRCS_LIESEL))

all: $(LIESEL)

$(LIESEL): $(OBJS_LIESEL)
	@mkdir -p $(BINDIR)
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)
	@echo "Built Liesel version $(VERSION)"

$(BINDIR)/liesel-objs/%.o: $(SRCDIR)/liesel/%.cpp update-version-information
	@mkdir -p $(BINDIR)/liesel-objs
	$(CXX) $(CXXFLAGS) $(INCLUDEFLAGS) -c $< -o $@

update-version-information: debian/changelog
	@echo "#define VERSION \"$(VERSION)\"" > $(SRCDIR)/common/version.h
	@echo "#define COPYRIGHT_YEAR \"$(YEAR)\"" >> $(SRCDIR)/common/version.h
	@echo "Updated version information to $(VERSION) ($(YEAR))"

clean:
	rm -rf $(BINDIR)/*

-include $(BINDIR)/*.d

.PHONY: all clean update-version-information
