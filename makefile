CURRENT_PLATFORM=$(shell uname)

# Directory of your Lazarus installation (below is correct for default Debian installs)
LAZDIR=/usr/lib/lazarus/default

# Default directory for Lazarus on MacOS
MACLAZDIR=/Applications/Lazarus

# Intentionally left blank unless we detect we're building on MacOS
MACEXTRA=

all:
ifeq ($(CURRENT_PLATFORM),Darwin)
	$(eval LAZDIR := $(MACLAZDIR))
	$(eval MACEXTRA := --widgetset=cocoa)
endif
	$(LAZDIR)/lazbuild --lazarusdir=$(LAZDIR) $(MACEXTRA) --build-mode=Release bookthief.lpr

windows:
	lazbuild --lazarusdir=$(LAZDIR) \
	--build-mode=Release \
	--widgetset=win64 \
	--operating-system=win64 \
	--cpu=x86_64 \
	bookthief.lpr

macpkg:
	rm -rf ./bookthief.app/
	mkdir -p bookthief.app/Contents/MacOS
	mkdir -p bookthief.app/Contents/Resources
	echo "APPL????" > bookthief.app/Contents/PkgInfo
	cp Info.plist bookthief.app/Contents/
	cp bookthief bookthief.app/Contents/MacOS/
	cp liesel bookthief.app/Contents/MacOS/
	
install:
	install -m 0755 bookthief /usr/bin

