all:
	lazbuild --lazarusdir=/usr/lib/lazarus/2.0.10 --build-mode=Release bookthief.lpr

windows:
	lazbuild --lazarusdir=/usr/lib/lazarus/2.0.10 --build-mode=Release --operating-system=win64 bookthief.lpr
	
install:
	install -m 0755 bookthief /usr/bin

