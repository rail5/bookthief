all:
	g++ -I/usr/lib/x86_64-linux-gnu/wx/include/gtk3-unicode-3.0 -I/usr/include/wx-3.0 -D_FILE_OFFSET_BITS=64 -DWXUSINGDLL -D__WXGTK__ -pthread -Wno-unused-local-typedefs -Wall -O2 -I/usr/include/ImageMagick-6 -I/usr/include/x86_64-linux-gnu/ImageMagick-6 -c bookthiefApp.cpp -o obj/Release/bookthiefApp.o
	g++ -I/usr/lib/x86_64-linux-gnu/wx/include/gtk3-unicode-3.0 -I/usr/include/wx-3.0 -D_FILE_OFFSET_BITS=64 -DWXUSINGDLL -D__WXGTK__ -pthread -Wno-unused-local-typedefs -Wall -O2 -I/usr/include/ImageMagick-6 -I/usr/include/x86_64-linux-gnu/ImageMagick-6 -c bookthiefMain.cpp -o obj/Release/bookthiefMain.o
	g++  -o bookthief obj/Release/bookthiefApp.o obj/Release/bookthiefMain.o  -s -L/usr/lib/x86_64-linux-gnu -pthread   -lwx_gtk3u_xrc-3.0 -lwx_gtk3u_html-3.0 -lwx_gtk3u_qa-3.0 -lwx_gtk3u_adv-3.0 -lwx_gtk3u_core-3.0 -lwx_baseu_xml-3.0 -lwx_baseu_net-3.0 -lwx_baseu-3.0   

install:
	install -m 0755 bookthief /usr/bin
	install -m 0755 BookThief.desktop /usr/share/applications
