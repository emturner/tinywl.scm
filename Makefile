WAYLAND_PROTOCOLS=$(shell pkg-config --variable=pkgdatadir wayland-protocols)
WAYLAND_SCANNER=$(shell pkg-config --variable=wayland_scanner wayland-scanner)
LIBS=\
	 $(shell pkg-config --cflags --libs wlroots) \
	 $(shell pkg-config --cflags --libs wayland-server) \
	 $(shell pkg-config --cflags --libs xkbcommon)

# wayland-scanner is a tool which generates C headers and rigging for Wayland
# protocols, which are specified in XML. wlroots requires you to rig these up
# to your build system yourself and provide them in the include path.
xdg-shell-protocol.h:
	$(WAYLAND_SCANNER) server-header \
		$(WAYLAND_PROTOCOLS)/stable/xdg-shell/xdg-shell.xml $@

xdg-shell-protocol.c: xdg-shell-protocol.h
	$(WAYLAND_SCANNER) private-code \
		$(WAYLAND_PROTOCOLS)/stable/xdg-shell/xdg-shell.xml $@

tinywl: tinywl.c xdg-shell-protocol.h xdg-shell-protocol.c
	$(CC) $(CFLAGS) \
		-g -Werror -I. \
		-DWLR_USE_UNSTABLE \
		-o $@ $< \
		$(LIBS)

check: tinywl.scm wayland-server-core.scm wayland-server-protocol.scm \
		emturner/util.scm \
		wayland/dylib.scm wayland/util.scm \
		wlr/types/wlr-output.scm
	GUILE_EXTENSIONS_PATH=$(GUIX_ENVIRONMENT)/lib:$(GUILE_EXTENSIONS_PATH) \
		guile -L . -c '(use-modules (tinywl)) (check)'

clean:
	rm -f tinywl xdg-shell-protocol.h xdg-shell-protocol.c

.DEFAULT_GOAL=check
.PHONY: clean
