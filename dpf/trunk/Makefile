CCL=ccl64
CONTENTS="build/Picture Window.app/Contents/"
RESOURCES=$(CONTENTS)/Resources
BINARY="$(CONTENTS)/MacOS/Picture Window"

%.nib : en.lproj/%.xib
	ibtool --errors --warnings --notices --output-format human-readable-text --compile $@ $<

NIBS=MainMenu.nib dpf-preferences.nib help.nib

build:  nibs 
	iconutil -c icns app.iconset
	mkdir -p $(RESOURCES)
	cp $(NIBS) $(RESOURCES)
	$(CCL) --no-init --load build.lisp

nibs: $(NIBS)

clean:
	rm -f $(NIBS) app.icns

dist-clean: clean
	rm -rf build
