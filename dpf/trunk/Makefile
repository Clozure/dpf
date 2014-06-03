CONTENTS="build/Picture Window.app/Contents/"
BINARY="$(CONTENTS)/MacOS/Picture Window"

%.nib : en.lproj/%.xib
	ibtool --errors --warnings --notices --output-format human-readable-text --compile $@ $<

NIBS=MainMenu.nib dpf-preferences.nib help.nib

build:  nibs 
	mkdir -p $(CONTENTS)
	cp $(NIBS) $(CONTENTS)
	ccl --no-init --load build.lisp

nibs: $(NIBS)
	

clean:
	rm -f $(NIBS)
