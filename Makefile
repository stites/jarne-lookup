
.PHONY: install

install: ${HOME}/.config/rofi/scripts/jarne-lookup.sh ${HOME}/.local/bin/jarne-lookup

${HOME}/.config/rofi/scripts/jarne-lookup.sh:
	ln -fs ${PWD}/rofi.sh ${HOME}/.config/rofi/scripts/jarne-lookup.sh

${HOME}/.local/bin/jarne-lookup:
	nix build -L
	cp -f ./result/bin/jarne-lookup ${HOME}/.local/bin/jarne-lookup:

