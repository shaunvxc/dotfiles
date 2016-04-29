SHELL := /bin/bash

all:
	@echo "gmahnin"
	cd dotfiles
	cp .emacs ~/
	cp -r .emacs.d ~/z
