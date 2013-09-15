SHELL := /usr/bin/env bash
DEPS  := vendor/simpleirc

default: build

all: clean lint install

build:
	cabal-dev build

install: add-sources
	cabal-dev install -j

clean:
	-rm -rf vendor tmp dist cabal-dev
	cabal-dev clean

lint:
	hlint src

add-sources: $(DEPS)
	find -L vendor -name "*.cabal" -exec dirname {} \; | xargs cabal-dev add-source

vendor/simpleirc:
	git clone git@github.com:dom96/SimpleIRC.git $@
