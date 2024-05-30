ROOT_DIR := $(dir $(realpath $(lastword $(MAKEFILE_LIST))))

.PHONY: all
all:
	home-manager switch --verbose --print-build-logs --flake '$(ROOT_DIR)'

.PHONY: up
up:
	@if ! git diff-index --quiet HEAD --; then echo 'commit changes first'; exit 1; fi
	nix flake update
	git diff-index --quiet HEAD -- && git commit -am 'nix flake update'
