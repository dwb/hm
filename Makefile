ROOT_DIR := $(dir $(realpath $(lastword $(MAKEFILE_LIST))))

.PHONY: all
all:
	home-manager switch --verbose --print-build-logs --flake '$(ROOT_DIR)'

.PHONY: up
	nix flake update
	git commit -am 'nix flake update'
