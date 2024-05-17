ROOT_DIR := $(dir $(realpath $(lastword $(MAKEFILE_LIST))))

.PHONY: all
all:
	home-manager switch --verbose --print-build-logs --flake '$(ROOT_DIR)'
