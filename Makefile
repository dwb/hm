ROOT_DIR := $(dir $(realpath $(lastword $(MAKEFILE_LIST))))

.PHONY: all
all:
	home-manager switch --flake '$(ROOT_DIR)'
