ROOT_DIR := $(dir $(realpath $(lastword $(MAKEFILE_LIST))))

.PHONY: all
all:
	@[[ $$(uname) == Darwin ]] && \
		[[ $$(/usr/libexec/PlistBuddy -c "print :Accounts:0:AccountID" ~/Library/Preferences/MobileMeAccounts.plist || echo) == d@dani.cool ]] && \
		{ echo "update from nix-dani on your main machine please"; false; }
	nix run nixpkgs#home-manager -- switch --verbose --print-build-logs --flake '$(ROOT_DIR)' --show-trace

.PHONY: up
up:
	@if ! git diff-index --quiet HEAD --; then echo 'commit changes first'; exit 1; fi
	nix flake update
	git diff-index --quiet HEAD -- || git commit -am 'nix flake update'
	git diff-index --quiet HEAD -- || git commit -am 'nushell: default config update'

.PHONY: link
link:
	mkdir -p ~/.config/zed
	ln -sf "${PWD}/conf/zed/"* ~/.config/zed/
