ROOT_DIR := $(dir $(realpath $(lastword $(MAKEFILE_LIST))))

.PHONY: all
all:
	@[[ $$(uname) == Darwin ]] && \
		[[ $$(/usr/libexec/PlistBuddy -c "print :Accounts:0:AccountID" ~/Library/Preferences/MobileMeAccounts.plist) == d@dani.cool ]] && \
		echo "update from nix-dani on your main machine please"; false
	home-manager switch --verbose --print-build-logs --flake '$(ROOT_DIR)'

.PHONY: up
up:
	@if ! git diff-index --quiet HEAD --; then echo 'commit changes first'; exit 1; fi
	nix flake update
	git diff-index --quiet HEAD -- || git commit -am 'nix flake update'
	nix run .#nushellDefaultConfig > "$(ROOT_DIR)/conf/default_config.nu"
	nix run .#nushellDefaultEnv > "$(ROOT_DIR)/conf/default_env.nu"
	git diff-index --quiet HEAD -- || git commit -am 'nushell: default config update'
