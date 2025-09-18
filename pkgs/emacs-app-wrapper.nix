/*
  A replacement for nixpkgs' `emacs.pkgs.withPackages`, differing only in how it
  produces `Applications/Emacs.app` on Darwin.

  Vendored from
  https://github.com/NixOS/nixpkgs/blob/2f364bf6308286fc9665d8ef6c5796e014d006f8/pkgs/applications/editors/emacs/build-support/wrapper.nix
  Everything up to and including the `$out/bin` loop is unchanged; re-sync that
  part when bumping nixpkgs. `wrapper.sh` and `mk-wrapper-subdirs.el` are used
  from the nixpkgs tree rather than copied.

  Why: upstream's Emacs.app is a stub bundle whose `Contents/MacOS/Emacs` is a
  wrapper that `exec`s the *unwrapped* emacs derivation's own Emacs.app. The
  process therefore runs a bundle LaunchServices never launched, and neither
  bundle is signed as a bundle (only the Mach-O, ad-hoc by the linker). That
  makes `[UNUserNotificationCenter currentNotificationCenter]` raise
  NSInternalInconsistencyException, so `ns-notification-available-p` (see
  emacs-macos-notifications.patch) is always nil.

  Here the bundle instead contains the real Emacs binary as its
  `CFBundleExecutable`, and the environment the wrapper script would have
  exported goes into an `LSEnvironment` dict in `Info.plist`. Nothing `exec`s
  anything, so the bundle can be code signed as a unit — done at home-manager
  activation time, since a Nix build cannot reach `/usr/bin/codesign`. See
  `home.activation.signEmacsApp` in ../emacs.nix.

  `LSEnvironment` only applies to LaunchServices launches, which is exactly the
  GUI case. Terminal use goes through `$out/bin/*`, which keep upstream's
  wrapper script and its EMACSLOADPATH merge semantics.
*/

{
  lib,
  lndir,
  makeBinaryWrapper,
  path,
  python3,
  runCommand,
}:
self:
let
  inherit (self) emacs;
  withNativeCompilation = emacs.withNativeCompilation or false;
  withTreeSitter = emacs.withTreeSitter or false;

  buildSupport = "${path}/pkgs/applications/editors/emacs/build-support";
in
packagesFun: # packages explicitly requested by the user
let
  explicitRequires = if lib.isFunction packagesFun then packagesFun self else packagesFun;
in
runCommand (lib.appendToName "with-packages" emacs).name
  {
    inherit emacs explicitRequires;
    nativeBuildInputs = [
      emacs
      lndir
      makeBinaryWrapper
    ]
    ++ lib.optional emacs.stdenv.hostPlatform.isDarwin python3;

    preferLocalBuild = true;
    allowSubstitutes = false;

    # Store all paths we want to add to emacs here, so that we only need to add
    # one path to the load lists
    deps =
      runCommand "emacs-packages-deps"
        (
          {
            inherit explicitRequires lndir emacs;
          }
          // lib.optionalAttrs withNativeCompilation {
            inherit (emacs) LIBRARY_PATH;
          }
        )
        ''
          findInputsOld() {
            local pkg="$1"; shift
            local var="$1"; shift
            local propagatedBuildInputsFiles=("$@")

            # TODO(@Ericson2314): Restore using associative array once Darwin
            # nix-shell doesn't use impure bash. This should replace the O(n)
            # case with an O(1) hash map lookup, assuming bash is implemented
            # well :D.
            local varSlice="$var[*]"
            # ''${..-} to hack around old bash empty array problem
            case " ''${!varSlice-} " in
                *" $pkg "*) return 0 ;;
            esac
            unset -v varSlice

            eval "$var"'+=("$pkg")'

            if ! [ -e "$pkg" ]; then
                echo "build input $pkg does not exist" >&2
                exit 1
            fi

            local file
            for file in "''${propagatedBuildInputsFiles[@]}"; do
                file="$pkg/nix-support/$file"
                [[ -f "$file" ]] || continue

                local pkgNext
                for pkgNext in $(< "$file"); do
                    findInputsOld "$pkgNext" "$var" "''${propagatedBuildInputsFiles[@]}"
                done
            done
          }
          mkdir -p $out/bin
          mkdir -p $out/share/emacs/site-lisp
          ${lib.optionalString withNativeCompilation ''
            mkdir -p $out/share/emacs/native-lisp
          ''}
          ${lib.optionalString withTreeSitter ''
            mkdir -p $out/lib
          ''}

          local requires
          for pkg in $explicitRequires; do
            findInputsOld $pkg requires propagated-user-env-packages
          done
          # requires now holds all requested packages and their transitive dependencies

          linkPath() {
            local pkg=$1
            local origin_path=$2
            local dest_path=$3

            # Add the path to the search path list, but only if it exists.
            # Executables in /bin are linked by their resolved paths in case they are
            # relative symlinks (which break when 'lndir'ed as is);
            # see https://github.com/NixOS/nixpkgs/issues/395442
            if [[ -d "$pkg/$origin_path" ]]; then
              case "$origin_path" in
                bin)
                  for exe in $pkg/$origin_path/*; do
                    ln -s "$(realpath "$exe")" "$out/$dest_path/$(basename "$exe")"
                  done
                  ;;
                *) $lndir/bin/lndir -silent "$pkg/$origin_path" "$out/$dest_path";;
              esac
            fi
          }

          linkEmacsPackage() {
            linkPath "$1" "bin" "bin"
            linkPath "$1" "share/emacs/site-lisp" "share/emacs/site-lisp"
            ${lib.optionalString withNativeCompilation ''
              linkPath "$1" "share/emacs/native-lisp" "share/emacs/native-lisp"
            ''}
            ${lib.optionalString withTreeSitter ''
              linkPath "$1" "lib" "lib"
            ''}
          }

          # Iterate over the array of inputs (avoiding nix's own interpolation)
          for pkg in "''${requires[@]}"; do
            linkEmacsPackage $pkg
          done

          siteStart="$out/share/emacs/site-lisp/site-start.el"
          siteStartByteCompiled="$siteStart"c
          subdirs="$out/share/emacs/site-lisp/subdirs.el"
          subdirsByteCompiled="$subdirs"c

          # A dependency may have brought the original siteStart or subdirs, delete
          # it and create our own
          # Begin the new site-start.el by loading the original, which sets some
          # NixOS-specific paths. Paths are searched in the reverse of the order
          # they are specified in, so user and system profile paths are searched last.
          rm -f $siteStart $siteStartByteCompiled $subdirs $subdirsByteCompiled
          cat >"$siteStart" <<EOF
          ;;; -*- lexical-binding: t -*-
          (load "$emacs/share/emacs/site-lisp/site-start" nil t)
          ;; "$out/share/emacs/site-lisp" is added to load-path in wrapper.sh
          ;; "$out/share/emacs/native-lisp" is added to native-comp-eln-load-path in wrapper.sh
          (add-to-list 'exec-path "$out/bin")
          ;; Also expose extra package binaries via PATH so that subprocesses
          ;; which rebuild their environment from PATH (e.g. direnv/envrc) can
          ;; still find them. See https://github.com/purcell/envrc/issues/9
          (let ((deps-bin "$out/bin")
                (current-path (or (getenv "PATH") "")))
            (unless (member deps-bin (split-string current-path path-separator))
              (setenv "PATH" (concat deps-bin path-separator current-path))))
          ${lib.optionalString withTreeSitter ''
            (add-to-list 'treesit-extra-load-path "$out/lib/")
          ''}
          EOF

          # Generate a subdirs.el that statically adds all subdirectories to load-path.
          cat >"$subdirs" <<EOF
          ;;; -*- lexical-binding: t -*-
          EOF
          $emacs/bin/emacs \
            --batch \
            --load ${buildSupport}/mk-wrapper-subdirs.el \
            --eval "(prin1 (macroexpand-1 '(mk-subdirs-expr \"$out/share/emacs/site-lisp\")))" \
            >> "$subdirs"

          # Byte-compiling improves start-up time only slightly, but costs nothing.
          $emacs/bin/emacs --batch -f batch-byte-compile "$siteStart" "$subdirs"

          ${lib.optionalString withNativeCompilation ''
            $emacs/bin/emacs --batch \
              --eval "(add-to-list 'native-comp-eln-load-path \"$out/share/emacs/native-lisp/\")" \
              -f batch-native-compile "$siteStart" "$subdirs"
          ''}
        '';

    inherit (emacs) meta;
  }
  ''
    mkdir -p "$out/bin"

    # Wrap emacs and friends so they find our site-start.el before the original.
    for prog in $emacs/bin/*; do # */
      local progname=$(basename "$prog")
      rm -f "$out/bin/$progname"

      substitute ${buildSupport}/wrapper.sh $out/bin/$progname \
        --subst-var-by bash ${emacs.stdenv.shell} \
        --subst-var-by wrapperSiteLisp "$deps/share/emacs/site-lisp" \
        --subst-var-by wrapperSiteLispNative "$deps/share/emacs/native-lisp" \
        --subst-var-by wrapperInvocationDirectory "$out/bin/" \
        --subst-var-by wrapperInvocationName "$progname" \
        --subst-var prog
      chmod +x $out/bin/$progname
      # Create a “NOP” binary wrapper for the pure sake of it becoming a
      # non-shebang, actual binary. See the makeBinaryWrapper docs for rationale
      # (summary: it allows you to use emacs as a shebang itself on Darwin,
      # e.g. #!$ {emacs}/bin/emacs --script)
      wrapProgramBinary $out/bin/$progname
    done

    # Build a single macOS app bundle around the real Emacs binary, ready to be
    # code signed as a unit. Diverges from upstream; see the header comment.
    if [ -d "$emacs/Applications/Emacs.app" ]; then
      app=$out/Applications/Emacs.app
      src=$emacs/Applications/Emacs.app

      mkdir -p "$app/Contents/MacOS"
      cp "$src/Contents/PkgInfo" "$app/Contents/PkgInfo"
      cp -R "$src/Contents/Resources" "$app/Contents/Resources"
      # A copy, not a symlink: codesign rewrites the executable in place.
      cp "$src/Contents/MacOS/Emacs" "$app/Contents/MacOS/Emacs"
      # Deliberately not recreating upstream's Contents/native-lisp symlink: it
      # points outside the bundle, which makes `codesign --verify --strict` fail
      # with "invalid destination for symbolic link in bundle". eln files are
      # found via EMACSNATIVELOADPATH below and Emacs' compiled-in paths.

      python3 ${./emacs-app-lsenvironment.py} \
        "$src/Contents/Info.plist" "$app/Contents/Info.plist"
    fi

    mkdir -p $out/share
    # Link icons and desktop files into place
    for dir in applications icons info man; do
      ln -s $emacs/share/$dir $out/share/$dir
    done
  ''
