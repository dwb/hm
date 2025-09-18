"""Copy Emacs.app's Info.plist, adding the emacsWithPackages environment.

Used by emacs-app-wrapper.nix. Reads $deps and $out from the build environment;
takes the source and destination plist paths, then the partial Info.plist
actool emitted for the replacement app icon (see ../prebuild-emacs-icon.nu), as
arguments. The partial plist supplies CFBundleIconName, which is what makes
macOS read the Icon Composer rendering out of Resources/Assets.car.

LSEnvironment cannot merge with an inherited value the way the upstream wrapper
script does, but a LaunchServices-launched app inherits no EMACSLOADPATH, so
these literal values are what that merge would have produced. The trailing
empty element (the bare ":") is what tells Emacs to append its built-in path.

invocation{Directory,Name} point at $out/bin/emacs rather than at the bundle, so
that anything Emacs re-invokes through
(expand-file-name invocation-name invocation-directory) -- asynchronous native
compilation, for one -- gets a fully set-up Emacs.
"""

import os
import plistlib
import sys

src, dst, icon = sys.argv[1:4]
deps, out = os.environ["deps"], os.environ["out"]

with open(src, "rb") as f:
    plist = plistlib.load(f)

with open(icon, "rb") as f:
    plist.update(plistlib.load(f))

plist["LSEnvironment"] = {
    "EMACSLOADPATH": deps + "/share/emacs/site-lisp:",
    "EMACSNATIVELOADPATH": deps + "/share/emacs/native-lisp:",
    "emacsWithPackages_siteLisp": deps + "/share/emacs/site-lisp",
    "emacsWithPackages_siteLispNative": deps + "/share/emacs/native-lisp",
    "emacsWithPackages_invocationDirectory": out + "/bin/",
    "emacsWithPackages_invocationName": "emacs",
}

with open(dst, "wb") as f:
    plistlib.dump(plist, f)
