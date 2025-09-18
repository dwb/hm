---
paths:
  - "*.el"
---

# Emacs Lisp Rules

* Strongly prefer breaking out functionality into new functions, unless doing so would result in an absurdly unwieldy argument list. But if that is the case, consider that the wider structure is not quite right.
* When naming functions and variables, have a good idea of whether they are private or public. Private symbols have two hyphens between package name and the rest of their name. Private names should be reserved for true implementation details. Emacs users like to be able to customise packages. Try and keep the private surface to true implementation details. If you follow the rule above about breaking functionality out into new functions, the public interface of a package can be pleasantly large, as functions are small and well-defined.
* Always use `if-let*`, `when-let*` `and-let*` instead of their non-asterisk `if-let`, `when-let`, `and-let` forms.
* Make good use of `(declare (pure t))`, `(declare (side-effect-free t))`, and `(declare (side-effect-free error-free))`. But be **EXTREMELY CAREFUL** to only declare functions as such if they do conform. Look up info nodes `(elisp) Declare Form` and `(elisp) Standard Properties` for authoritative documentation.
* Use `emacsclient` to look up documentation, explore the user's Emacs session, debug problems, etc.
