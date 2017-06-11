# emacs

Basic Emacs Setup.

# Install
(OSX & Linux)
- Save your previous .emacs.d directory (recommended).
- Download this emacs repository and rename it to **.emacs.d** (OSX e.g. `mv ~/Downloads/emacs-master ~/.emacs.d`).
- Move this newly added **.emacs.d** directory to your home directory.
- Load the emacs setup by calling the following code segment from your **.emacs** file.

``` elisp 
(load "~/.emacs.d/init.el")
```
- If you don't like it, revert to your previous setup by deleting the **.emacs.d**,
  `rm -r ~/.emacs.d`,  and renaming your **.emacs.d-saved** to **.emacs.d**,
  `mv ~/.emacs.d-saved ~/.emacs.d`.
