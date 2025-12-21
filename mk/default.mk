nix_modules_dir = $(XDG_CONFIG_HOME)/modules
emacs_package_module = $(nix_modules_dir)/emacsPackages
EMACS = $(emacs_package_module)/result/bin/emacs
EMACSQ = $(EMACS) -Q

RM = rm -f
