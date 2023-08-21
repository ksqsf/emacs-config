#!/bin/bash

# Make a tarball that I can copy everywhere
# The output is mostly self-contained

tar cjvf ksqsf-emacs.tar.bz2 early-init.el init.el modules elpa etc custom.el lisp
