### epilogue.make --- Epilogue Makefile

## Copyright (C) 2024 Didier Verna

## Author: Didier Verna <didier@didierverna.net>

## This file is part of TFM-VALIDATE.

## Permission to use, copy, modify, and distribute this software for any
## purpose with or without fee is hereby granted, provided that the above
## copyright notice and this permission notice appear in all copies.

## THIS SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
## WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
## MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
## ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
## WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
## ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
## OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


### Commentary:

## Contents management by FCM version 0.1.


### Code:

$(TOP_DIR)/make/version.make: \
  $(TOP_DIR)/make/epilogue.make $(TOP_DIR)/make/version.cl \
  $(TOP_DIR)/setup/src/version.lisp
	$($(LISP)_PATH) $($(LISP)_LOAD) $(TOP_DIR)/make/version.cl \
	  | tail -3 > $@.new
	mv $@.new $@

### epilogue.make ends here
