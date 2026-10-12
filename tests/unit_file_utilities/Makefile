# ------------------------------------------------------------------------------
# smk, the smart make (https://github.com/LionelDraghi/smk)
# SPDX-FileCopyrightText: 2018, 2019, 2026 Lionel Draghi
# SPDX-License-Identifier: Apache-2.0
# ------------------------------------------------------------------------------


.PHONY : clean test_file_utilities

all: check doc

test_file_utilities:
	alr --non-interactive build --validation

check: test_file_utilities
	./test_file_utilities > results.md
	
doc:

clean:
	- alr clean
	- rm results.md
