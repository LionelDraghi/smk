# ------------------------------------------------------------------------------
# smk, the smart make (https://github.com/LionelDraghi/smk)
# SPDX-FileCopyrightText: 2018, 2026 Lionel Draghi
# SPDX-License-Identifier: Apache-2.0
# ------------------------------------------------------------------------------

# NB: the .ads specs are generated from the Linux uapi headers
# (g++ -c -fdump-ada-spec): they keep the kernel headers license notice,
# and must not be given the project license or the project header.

all: build
	
linux_ptrace_h.ads:
	g++ -c -fdump-ada-spec -C /usr/include/linux/ptrace.h

build:linux_ptrace_h.ads minimal_strace.c
	gcc -c *.ads
	gcc -std=c99 -Wall -Wextra -O3 -g3 minimal_strace.c -o minimal_strace

clean:
	rm *.o *.ali

mrproper: clean
	rm asm_generic_int_ll64_h.ads linux_types_h.ads asm_generic_posix_types_h.ads x86_64_linux_gnu_asm_posix_types_64_h.ads linux_posix_types_h.ads x86_64_linux_gnu_asm_ptrace_h.ads linux_ptrace_h.ads