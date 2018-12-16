# ------------------------------------------------------------------------------
# smk, the smart make (http://lionel.draghi.free.fr/smk/)
#  © 2018 Lionel Draghi <lionel.draghi@free.fr>
# SPDX-License-Identifier: APSL-2.0
# ------------------------------------------------------------------------------
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# http://www.apache.org/licenses/LICENSE-2.0
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# ------------------------------------------------------------------------------

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