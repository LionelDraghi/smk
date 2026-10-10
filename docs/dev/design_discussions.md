<!-- omit from toc -->
# Design discussions

- [D1. Observing file accesses: linking to an API instead of parsing strace text output](#d1-observing-file-accesses-linking-to-an-api-instead-of-parsing-strace-text-output)

This document holds the design discussions: each entry records a
significant design subject, with its status - under discussion, or
arbitrated - and, once arbitrated, the decision, the alternatives that
were rejected and why. When the discussion happened elsewhere (a pull
request, a GitHub discussion, a forum thread), the entry references it
and adds only the complements needed to understand the subject as it
stands.

An arbitrated entry may be superseded by a later entry; it is then
updated with a reference to its replacement.

| Subject                                                                                          | Status           | References                                                       |
|--------------------------------------------------------------------------------------------------|------------------|------------------------------------------------------------------|
| [D1. Observing file accesses](#d1-observing-file-accesses-linking-to-an-api-instead-of-parsing-strace-text-output) | Under discussion | [design_notes.md](design_notes.md), [smk-runs-strace_analyzer.adb](../../src/smk-runs-strace_analyzer.adb) |

The table is sorted by status: the entries under discussion first.

## D1. Observing file accesses: linking to an API instead of parsing strace text output

Status: under discussion (October 2026)

### Goal

Today smk spawns `strace`, lets it write a text file, then re-parses
that text ([smk-runs-strace_analyzer.adb](../../src/smk-runs-strace_analyzer.adb)).
The goal of this discussion is to replace this by **linking smk against
an API or a library** that provides the file access information directly,
removing:

- the dependency on the strace output format (PID width, localization,
  argument layout, changes between versions);
- the cost of spawning an external tool and of serializing then
  re-parsing a text trace;
- the portability blocker: strace is Linux-only.

Note that the goal is *an API to link against*, not another external
tracer whose output would be parsed: swapping strace for another text
producer (fsatrace, kdump, dtruss...) would only move the format
dependency.

### D1.1 What smk actually needs

smk uses only a small subset of what strace provides. Extracting the
needs from the current analyzer and from the
[design notes](design_notes.md#strace-file-operations-output-analysis):

1. **Scope**: the file accesses of one command, and of all its
   descendant processes (fork / clone / exec); threads included.
   smk traces one command at a time, so the events of a trace are all
   attributed to the same run: no global system monitoring is needed.
2. **Events**: successful file-related operations only:
   - read access: open* with O_RDONLY, readlink* (Source);
   - write access: open* with O_WRONLY or O_RDWR, creat, write,
     mkdir, link (Target);
   - deletion: unlink, rmdir, and renameat* seen as target removed +
     target created (the Trigger concept: If update / presence /
     absence);
   - no need for: signals, network, memory, ioctl, exit status decoding.
3. **Paths**: absolute paths at the time of the operation, including
   for fd-based operations (strace's `-y` gives this today; a linked
   implementation would maintain its own fd table per traced process),
   and cwd tracking (chdir) to resolve relative paths.
4. **Success only**: failed calls must not be recorded (a failed
   `mkdir` or `rename` must not be counted as a write or a move). Today
   this is obtained with strace `-e status=successful`; a linked
   implementation gets the return value directly.
5. **Observation only**: smk never denies nor modifies the traced
   operations; notification is enough, no authorization/permission
   events needed.
6. **Operational constraints**:
   - usable **without special privileges** (smk is a build tool run as
     a normal user, tracing its own children);
   - acceptable overhead (the current ptrace-based mechanism is the
     main performance complaint);
   - no dependency on a text format: structured events or binary
     records whose layout is defined in system headers;
   - maintained, and well integrated with the OS.

### D1.2 Survey of the solutions per OS

The survey focuses on what can be linked from smk (native Ada binding
or C library), and mentions the external tools only as evidence that
the API family is alive.

#### Linux

| Solution                         | API to link                                                | Privileges                        | Fit with smk's needs                                                                                                                                                                                                                         |
|----------------------------------|------------------------------------------------------------|-----------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| ptrace(2)                        | syscall; an Ada binding exists since 2018 ([src/ptrace_binding](../../src/ptrace_binding), [src/minimal_strace](../../src/minimal_strace)) | none (own children)               | Full fit: syscall args and return value in the registers, children followed with PTRACE_O_TRACEFORK/CLONE/EXEC. Cost: two stops per syscall, per-arch register handling, own fd table. This is strace's mechanism, but without the text detour. |
| seccomp-bpf (SECCOMP_RET_TRACE) combined with ptrace | syscall (prctl, seccomp)                     | none (own children)               | Same as ptrace, but the child installs a filter so that only file-related syscalls trap: the other syscalls run at full speed. Removes most of the ptrace overhead on syscall-heavy commands (the typical smk workload).                          |
| eBPF (libbpf)                    | libbpf (C); tracepoints sys_enter_*/sys_exit_*             | root / CAP_BPF + CAP_PERFMON      | Excellent technical fit (kernel-integrated, very low overhead, ring buffer, pid in events), but `kernel.unprivileged_bpf_disabled` is 1 or 2 by default on current distros: a build tool cannot require root. Only viable as an opt-in privileged backend. |
| fanotify(7)                      | syscall                                                      | FID notification mode unprivileged; mount/fs marks need CAP_SYS_ADMIN | Partial fit: FAN_OPEN / FAN_CLOSE_WRITE / FAN_DELETE etc., but without privilege the kernel blanks the pid of events caused by other processes, which kills per-command attribution for smk's children; and the read/write intention is only known at close time, not at open time. |
| LD_PRELOAD interposition (fsatrace's mechanism) | own .so, links nothing but libc            | none                              | Good fit on coverage (fsatrace's `r/w/m/d/q/t` ops are exactly smk's model), but only sees libc calls: static binaries, raw syscalls and setuid programs are invisible. Not an API to link into smk, but an injection mechanism.                          |

References: [fanotify(7)](https://man7.org/linux/man-pages/man7/fanotify.7.html),
[kernel sysctl docs (unprivileged_bpf_disabled)](https://docs.kernel.org/admin-guide/sysctl/kernel.html),
[Neil Mitchell, File Access Tracing (2020)](http://neilmitchell.blogspot.com/2020/05/file-tracing.html)
(the Shake build system faces the exact same need and surveyed the same
approaches; it settled on fsatrace, i.e. interposition, because no
unprivileged kernel API fits).

#### macOS (Darwin)

| Solution                        | API to link                                     | Privileges / constraints                                | Fit with smk's needs                                                                                                                                                |
|---------------------------------|--------------------------------------------------|---------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Endpoint Security framework     | libEndpointSecurity.dylib (C API)                 | root + System Extension + Apple-granted entitlement      | Technically the best fit: notify events for open (with the open flags), exec, rename, unlink, with per-process attribution. But the entitlement (`com.apple.developer.endpoint-security.client`) is granted by Apple case by case, for security products: out of reach for a build tool. |
| DTrace                          | no linkable API; dtrace(1)/dtruss                | SIP blocks tracing protected binaries; root            | Not a library, and SIP (on by default) prevents tracing system binaries without `csrutil enable --without dtrace`. Not shippable as a default.                                                  |
| DYLD_INSERT_LIBRARIES interposition | own .dylib                                    | none; SIP blocks injection into platform binaries       | Same mechanism and same blind spots as LD_PRELOAD.                                                                                                               |

Reference: [Apple, Endpoint Security](https://developer.apple.com/documentation/endpointsecurity),
[com.apple.developer.endpoint-security.client entitlement](https://developer.apple.com/documentation/bundleresources/entitlements/com.apple.developer.endpoint-security.client),
[Red Canary mac-monitor wiki, Endpoint Security overview](https://github.com/redcanaryco/mac-monitor/wiki/5.-Endpoint-Security-Overview).

#### Windows

| Solution                        | API to link                                     | Privileges                | Fit with smk's needs                                                                                                                                           |
|---------------------------------|--------------------------------------------------|---------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------|
| ETW                             | StartTrace/ProcessTrace (advapi32, tdh); C++ helpers: krabsetw | admin for kernel providers | The supported kernel telemetry channel: Microsoft-Windows-Kernel-File gives create/read/write/close with process correlation. Requires elevation, which is awkward for a build tool. |
| Microsoft Detours               | MIT C++ library (linkable into smk)              | none for own children      | Injection + hooking of CreateFileW etc. in the children smk spawns: no admin, good coverage of normal tools. This is how fsatrace works on Windows. Only user-mode API calls are seen (fine in practice). |
| Minifilter driver               | FltMgr (kernel driver)                            | driver signing (WHQL)      | Out of reach.                                                                                                                                                  |

References: [ETW security (admin requirement)](https://www.geoffchappell.com/studies/windows/km/ntoskrnl/api/etw/secure/index.htm),
[microsoft/Detours](https://github.com/microsoft/Detours).

#### BSDs (FreeBSD, NetBSD, OpenBSD)

| Solution                        | API to link                                     | Privileges                | Fit with smk's needs                                                                                                                                           |
|---------------------------------|--------------------------------------------------|---------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------|
| ktrace(2) / kdump(1)            | syscall; records defined in `<sys/ktrace.h>`      | none (own processes)      | The best native fit found: kernel-side tracing (no ptrace stops, much faster than strace), follows children (`ktrace -i`, inherited trace points), unprivileged, and the record layout is a system header, not a text format. kdump is only a decoder; smk could read the binary records itself. Present in FreeBSD, NetBSD and OpenBSD. |
| DTrace (FreeBSD)                | no linkable API; dtrace(1)                        | root                      | syscall provider available, but root, and not a library.                                                                                                       |

References: [ktrace(2) FreeBSD](https://man.freebsd.org/cgi/man.cgi?query=ktrace&sektion=2),
[ktrace(1) NetBSD](https://man.netbsd.org/ktrace.1),
[ktrace(1) OpenBSD](https://man.openbsd.org/ktrace).

### D1.3 Which solutions match smk's needs

Combining the coverage of needs (D1.1) with the linkable constraint:

1. **No single cross-OS API exists.** Every OS provides its own
   mechanism, with very different privilege models; a cross-platform
   smk would need one backend per OS behind a common internal
   interface (the OS dependency is already localized in
   [smk-runs-run_command.adb](../../src/smk-runs-run_command.adb)).
2. **On Linux**, the only *unprivileged* and *linkable* general
   mechanism is **ptrace**, optionally combined with a **seccomp**
   filter so that only file syscalls trap (the performance answer to
   ptrace's usual cost). An Ada binding already exists in the repo
   from a 2018 experiment ([src/ptrace_binding](../../src/ptrace_binding),
   [src/minimal_strace](../../src/minimal_strace)). fanotify and eBPF
   are kernel-integrated and better maintained as *mechanisms*, but
   they are not usable unprivileged for smk's per-command attribution.
3. **On the BSDs**, **ktrace(2)** is nearly a perfect match:
   unprivileged, kernel-side (fast), binary format defined in a system
   header.
4. **On macOS**, the well-integrated API (Endpoint Security) is gated
   by an Apple-granted entitlement: realistically out of reach; only
   interposition remains.
5. **On Windows**, **Detours** (Microsoft, MIT, maintained) is the
   only unprivileged linkable option; ETW is the OS-integrated one but
   requires admin.

Interposition (LD_PRELOAD / DYLD_INSERT / Detours) is the only family
that works unprivileged everywhere, and it is the one Shake/rattle
adopted through fsatrace; but it is an injection mechanism, not an API
smk links against, and it has intrinsic blind spots (static binaries,
raw syscalls, setuid).

### D1.4 Options for smk, and open questions

- **Option A - direct ptrace (+ seccomp filter) on Linux**: smk
  becomes its own tracer, using the 2018 binding as a starting point.
  Removes the external tool, the text format and the re-parsing; the
  seccomp filter addresses the overhead on syscalls that don't
  interest smk. Effort: fd table per process, cwd tracking, per-arch
  register decoding; risk: ptrace edge cases (vfork, multiple threads)
  are the reason strace is 40k lines.
- **Option B - ktrace(2) backend for the BSDs**: low effort, high
  benefit, but only helps on BSDs; not available on Linux.
- **Option C - keep strace as the default, add the new backends
  progressively** behind a common "run observer" abstraction, the
  analyzer working on a common event model instead of a text buffer.
- **Option D - interposition everywhere** (fsatrace mechanism):
  unprivileged and fast on all OSes, but injection-based, with the
  blind spots above; could be a fallback rather than the main design.

Open questions:

1. Is the ptrace + seccomp effort acceptable, given that smk only
   needs a small, well-defined subset of what strace does (no signals,
   no network, no exec decoding)?
2. Should the common event model be defined first (replacing the
   strace text buffer), so that backends can be added independently?
3. Benchmark protocol: compare strace, direct ptrace and ptrace +
   seccomp on the smk test cases (gcc, sox/mp3 use cases are
   representative: syscall-heavy commands).
