# ------------------------------------------------------------------------------
# smk, the smart make (https://github.com/LionelDraghi/smk)
# SPDX-FileCopyrightText: 2018, 2019, 2026 Lionel Draghi
# SPDX-License-Identifier: Apache-2.0
# ------------------------------------------------------------------------------

.PHONY : all check sanity features tutorial unit doc clean benchmark benchmark-ptrace benchmark-compare

all: check doc

# Scenario files, in run order, as seen from the run dir: the features
# share the run dir state, and some expect the files left by the
# previous ones (for instance, the mp3 find reports hello_c as a
# source dir). The sanity tests and the tutorial are self-contained,
# and the features are run in a single bbt invocation, so that each
# target below can be run alone.
SANITY   = ../../tests/sanity/sanity.md
FEATURES = ../../docs/Features/A010_queries.md \
           ../../docs/Features/A020_build.md \
           ../../docs/Features/A030_smkfile_format.md \
           ../../docs/Features/A040_errors.md \
           ../../docs/Features/A050_use_cases.md
TUTORIAL = ../../docs/tutorial.md

# The scenarios are run in a fresh run dir, recreated by each target,
# with a smk link to the built binary, so that the scenarios just
# invoke `smk`.
# NB: bbt is run from the run dir, because it stores in the index the
# paths as given on the command line: the scenario files must be given
# relatively to the run dir, so that the links in docs/tests/results.md
# render correctly. This is also why the bbt --index and
# --generate_badge outputs go directly in docs/tests/, and not in the
# current dir then moved at the end of the run: otherwise the relative
# paths in the results file would be wrong.
FRESH_RUN = rm -rf run && mkdir run && ln -s ../../smk run/smk
# bbt is run with LD_LIBRARY_PATH unset: a polluted one (e.g. from a
# VSCode extension) leaks into the traced commands, and breaks the
# tools they run (see the root Makefile help note).
BBT       = cd run && PATH="$$PWD:$$PATH" env -u LD_LIBRARY_PATH bbt -k --yes

# Benchmark directory and files
BENCHMARK_DIR = ../benchmarks
BENCHMARK_SCRIPTS = ../scripts
BBT_ALL = ../../tests/sanity/sanity.md \
         ../../docs/Features/A010_queries.md \
         ../../docs/Features/A020_build.md \
         ../../docs/Features/A030_smkfile_format.md \
         ../../docs/Features/A040_errors.md \
         ../../docs/Features/A050_use_cases.md \
         ../../docs/tutorial.md
BBT_FEATURES = ../../docs/Features/A010_queries.md \
               ../../docs/Features/A020_build.md \
               ../../docs/Features/A030_smkfile_format.md \
               ../../docs/Features/A040_errors.md \
               ../../docs/Features/A050_use_cases.md

sanity: ../smk
	$(FRESH_RUN)
	$(BBT) $(SANITY)

features: ../smk
	$(FRESH_RUN)
	$(BBT) $(FEATURES)

tutorial: ../smk
	$(FRESH_RUN)
	$(BBT) $(TUTORIAL)

unit:
	$(MAKE) check --directory=unit_file_utilities

# Benchmark targets: bbt execution times of the current smk, stored
# as JUnit XML reports (the ptrace implementation since D1 M3).
# The full-suite strace baseline (pre-D1, 62 scenarios) is frozen in
# benchmarks/strace_baseline/reference_full.xml: it was produced with
# the same protocol, on the last strace based smk, in this directory
# (the machine dependent goldens embed the run dir absolute path).
# Usage:
#   make benchmark-ptrace      # Run the benchmark with the current smk
#   make benchmark-compare BENCHMARK_NEW=file.xml  # Compare with baseline

benchmark: benchmark-ptrace
	@echo "Benchmark complete. Results saved in $(BENCHMARK_DIR)/ptrace/"
	@echo "To compare with the strace baseline, run:"
	@echo "  make benchmark-compare BENCHMARK_NEW=<path/to/ptrace/results.xml>"

benchmark-ptrace: ../smk
	@echo "--- Running benchmark with the current ptrace implementation ---"
	@mkdir -p $(BENCHMARK_DIR)/ptrace
	$(FRESH_RUN)
	-cd run && PATH="$$PWD:$$PATH" env -u LD_LIBRARY_PATH bbt -k --yes --junit /tmp/bbt_ptrace_$$(date +%Y%m%d_%H%M%S).xml $(BBT_ALL) || true
	@XML_FILE=$$(ls -t /tmp/bbt_ptrace_*.xml 2>/dev/null | head -1); \
	if [ -n "$$XML_FILE" ] && [ -f "$$XML_FILE" ]; then \
		cp $$XML_FILE $(BENCHMARK_DIR)/ptrace/results_$$(date +%Y%m%d_%H%M%S).xml; \
		$(BENCHMARK_SCRIPTS)/extract_bbt_times.sh $$XML_FILE > $(BENCHMARK_DIR)/ptrace/report_$$(date +%Y%m%d_%H%M%S).txt; \
		echo "Benchmark results saved to $(BENCHMARK_DIR)/ptrace/"; \
		ls -la $(BENCHMARK_DIR)/ptrace/; \
	else \
		echo "Error: No XML file generated"; \
		exit 1; \
	fi

benchmark-compare:
	@if [ -z "$(BENCHMARK_NEW)" ]; then \
		echo "Error: BENCHMARK_NEW not specified"; \
		echo "Usage: make benchmark-compare BENCHMARK_NEW=path/to/new/results.xml"; \
		exit 1; \
	fi
	@echo "--- Comparing benchmark results ---"
	@echo "Baseline: $(BENCHMARK_DIR)/strace_baseline/reference_full.xml"
	@echo "New:      $(BENCHMARK_NEW)"
	@$(BENCHMARK_SCRIPTS)/compare_bbt_times.sh \
		$(BENCHMARK_DIR)/strace_baseline/reference_full.xml \
		$(BENCHMARK_NEW) \
		> $(BENCHMARK_DIR)/comparison_report_$$(date +%Y%m%d_%H%M%S).txt 2>&1
	@cat $(BENCHMARK_DIR)/comparison_report_*.txt
	@echo "Comparison report saved to $(BENCHMARK_DIR)/comparison_report_*.txt"

check: ../smk
	$(FRESH_RUN)
	$(BBT) \
	  --index ../../docs/tests/results.md \
	  --generate_badge ../../docs/tests/badge.url \
	  $(SANITY) $(FEATURES) $(TUTORIAL)
	$(MAKE) unit
	# the unit tests results are appended to the bbt index:
	cat unit_file_utilities/results.md >> ../docs/tests/results.md

doc:
	# fetch the tests badge generated by bbt during check:
	curl -s "$$(cat ../docs/tests/badge.url)" -o ../docs/tests/badge.svg

clean:
	# remove the run dir, and the residues of scenario runs that
	# would have been executed with tests/ as working dir, instead
	# of the run dir
	rm -rf run hello_c dir1 dir2
	rm -f .smk.* out.* default.smk expected.13 MyBuild \
	      hello.c main.c hello.h \
	      multiline_smkfile1.txt multiline_smkfile2.txt \
	      hill_multiline_smkfile.txt ogg-to-mp3.sh to-mp3.sh \
	      x.ogg y.ogg z.ogg *.mp3 *.md.out *.md.err
	$(MAKE) -i clean --directory=unit_file_utilities

