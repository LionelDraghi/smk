# ------------------------------------------------------------------------------
# smk, the smart make (https://github.com/LionelDraghi/smk)
#  © 2018, 2019 Lionel Draghi <lionel.draghi@free.fr>
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

.PHONY : all check sanity features tutorial unit doc clean benchmark benchmark-strace benchmark-compare

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
BBT       = cd run && PATH="$$PWD:$$PATH" bbt -k --yes

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

# Benchmark targets for performance comparison (strace vs ptrace API)
# These targets generate JUnit XML reports with timing information
# Usage:
#   make benchmark-strace      # Run benchmark with current strace implementation
#   make benchmark-compare BENCHMARK_NEW=file.xml  # Compare with new results

benchmark: benchmark-all
	@echo "Benchmark complete. Results saved in $(BENCHMARK_DIR)/"
	@echo "To compare with a future ptrace API implementation, run:"
	@echo "  make benchmark-compare BENCHMARK_NEW=<path/to/new/results.xml>"

benchmark-all: benchmark-strace

benchmark-sanity: ../smk
	@echo "--- Running sanity benchmark ---"
	@mkdir -p $(BENCHMARK_DIR)/strace_baseline
	$(FRESH_RUN)
	-cd run && PATH="$$PWD:$$PATH" bbt -k --yes --junit /tmp/bbt_sanity_$$(date +%Y%m%d_%H%M%S).xml ../../tests/sanity/sanity.md || true
	@XML_FILE=$$(ls /tmp/bbt_sanity_*.xml 2>/dev/null | head -1); \
	if [ -n "$$XML_FILE" ] && [ -f "$$XML_FILE" ]; then \
		cp $$XML_FILE $(BENCHMARK_DIR)/strace_baseline/sanity_$$(date +%Y%m%d_%H%M%S).xml; \
		$(BENCHMARK_SCRIPTS)/extract_bbt_times.sh $$XML_FILE > $(BENCHMARK_DIR)/strace_baseline/sanity_report_$$(date +%Y%m%d_%H%M%S).txt; \
		echo "Sanity benchmark results saved to $(BENCHMARK_DIR)/strace_baseline/"; \
		ls -la $(BENCHMARK_DIR)/strace_baseline/; \
	else \
		echo "Error: No XML file generated"; \
		exit 1; \
	fi

benchmark-strace: ../smk
	@echo "--- Running benchmark with current strace implementation ---"
	@mkdir -p $(BENCHMARK_DIR)/strace_baseline
	$(FRESH_RUN)
	-cd run && PATH="$$PWD:$$PATH" bbt -k --yes --junit /tmp/bbt_benchmark_$$(date +%Y%m%d_%H%M%S).xml $(BBT_ALL) || true
	@XML_FILE=$$(ls /tmp/bbt_benchmark_*.xml 2>/dev/null | head -1); \
	if [ -n "$$XML_FILE" ] && [ -f "$$XML_FILE" ]; then \
		cp $$XML_FILE $(BENCHMARK_DIR)/strace_baseline/results_$$(date +%Y%m%d_%H%M%S).xml; \
		$(BENCHMARK_SCRIPTS)/extract_bbt_times.sh $$XML_FILE > $(BENCHMARK_DIR)/strace_baseline/report_$$(date +%Y%m%d_%H%M%S).txt; \
		echo "Benchmark results saved to $(BENCHMARK_DIR)/strace_baseline/"; \
		ls -la $(BENCHMARK_DIR)/strace_baseline/; \
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
	@echo "Baseline: $(BENCHMARK_DIR)/strace_baseline/results_*.xml"
	@echo "New:      $(BENCHMARK_NEW)"
	@$(BENCHMARK_SCRIPTS)/compare_bbt_times.sh \
		$(BENCHMARK_DIR)/strace_baseline/results_*.xml \
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

