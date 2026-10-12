#!/bin/bash
# Script to compare bbt execution times between two XML result files
# Usage: ./compare_bbt_times.sh baseline.xml new.xml [output_file]

BASELINE="$1"
NEW="$2"
OUTPUT="${3:-/dev/stdout}"

if [ ! -f "$BASELINE" ] || [ ! -f "$NEW" ]; then
    echo "Error: XML files not found"
    echo "Usage: $0 baseline.xml new.xml [output_file]"
    exit 1
fi

export LC_NUMERIC=C

echo "=========================================="
echo "BBT Execution Times Comparison"
echo "=========================================="
echo "Baseline: $BASELINE"
echo "New:      $NEW"
echo "Generated: $(date)"
echo ""

# Extract total times
BASELINE_TOTAL=$(grep -oP 'testsuites.*time="\K[0-9.]+' "$BASELINE" | head -1)
NEW_TOTAL=$(grep -oP 'testsuites.*time="\K[0-9.]+' "$NEW" | head -1)
BASELINE_TESTS=$(grep -oP 'testsuites.*tests="\K[0-9]+' "$BASELINE" | head -1)
NEW_TESTS=$(grep -oP 'testsuites.*tests="\K[0-9]+' "$NEW" | head -1)

echo "--- Global Comparison ---"
echo "Baseline - Tests: $BASELINE_TESTS, Total time: ${BASELINE_TOTAL}s"
echo "New      - Tests: $NEW_TESTS, Total time: ${NEW_TOTAL}s"
echo ""

# Calculate difference
TIME_DIFF=$(echo "$NEW_TOTAL - $BASELINE_TOTAL" | bc -l)
PERCENT_DIFF=$(echo "($TIME_DIFF / $BASELINE_TOTAL) * 100" | bc -l)

if (( $(echo "$TIME_DIFF < 0" | bc -l) )); then
    echo "✓ Improvement: ${TIME_DIFF}s (${PERCENT_DIFF}% faster)"
else
    echo "✗ Regression: +${TIME_DIFF}s (${PERCENT_DIFF}% slower)"
fi
echo ""

# Per-suite comparison
echo "--- Per-Feature Comparison ---"
echo "Feature | Baseline (s) | New (s) | Diff (s) | Diff (%)"
echo "--------|---------------|---------|----------|----------"

# Extract suite names and times from both files
# For each suite in baseline, find corresponding in new
for suite in $(grep -oP 'testsuite name="\K[^"]+' "$BASELINE" | sort -u); do
    # Get baseline time
    BASELINE_SUITE_TIME=$(grep "name=\"${suite}\"" "$BASELINE" | grep -oP 'time="\K[0-9.]+' | head -1)
    
    # Get new time
    NEW_SUITE_TIME=$(grep "name=\"${suite}\"" "$NEW" | grep -oP 'time="\K[0-9.]+' | head -1)
    
    # Default to 0 if not found
    BASELINE_SUITE_TIME=${BASELINE_SUITE_TIME:-0}
    NEW_SUITE_TIME=${NEW_SUITE_TIME:-0}
    
    # Calculate diff
    SUITE_DIFF=$(echo "$NEW_SUITE_TIME - $BASELINE_SUITE_TIME" | bc -l)
    SUITE_PERCENT=$(echo "($SUITE_DIFF / $BASELINE_SUITE_TIME) * 100" | bc -l 2>/dev/null || echo "N/A")
    
    # Get just filename
    filename=$(basename "$suite")
    
    # Format output
    if (( $(echo "$SUITE_DIFF < 0" | bc -l) )); then
        DIFF_STR="✓ ${SUITE_DIFF}"
        PERCENT_STR="✓ ${SUITE_PERCENT}%"
    else
        DIFF_STR="✗ +${SUITE_DIFF}"
        PERCENT_STR="✗ +${SUITE_PERCENT}%"
    fi
    
    printf "%-25s | %-13s | %-7s | %-8s | %s\n" "$filename" "$BASELINE_SUITE_TIME" "$NEW_SUITE_TIME" "$DIFF_STR" "$PERCENT_STR"
done

echo ""
echo "--- Per-Scenario Comparison (Top 10 slowest in baseline) ---"
echo "Scenario | Baseline (s) | New (s) | Diff (s) | Diff (%)"
echo "--------|---------------|---------|----------|----------"

# Get top 10 slowest scenarios from baseline
grep '<testcase ' "$BASELINE" | sed -E 's/^[[:space:]]*<testcase name="([^"]+)".*time="([0-9.]+).*/\1|\2/' | sort -t'|' -k2 -nr | head -10 | while IFS='|' read -r scenario baseline_time; do
    # Get new time for this scenario
    # Find the testcase with the same name in new file
    new_time=$(grep "name=\"${scenario}\"" "$NEW" | grep -oP 'time="\K[0-9.]+' | head -1)
    new_time=${new_time:-0}
    
    # Calculate diff
    SCENARIO_DIFF=$(echo "$new_time - $baseline_time" | bc -l)
    SCENARIO_PERCENT=$(echo "($SCENARIO_DIFF / $baseline_time) * 100" | bc -l 2>/dev/null || echo "N/A")
    
    # Format output
    if (( $(echo "$SCENARIO_DIFF < 0" | bc -l) )); then
        DIFF_STR="✓ ${SCENARIO_DIFF}"
        PERCENT_STR="✓ ${SCENARIO_PERCENT}%"
    else
        DIFF_STR="✗ +${SCENARIO_DIFF}"
        PERCENT_STR="✗ +${SCENARIO_PERCENT}%"
    fi
    
    # Truncate long scenario names
    if [ ${#scenario} -gt 35 ]; then
        scenario="${scenario:0:32}..."
    fi
    
    printf "%-35s | %-13s | %-7s | %-8s | %s\n" "$scenario" "$baseline_time" "$new_time" "$DIFF_STR" "$PERCENT_STR"
done

echo ""
echo "=========================================="
echo "Comparison complete"
echo "=========================================="

# Save to file if output specified
if [ "$OUTPUT" != "/dev/stdout" ]; then
    echo "Report saved to: $OUTPUT"
fi
