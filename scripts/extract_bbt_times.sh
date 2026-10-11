#!/bin/bash
# Script to extract and format timing information from bbt JUnit XML results
# Usage: ./extract_bbt_times.sh [xml_file]
# If no file provided, uses /tmp/bbt_full_results.xml by default

XML_FILE="${1:-/tmp/bbt_full_results.xml}"

if [ ! -f "$XML_FILE" ]; then
    echo "Error: XML file not found: $XML_FILE"
    echo "Usage: $0 [xml_file]"
    echo "Example: $0 /tmp/bbt_results.xml"
    exit 1
fi

# Use C locale for numeric formatting
export LC_NUMERIC=C

# Helper function to get suite name for a testcase
get_suite_name() {
    local scenario="$1"
    local xml_file="$2"
    # First try to get from parent testsuite
    local suite_name=$(grep -B10 "name=\"$scenario\"" "$xml_file" | grep -oP 'testsuite name="\K[^"]+' | tail -1)
    if [ -n "$suite_name" ] && [ "$suite_name" != "" ]; then
        echo "$suite_name"
        return
    fi
    # If testsuite name is empty, try to get from testsuites name
    echo $(grep -oP 'testsuites name="\K[^"]+' "$xml_file" | head -1)
}

echo "=========================================="
echo "BBT Test Execution Times Report"
echo "=========================================="
echo "Generated from: $XML_FILE"
echo "Generated at: $(date)"
echo ""

# Extract total time
echo "--- Global Summary ---"
TOTAL_TIME=$(grep -oP 'testsuites.*time="\K[0-9.]+' "$XML_FILE" | head -1)
TOTAL_TESTS=$(grep -oP 'testsuites.*tests="\K[0-9]+' "$XML_FILE" | head -1)
TOTAL_FAILURES=$(grep -oP 'testsuites.*failures="\K[0-9]+' "$XML_FILE" | head -1)
echo "Total tests: $TOTAL_TESTS"
echo "Total failures: $TOTAL_FAILURES"
echo "Total time: ${TOTAL_TIME}s"
echo ""

# Extract per-suite times
echo "--- Per-Feature Times ---"
echo "Feature | Tests | Failures | Time (s) | Avg (s)"
echo "--------|-------|----------|----------|---------"

# Use awk to process each testsuite
awk '
/testsuite name=/ {
    suite_name = ""
    suite_tests = 0
    suite_failures = 0
    suite_time = 0
    
    if (match($0, /name="([^"]+)".*tests="([0-9]+)".*failures="([0-9]+)".*time="([0-9.]+)/, arr)) {
        suite_name = arr[1]
        suite_tests = arr[2] + 0
        suite_failures = arr[3] + 0
        suite_time = arr[4] + 0
        
        # Get just the filename
        if (suite_name != "") {
            split(suite_name, parts, "/")
            filename = parts[length(parts)]
        } else {
            filename = "(root)"
        }
        
        # Calculate average
        avg = (suite_tests > 0) ? suite_time / suite_tests : 0
        
        printf "%-25s | %-5d | %-8d | %-8.3f | %.3f\n", filename, suite_tests, suite_failures, suite_time, avg
    }
}
' "$XML_FILE"

echo ""
echo "--- Per-Scenario Times (sorted by time, descending) ---"
echo "Scenario | Feature | Time (s)"
echo "--------|---------|----------"

# Extract all testcase times with their suite and sort by time
# Handle both formats: with classname and without
while IFS='|' read -r scenario feature time; do
    # If feature is empty, try to get it from the suite
    if [ -z "$feature" ] || [ "$feature" = " " ]; then
        feature=$(get_suite_name "$scenario" "$XML_FILE")
        if [ -z "$feature" ]; then
            feature="(unknown)"
        fi
    fi
    printf "%-40s | %-20s | %.3fs\n" "$scenario" "$feature" "$time"
done < <(grep -E 'testcase.*time=' "$XML_FILE" | sed -E 's/.*name="([^"]+)".*classname="([^"]*)".*time="([0-9.]+).*/\1|\2|\3/' | sort -t'|' -k3 -nr)

echo ""
echo "--- Top 10 Slowest Scenarios ---"
echo "Scenario | Feature | Time (s)"
echo "--------|---------|----------"

# Top 10 slowest
while IFS='|' read -r scenario feature time; do
    # If feature is empty, try to get it from the suite
    if [ -z "$feature" ] || [ "$feature" = " " ]; then
        feature=$(get_suite_name "$scenario" "$XML_FILE")
        if [ -z "$feature" ]; then
            feature="(unknown)"
        fi
    fi
    # Truncate long scenario names
    if [ ${#scenario} -gt 38 ]; then
        scenario="${scenario:0:35}..."
    fi
    printf "%-40s | %-20s | %.3fs\n" "$scenario" "$feature" "$time"
done < <(grep -E 'testcase.*time=' "$XML_FILE" | sed -E 's/.*name="([^"]+)".*classname="([^"]*)".*time="([0-9.]+).*/\1|\2|\3/' | sort -t'|' -k3 -nr | head -10)

echo ""
echo "=========================================="
echo "Report generated successfully"
echo "=========================================="
