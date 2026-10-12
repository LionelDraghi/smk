# ------------------------------------------------------------------------------
# smk, the smart make (https://github.com/LionelDraghi/smk)
# SPDX-FileCopyrightText: 2026 Lionel Draghi
# SPDX-License-Identifier: Apache-2.0
# ------------------------------------------------------------------------------
# Refresh the SPDX-FileCopyrightText years of the file headers from the
# git history: each file carries the years of its changes.
# Run it on a clean working tree, as part of the release procedure
# (docs/dev/release_procedure.md), then commit the result with the release.
# Files with uncommitted changes also get the current year.

set -eu

NAME="Lionel Draghi"
THIS_YEAR=$(date +%Y)

# Every tracked file carrying the standard header:
FILES=$(git grep -l "SPDX-FileCopyrightText:" -- \
            'src/**' 'Makefile' 'smk.gpr' 'scripts/**' 'tests/**' \
            ':!tests/run' ':!tests/unit_strace_analysis')

for f in $FILES; do
   # years of the committed changes:
   YEARS=$(git log --follow --format=%ad --date=format:%Y -- "$f" | sort -u | paste -sd, -)
   # add the current year if the file has uncommitted changes:
   if [ -n "$(git status --porcelain -- "$f")" ]; then
      case ",$YEARS," in
         *",$THIS_YEAR,"*) ;;
         *) YEARS="$YEARS,$THIS_YEAR" ;;
      esac
   fi
   YEARS=$(echo "$YEARS" | sed "s/,/, /g")
   sed -i -E "s/^([[:space:]]*)(#|--|\*)[[:space:]]*SPDX-FileCopyrightText:.*/\1\2 SPDX-FileCopyrightText: ${YEARS} ${NAME}/" "$f"
done

echo "Copyright years refreshed in:"
echo "$FILES"
