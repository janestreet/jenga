#!/usr/bin/env bash
set -e -u -o pipefail

if [ 5 != "$#" ]; then echo -e "use with 4 features and a target to build:\n  run.sh <jenga1> <jenga2> <build1> <build2> <target>"; exit 1; fi

export FEATURE1="$1"
export FEATURE2="$2"

FEATURE_TO_BUILD1="$3"
FEATURE_TO_BUILD2="$4"

TARGET="$5"

[ "$(stat -f -L -c %T .)" != "nfs" ] || (echo "don't run this on nfs"; exit 1)

SCRIPT_DIR="$(dirname "$(readlink -f "$0")")"
BASE_DIR=$(pwd)

ensure_clean () {
  fe workspace create "$FEATURE"
  cd "$(fe workspace dir "$FEATURE")"
  diff <(:) <(hg status) || (echo unclean feature "$FEATURE"; false)
}

build_jenga () {
  fe workspace create "$FEATURE"
  cd "$(fe workspace dir "$FEATURE")"
  fe up
  diff <(:) <(hg status)
  REV=$(fe show -tip)
  echo "$FEATURE rev.: $REV" >&2
  diff <(cat <<< "$REV") <(hg log -r . --template '{node}\n') || (echo "unpushed changes in $FEATURE?"; exit 1)
  REV_SHORT=$(cut --bytes 1-12 <<< "$REV")
  VERSION_UTIL_SUPPORT=true X_LIBRARY_INLINING=true jenga app/jenga/bin/jenga.exe -show-mem
  BIN="$(fe workspace dir "$FEATURE")"/app/jenga/bin/jenga.exe
  diff <("$BIN" version | tail -n 1 | sed -r 's#^.*_(.*)$#\1#') <(cat <<< "$REV_SHORT")
}

FEATURE=$FEATURE1 ensure_clean
FEATURE=$FEATURE2 ensure_clean

FEATURE=$FEATURE1 build_jenga
BIN1=$BIN
REV1=$REV

FEATURE=$FEATURE2 build_jenga
BIN2=$BIN
REV2=$REV

REV_TO_BUILD1=$(fe show "$FEATURE_TO_BUILD1" -tip)
REV_TO_BUILD2=$(fe show "$FEATURE_TO_BUILD2" -tip)

TEST_DIR1="$BASE_DIR/${REV1}_$REV_TO_BUILD1"
TEST_DIR2="$BASE_DIR/${REV2}_$REV_TO_BUILD2"

echo "writing to $TEST_DIR1 and $TEST_DIR2"

mkdir "$TEST_DIR1"
mkdir "$TEST_DIR2"

J1="$TEST_DIR1/+clone+"
J2="$TEST_DIR2/+clone+"

(cd "$(fe workspace dir -basedir)"/jane/+clone+; hg pull -r "$REV_TO_BUILD1")
(cd "$(fe workspace dir -basedir)"/jane/+clone+; hg pull -r "$REV_TO_BUILD2")
hg quick-share "$(fe workspace dir -basedir)"/jane/+clone+ "$J1"
hg quick-share "$(fe workspace dir -basedir)"/jane/+clone+ "$J2"
(cd "$J1"; hg up -r "$REV_TO_BUILD1")
(cd "$J2"; hg up -r "$REV_TO_BUILD2")

ulimit -s 500000
export LINK_EXECUTABLES=false


for x in `seq 1 5`; do
  build() {
      cd "$1"/+clone+
      "$2" build "$TARGET" > "$1"/"$3"-verbose
      grep done < "$1"/"$3"-verbose >> "$1"/"$3"
  }
  echo -n "$x "
  (cd "$J1"; hg distclean; cd "$J2"; hg distclean)
  diff <(:) <(hg status)

  build "$TEST_DIR1" "$BIN1" "from-clean"
  build "$TEST_DIR2" "$BIN2" "from-clean"
  for x in `seq 1 4`; do
    echo -n "."
    build "$TEST_DIR1" "$BIN1" "incremental"
    build "$TEST_DIR2" "$BIN2" "incremental"
  done
  echo
done

echo from-clean:
"$SCRIPT_DIR"/analyze.ml "$TEST_DIR1"/from-clean "$TEST_DIR2"/from-clean
echo incremental:
"$SCRIPT_DIR"/analyze.ml "$TEST_DIR1"/incremental "$TEST_DIR2"/incremental
