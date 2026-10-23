#!/bin/sh
set -eu

package_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
build_dir="$package_dir/build"

mkdir -p "$build_dir/SOURCES"
install -m 0644 "$package_dir/.config/xkb/symbols/uf" "$build_dir/SOURCES/uf"
install -m 0644 "$package_dir/evdev.xml" "$build_dir/SOURCES/evdev.xml"

exec rpmbuild -ba \
    --define "_topdir $build_dir" \
    "$package_dir/personal-keyboard-layouts.spec"
