#!/usr/bin/env python3
"""Rebuild src/rust/vendor.tar.xz and inst/AUTHORS (maintainer tool, not run at install).

Run from the package root (the directory that holds DESCRIPTION):

    python tools/update_vendor.py            # vendor, filter, pack, write inst/AUTHORS
    python tools/update_vendor.py --authors  # only rewrite inst/AUTHORS

What it does, following "Using Rust in CRAN packages":

1. `cargo vendor --locked` of src/rust/Cargo.lock into a temporary directory.
2. Keeps only the crates that are compiled (normal and build dependencies, never
   dev-dependencies) on the platforms CRAN builds on (PLATFORMS below). Every other
   crate is replaced by an empty stub (manifest + empty lib.rs) so that Cargo can
   still read the lockfile offline.
3. Removes tests/, benches/, examples/ and docs/ of each crate, and every
   precompiled binary (.a, .lib, .o, .obj, .dll, .so, .dylib, .exe), so the tarball
   carries source code only. Windows import libraries are not needed because
   src/Makevars.win.in builds with `--cfg windows_raw_dylib`.
4. Writes .cargo-checksum.json with an empty file list for every crate (the
   package checksum is kept), then packs `vendor/` reproducibly with xz -9e.
   psm < 0.1.29 gets the upstream .note.GNU-stack fix (see patch_psm()).
5. Writes inst/AUTHORS with name, version, authors, licence and repository of
   every compiled crate, and prints the highest `rust-version` among them, which
   is the real minimum for SystemRequirements.

Requires Python >= 3.11 and cargo on the PATH (network access to crates.io only
for step 1, never at install time).
"""

from __future__ import annotations

import argparse
import io
import json
import lzma
import os
import re
import shutil
import subprocess
import sys
import tarfile
import tempfile
import tomllib
from pathlib import Path

PLATFORMS = [
    "x86_64-unknown-linux-gnu",
    "aarch64-unknown-linux-gnu",
    "x86_64-apple-darwin",
    "aarch64-apple-darwin",
    "x86_64-pc-windows-gnu",
    "aarch64-pc-windows-gnullvm",
]
DROP_DIRS = {"tests", "testdata", "benches", "examples", "docs", "doc", "fuzz", ".github", "ci"}
# Parts of the C libraries bundled by *-sys crates that their build.rs never compiles.
DROP_NESTED = {"zstd-sys": ["zstd/programs", "zstd/tests", "zstd/contrib", "zstd/doc",
                            "zstd/examples", "zstd/zlibWrapper", "zstd/build"],
               "lz4-sys": ["liblz4/programs", "liblz4/tests", "liblz4/examples",
                           "liblz4/contrib", "liblz4/doc", "liblz4/build"]}
DROP_FILE = re.compile(r"^(CHANGELOG|CHANGES|RELEASES|HISTORY)(\.md|\.txt)?$|\.pdf$|^Cargo\.(lock|toml\.orig)$",
                       re.I)
BINARY_EXT = {".a", ".lib", ".o", ".obj", ".dll", ".so", ".dylib", ".exe", ".pdb", ".wasm"}
TARGET_TABLES = ("bin", "example", "test", "bench")

ROOT = Path.cwd()
RUST = ROOT / "src" / "rust"
MANIFEST = RUST / "Cargo.toml"


def cargo_metadata(platform: str | None) -> dict:
    cmd = ["cargo", "metadata", "--format-version", "1", "--locked",
           "--manifest-path", str(MANIFEST)]
    if platform:
        cmd += ["--filter-platform", platform]
    out = subprocess.run(cmd, check=True, capture_output=True, text=True).stdout
    return json.loads(out)


def compiled_packages() -> dict[str, dict]:
    """Package metadata (by id) of crates compiled on any of PLATFORMS.

    Each entry gets an extra "enabled_features" set (union over platforms).
    """
    keep: dict[str, dict] = {}
    for platform in PLATFORMS:
        meta = cargo_metadata(platform)
        pkgs = {p["id"]: p for p in meta["packages"]}
        nodes = {n["id"]: n for n in meta["resolve"]["nodes"]}
        root = meta["resolve"]["root"]
        stack, seen = [root], set()
        while stack:
            pid = stack.pop()
            if pid in seen:
                continue
            seen.add(pid)
            for dep in nodes[pid]["deps"]:
                kinds = {k["kind"] for k in dep["dep_kinds"]}
                if kinds - {"dev"}:
                    stack.append(dep["pkg"])
        for pid in seen:
            if pid != root:
                entry = keep.setdefault(pid, dict(pkgs[pid], enabled_features=set()))
                entry["enabled_features"].update(nodes[pid]["features"])
    return keep


def strip_manifest(text: str) -> str:
    """Manifest for an empty stub: no build script and no extra targets."""
    out, skip, section = [], False, ""
    for line in text.splitlines():
        header = re.match(r"^\s*\[\[?([^\]]+)\]\]?\s*$", line)
        if header:
            section = header.group(1).strip()
            skip = section in TARGET_TABLES
        if skip:
            continue
        # `links` without a build script is an error, so both go.
        if section == "package" and re.match(r"^\s*(build|links|rust-version)\s*=", line):
            continue
        # Stubs are never compiled; an old Cargo must still be able to parse them.
        if section == "package" and re.match(r'^\s*edition\s*=\s*"2024"', line):
            line = 'edition = "2021"'
        out.append(line)
    return "\n".join(out) + "\n"


def write_checksum(crate_dir: Path) -> None:
    ck = crate_dir / ".cargo-checksum.json"
    package = json.loads(ck.read_text(encoding="utf-8")).get("package")
    ck.write_text(json.dumps({"files": {}, "package": package}), encoding="utf-8")


def stub(crate_dir: Path) -> None:
    manifest = (crate_dir / "Cargo.toml").read_text(encoding="utf-8")
    checksum = (crate_dir / ".cargo-checksum.json").read_text(encoding="utf-8")
    shutil.rmtree(crate_dir)
    (crate_dir / "src").mkdir(parents=True)
    (crate_dir / "src" / "lib.rs").write_text("", encoding="utf-8")
    (crate_dir / "Cargo.toml").write_text(strip_manifest(manifest), encoding="utf-8")
    (crate_dir / ".cargo-checksum.json").write_text(checksum, encoding="utf-8")
    write_checksum(crate_dir)


def prune(crate_dir: Path, crate: str = "") -> list[str]:
    removed = []
    for name in [*DROP_DIRS, *DROP_NESTED.get(crate, [])]:
        d = crate_dir / name
        if d.is_dir():
            shutil.rmtree(d)
            removed.append(name + "/")
    for f in list(crate_dir.rglob("*")):
        if f.is_file() and (f.suffix.lower() in BINARY_EXT or DROP_FILE.search(f.name)):
            removed.append(str(f.relative_to(crate_dir)))
            f.unlink()
    write_checksum(crate_dir)
    return removed


def prune_windows_modules(crate_dir: Path, features: set[str]) -> int:
    """Drop API modules of the 'windows'/'windows-sys' crates whose feature is off.

    Every module there is declared as `#[cfg(feature = "Win32_System_Com")] pub mod Com;`
    and lives in src/Windows/Win32/System/Com/, so the files of a disabled feature are
    never read by rustc. This takes 'windows' from ~120 MB to ~10 MB.
    """
    base = crate_dir / "src" / "Windows"
    if not base.is_dir():
        return 0
    removed = 0
    for top, dirs, _files in os.walk(base, topdown=True):
        rel = Path(top).relative_to(base).parts
        for d in list(dirs):
            if "_".join((*rel, d)) not in features:
                shutil.rmtree(Path(top) / d)
                dirs.remove(d)
                removed += 1
    return removed


def prune_winapi_modules(crate_dir: Path, features: set[str]) -> int:
    """Same idea for 'winapi' 0.3 (`#[cfg(feature = "x")] pub mod x;` in src/*/mod.rs).

    Its build.rs turns on the headers each enabled header needs (table
    `("name", &[deps], &[libs])`), so the closure of that table is kept.
    """
    if "everything" in features:
        return 0
    table = re.findall(r'\("(\w+)", &\[([^\]]*)\], &\[',
                       (crate_dir / "build.rs").read_text(encoding="utf-8"))
    deps = {name: re.findall(r'"(\w+)"', body) for name, body in table}
    needed, stack = set(), [f for f in features if f in deps]
    while stack:
        f = stack.pop()
        if f not in needed:
            needed.add(f)
            stack.extend(deps.get(f, []))
    gated = re.compile(r'^#\[cfg\(feature = "(\w+)"\)\] pub mod (\w+);', re.M)
    removed = 0
    for mod_rs in (crate_dir / "src").glob("*/mod.rs"):
        for feature, name in gated.findall(mod_rs.read_text(encoding="utf-8")):
            if feature in needed or feature in features:
                continue
            for target in (mod_rs.parent / f"{name}.rs", mod_rs.parent / name):
                if target.is_file():
                    target.unlink()
                    removed += 1
                elif target.is_dir():
                    shutil.rmtree(target)
                    removed += 1
    return removed


GNU_STACK_NOTE = """#if defined(__linux__) && defined(__ELF__)
.section .note.GNU-stack,"",%progbits
#endif
"""


def patch_psm(crate_dir: Path) -> int:
    """Backport of psm 0.1.29: mark the assembly as not needing an executable stack.

    psm >= 0.1.29 needs ar_archive_writer 0.5 (edition 2024, rustc >= 1.85), so the
    lockfile keeps psm 0.1.26 and gets the upstream fix here. Without it GNU ld warns
    'missing .note.GNU-stack section implies executable stack', and glibc >= 2.41
    refuses to dlopen() a shared object that asks for an executable stack.
    """
    arch = crate_dir / "src" / "arch"
    note = arch / "gnu_stack_note.s"
    if note.exists():
        return 0
    note.write_text(GNU_STACK_NOTE, encoding="ascii", newline="\n")
    patched = 1
    for asm in arch.glob("*.s"):
        text = asm.read_text(encoding="utf-8")
        if '#include "psm.h"' in text and "gnu_stack_note" not in text:
            text = text.replace('#include "psm.h"', '#include "psm.h"\n#include "gnu_stack_note.s"', 1)
            asm.write_text(text, encoding="utf-8", newline="\n")
            patched += 1
    return patched


def crate_dirs(vendor: Path) -> dict[tuple[str, str], Path]:
    dirs = {}
    for d in vendor.iterdir():
        m = d / "Cargo.toml"
        if m.is_file():
            pkg = tomllib.loads(m.read_text(encoding="utf-8"))["package"]
            dirs[(pkg["name"], pkg["version"])] = d
    return dirs


def pack(vendor: Path, out: Path) -> None:
    files = sorted(p for p in vendor.rglob("*"))
    buf = io.BytesIO()
    with tarfile.open(fileobj=buf, mode="w", format=tarfile.USTAR_FORMAT) as tar:
        for p in [vendor, *files]:
            info = tar.gettarinfo(str(p), arcname=p.relative_to(vendor.parent).as_posix())
            info.uid = info.gid = 0
            info.uname = info.gname = ""
            info.mtime = 0
            info.mode = 0o755 if p.is_dir() else 0o644
            if p.is_file():
                with open(p, "rb") as fh:
                    tar.addfile(info, fh)
            else:
                tar.addfile(info)
    data = lzma.compress(buf.getvalue(), preset=9 | lzma.PRESET_EXTREME)
    out.write_bytes(data)


def version_key(v: str) -> tuple[int, ...]:
    return tuple(int(x) for x in re.findall(r"\d+", v)[:3])


def license_of(p: dict) -> str:
    """SPDX expression, or the licence named in the crate's license-file."""
    if p.get("license"):
        return p["license"]
    lf = p.get("license_file")
    if lf:
        path = Path(p["manifest_path"]).parent / lf
        head = path.read_text(encoding="utf-8", errors="replace")[:400] if path.is_file() else ""
        for pattern, spdx in ((r"Apache License\s+Version 2\.0", "Apache-2.0"),
                              (r"MIT License|Permission is hereby granted", "MIT"),
                              (r"BSD", "BSD (see licence file)")):
            if re.search(pattern, head):
                return f"{spdx} (license file {Path(lf).name})"
        return f"see license file {Path(lf).name}"
    return "see crate"


def write_authors(keep: dict[str, dict]) -> None:
    pkgs = sorted(keep.values(), key=lambda p: (p["name"], version_key(p["version"])))
    lines = [
        "Authors and copyright holders of the Rust crates vendored in",
        "src/rust/vendor.tar.xz and compiled into the rquasar package.",
        "",
        "Generated by tools/update_vendor.py from src/rust/Cargo.lock; do not edit by hand.",
        f"Crates: {len(pkgs)}. Licences of every crate are compatible with GPL (>= 3).",
        "The full licence texts ship inside each crate directory of vendor.tar.xz.",
        "",
    ]
    for p in pkgs:
        authors = ", ".join(p.get("authors") or []) or "The " + p["name"] + " developers"
        lines += [
            f"Name: {p['name']}",
            f"Version: {p['version']}",
            f"Authors: {authors}",
            f"License: {license_of(p)}",
        ]
        if p.get("repository"):
            lines.append(f"Repository: {p['repository']}")
        lines.append("")
    target = ROOT / "inst" / "AUTHORS"
    target.parent.mkdir(exist_ok=True)
    text = "\n".join(lines)
    # ASCII only: CRAN prefers it and some crate authors use accents.
    text = text.encode("ascii", "backslashreplace").decode("ascii")
    target.write_text(text, encoding="ascii", newline="\n")
    licenses = sorted({license_of(p) for p in pkgs})
    print(f"inst/AUTHORS: {len(pkgs)} crates; licences: {'; '.join(licenses)}")


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--authors", action="store_true", help="only rewrite inst/AUTHORS")
    args = ap.parse_args()
    if not MANIFEST.is_file():
        sys.exit("Run from the package root (src/rust/Cargo.toml not found).")

    keep = compiled_packages()
    write_authors(keep)
    msrv = max((p.get("rust_version") or "0" for p in keep.values()), key=version_key)
    top = sorted(keep.values(), key=lambda p: version_key(p.get("rust_version") or "0"))[-3:]
    print("Highest rust-version among compiled crates: " + msrv + " (" +
          ", ".join(f"{p['name']} {p['version']}: {p.get('rust_version')}" for p in top) + ")")
    if args.authors:
        return

    wanted = {(p["name"], p["version"]): p for p in keep.values()}
    with tempfile.TemporaryDirectory() as tmp:
        vendor = Path(tmp) / "vendor"
        subprocess.run(["cargo", "vendor", "--locked", "--manifest-path", str(MANIFEST),
                        str(vendor)], check=True, stdout=subprocess.DEVNULL)
        stubs, removed = 0, 0
        for key, d in crate_dirs(vendor).items():
            if key in wanted:
                if key[0] in ("windows", "windows-sys"):
                    removed += prune_windows_modules(d, wanted[key]["enabled_features"])
                if key[0] == "psm":
                    removed += patch_psm(d)
                if key[0] == "winapi":
                    removed += prune_winapi_modules(d, wanted[key]["enabled_features"])
                removed += len(prune(d, key[0]))
            else:
                stub(d)
                stubs += 1
        out = RUST / "vendor.tar.xz"
        pack(vendor, out)
        size = sum(f.stat().st_size for f in vendor.rglob("*") if f.is_file())
    print(f"Kept {len(wanted)} crates, stubbed {stubs}, removed {removed} files/dirs.")
    print(f"vendor/: {size / 2**20:.1f} MB -> {out.relative_to(ROOT)}: "
          f"{out.stat().st_size / 2**20:.2f} MB")


if __name__ == "__main__":
    os.environ.setdefault("CARGO_TERM_COLOR", "never")
    main()
