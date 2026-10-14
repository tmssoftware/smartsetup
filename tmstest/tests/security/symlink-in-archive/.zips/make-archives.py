# Regenerates evil_symlink.zip and evil_symlink.tar.zst from the files in ../.src
# Each archive contains the product files plus a symlink "evil-link" pointing to "..\evil".
# Usage: python -I make-archives.py   (needs the zstd command line tool in the PATH)

import io
import os
import subprocess
import tarfile
import time
import zipfile

LINK_NAME = "evil-link"
LINK_TARGET = "..\\evil"

here = os.path.dirname(os.path.abspath(__file__))
src = os.path.join(here, "..", ".src")


def source_files():
    for root, _, files in os.walk(src):
        for f in sorted(files):
            full = os.path.join(root, f)
            yield full, os.path.relpath(full, src).replace("\\", "/")


def make_zip(path):
    with zipfile.ZipFile(path, "w", zipfile.ZIP_DEFLATED) as zf:
        for full, name in source_files():
            zf.write(full, name)
        # A symlink in a zip is an entry "made by" unix, with S_IFLNK in the external attributes and the target as content.
        info = zipfile.ZipInfo(LINK_NAME)
        info.create_system = 3
        info.external_attr = 0o120777 << 16
        zf.writestr(info, LINK_TARGET)


def make_tar_zst(path):
    data = io.BytesIO()
    with tarfile.open(fileobj=data, mode="w", format=tarfile.GNU_FORMAT) as tf:
        for full, name in source_files():
            tf.add(full, name)
        info = tarfile.TarInfo(LINK_NAME)
        info.type = tarfile.SYMTYPE
        info.linkname = LINK_TARGET
        info.mode = 0o777
        info.mtime = int(time.time())
        tf.addfile(info)
    subprocess.run(["zstd", "-q", "-f", "-19", "-o", path], input=data.getvalue(), check=True)


make_zip(os.path.join(here, "evil_symlink.zip"))
make_tar_zst(os.path.join(here, "evil_symlink.tar.zst"))
print("Archives created in " + here)
