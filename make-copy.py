#!/usr/bin/env python3

from os import open, close, read, write, fsync, makedirs, chmod, rename, fstat, copy_file_range, utime, strerror, O_NOATIME, O_RDONLY, O_WRONLY, O_TMPFILE
from os.path import dirname, join
from tempfile import mkstemp
from stat import S_IRUSR
from fcntl import ioctl
try:
    from fcntl import FICLONE
except ImportError:
    FICLONE = (1<<30)|(4<<16)|(0x94<<8)|9
from errno import ENOTSUP, EXDEV, EINVAL

import sys
from contextlib import contextmanager

from ctypes.util import find_library
from ctypes import CDLL, get_errno, c_int, c_char_p

libc = CDLL(find_library("c"), use_errno=True)
AT_FDCWD = -100
AT_SYMLINK_FOLLOW = 0x400

linkat = libc.linkat
linkat.argtypes = [c_int, c_char_p, c_int, c_char_p, c_int]

def link(src, dst):
    if linkat(AT_FDCWD, src.encode(), AT_FDCWD, dst.encode(), AT_SYMLINK_FOLLOW) != 0:
        errno = get_errno()
        raise OSError(errno, strerror(errno))

@contextmanager
def open_fd(dst_dir, dst_path):
    dst = join(dst_dir, dst_path)
    try:
        fd = open(dst_dir, O_WRONLY | O_TMPFILE, mode=S_IRUSR)
    except OSError as e:
        if e.errno != ENOTSUP:
            raise
    else:
        try:
            yield fd
            link(f"/proc/self/fd/{fd}", dst)
        finally:
            close(fd)
        return

    tmpdir = join(dst_dir, "tmp")
    makedirs(tmpdir, mode=0o755, exist_ok=True)
    fd, path = mkstemp(prefix='', dir=tmpdir)
    chmod(path, S_IRUSR)
    try:
        yield fd
        rename(path, dst)
    finally:
        close(fd)

def copy_file(src_fd, dst_fd, size):
    try:
        ioctl(dst_fd, FICLONE, src_fd)
    except OSError as e:
        if e.errno not in (ENOTSUP, EXDEV):
            raise
    else:
        return

    try:
        while size:
            size -= copy_file_range(src_fd, dst_fd, size)
    except OSError as e:
        if e.errno not in (EINVAL, EXDEV):
            raise
    else:
        return

    while True:
        data = read(src_fd, 4096)
        if not data:
            return
        write(dst_fd, data)

def make_copy(src_path, dst_dir, path):
    src_fd = open(src_path, O_RDONLY|O_NOATIME)
    st = fstat(src_fd)
    try:
        with open_fd(dst_dir, path) as dst_fd:
            copy_file(src_fd, dst_fd, st.st_size)
            fsync(dst_fd)
        utime(join(dst_dir, path), ns=(st.st_atime_ns, st.st_mtime_ns))
    finally:
        close(src_fd)

if __name__ == '__main__':
    make_copy(sys.argv[1], sys.argv[2], sys.argv[3])
