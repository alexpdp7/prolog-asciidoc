#!/usr/bin/env python3

import itertools
import shlex
import subprocess


def _run(cmd, check=True, cwd=None, **kwargs):
    print(f"> ({cwd}) $ {shlex.join(cmd)}", flush=True)
    return subprocess.run(cmd, **kwargs, check=check, cwd=cwd)


def describe(long):
    long_args = ["--long"] if long else []
    description = _run(["git", "describe", "--all", "--always"] + long_args, encoding="utf8", stdout=subprocess.PIPE).stdout
    assert description.startswith("heads/")
    return description[len("heads/"):-1]  # remove heads/ and trailing newline


def is_dirty():
    returncode = _run(["git", "diff-index", "--quiet", "HEAD", "--"], check=False).returncode
    assert returncode in (0,1), f"returncode is {returncode}"
    return returncode == 1


def build_images_and_push():
    tags = ["dirty"] if is_dirty() else [describe(False), describe(True)]
    for tag in tags:
        quay_expires_after = "never" if not is_dirty() and tag == "main" else "3d"
        _run(["podman", "build", ".", "--build-arg", f"QUAY_EXPIRES_AFTER={quay_expires_after}", "-t", f"prolog-asciidoc:{tag}"])
        assert tag != "dirty", "attempting to push dirty image"
        _run(["podman", "push", f"prolog-asciidoc:{tag}", f"quay.io/alexpdp7/prolog-asciidoc:{tag}"])
    return tags
