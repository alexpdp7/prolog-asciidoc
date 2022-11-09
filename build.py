import itertools
import subprocess


def describe(long):
    long_args = ["--long"] if long else []
    description = subprocess.run(["git", "describe", "--all", "--always"] + long_args, check=True, encoding="utf8", stdout=subprocess.PIPE).stdout
    assert description.startswith("heads/")
    return description[len("heads/"):-1]  # remove heads/ and trailing newline


def is_dirty():
    returncode = subprocess.run(["git", "diff-index", "--quiet", "HEAD", "--"]).returncode
    assert returncode in (0,1), f"returncode is {returncode}"
    return returncode == 1


def build_image():
    tags = ["dirty"] if is_dirty() else [describe(False), describe(True)]
    for tag in tags:
        quay_expires_after = "never" if not is_dirty() and tag == "main" else "3d"
        subprocess.run(["podman", "build", ".", "--label", f"quay.expires-after={quay_expires_after}", "-t", f"prolog-asciidoc:{tag}"], check=True)
    return tags


def build_image_and_push():
    tags = build_image()
    for tag in tags:
        assert tag != "dirty", "attempting to push dirty image"
        subprocess.run(["podman", "push", f"prolog-asciidoc:{tag}", f"quay.io/alexpdp7/prolog-asciidoc:{tag}"], check=True)