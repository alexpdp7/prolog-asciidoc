import json
import os
import re
import sys
import tempfile

import pasc


def transform_document(d):
    return {
        "body": list(filter(None, map(transform, d.blocks))),
    }


def transform_paragraph(p):
    return {
        "type": "Paragraph",
        "lines": convert_texts(p.content),
    }


def convert_texts(ts):
    return list(map(convert_text, ts))


def convert_text(t):
    return re.sub(r"\s+", " ", t.text).strip()


TRANSFORMERS = {
    pasc.Document : transform_document,
    pasc.Paragraph: transform_paragraph,
    pasc.BlockSeparator: lambda _: None,
}


def transform(e):
    return TRANSFORMERS[type(e)](e)


def main():
    docs = pasc.parse_from_args()
    assert len(docs) == 1, "ambiguous parse"
    doc = docs[0]
    print(json.dumps(transform(doc)))


if __name__ == "__main__":
    main()
