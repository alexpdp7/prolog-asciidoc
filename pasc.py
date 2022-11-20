import argparse
import json
import pathlib
import os
import sys
import tempfile

from swiplserver import PrologMQI, PrologThread


def call_prolog_parser(f):
    with PrologMQI() as mqi:
        with mqi.create_thread() as prolog_thread:
            prolog_path = pathlib.Path(__file__).parent / "prolog"
            prolog_thread.query(f'consult("{prolog_path}/document")')
            result = prolog_thread.query(f'parse_file("{f}", X)')
            assert result, f"parsing failed"
            return [r["X"] for r in result]


def prolog_to_object(p):
    if isinstance(p, list):
        return prolog_list_to_object(p)
    if isinstance(p, str):
        return p
    return FUNCTOR_TO_CLASS[p["functor"]](*p["args"])


def prolog_list_to_object(l):
    res = []
    acc = ""
    for e in l:
        if isinstance(e, str):
            acc += e
        else:
            if acc:
                res.append(Text(acc))
                acc = ""
            res.append(prolog_to_object(e))
    if acc:
        res.append(Text(acc))
    return res


def parse_asciidoc(f, annotate_pos=True):
    prolog_trees = call_prolog_parser(f)
    result = []
    for tree in prolog_trees:
        r = prolog_to_object(tree)
        with open(f, encoding="utf8") as h:
            assert r.as_str() == h.read(), f"parsed:\n--{r.as_str()}--\ndid not match original file\nr: {NodeJSONEncoder().encode(r)}"
        if annotate_pos:
            r.annotate_pos()
        result.append(r)
    return result


class Node:
    def as_dict(self):
        return {
            "type": type(self).__name__,
            **self.__dict__,
        }

    def as_str(self):
        return _node_as_str(self)

    def annotate_pos(self):
        _annotate_pos(self, PositionTracker())

    def _set_pos(self, begin, end):
        self.pos = {
            "begin": {
                "character": begin.character,
                "line": begin.line,
                "column": begin.column,
            },
            "end": {
                "character": end.character,
                "line": end.line,
                "column": end.column,
            }
        }


def _annotate_pos(n, position_tracker):
    begin = position_tracker
    end = position_tracker
    if isinstance(n, str):
        end = end.make_next(n)
    elif isinstance(n, list):
        for el in n:
            end = _annotate_pos(el, end)
    elif isinstance(n, Node):
        for c in n.components():
            end = _annotate_pos(c, end)
        n._set_pos(begin, end)
    else:
        assert False, n
    return end

class PositionTracker:
    def __init__(self, character=0, line=0, column=0):
        self.character = character
        self.line = line
        self.column = column

    def make_next(self, text):
        next_character = self.character
        next_line = self.line
        next_column = self.column
        for ch in text:
            next_character += 1
            if ch == "\n":
                next_line += 1
                next_column = 0
            else:
                next_column += 1

        return PositionTracker(next_character, next_line, next_column)
            

        
def _node_as_str(n):
    if isinstance(n, str):
        return n
    if isinstance(n, list):
        return "".join(map(_node_as_str, n))
    if isinstance(n, Node):
        return _node_as_str(n.components())
    assert False, n

    
class NodeJSONEncoder(json.JSONEncoder):
    def default(self, o):
        if isinstance(o, Node):
            return o.as_dict()
        return json.JSONEncoder.default(self, o)


class Document(Node):
    def __init__(self, header, blocks):
        self.header = prolog_to_object(header)
        self.blocks = prolog_to_object(blocks)

    def components(self):
        return [self.header, self.blocks]

class Header(Node): 
    def __init__(self, header_lines, header_separator):
        self.header_lines = prolog_to_object(header_lines)
        if self.header_lines == "empty":
            self.header_lines = []
        assert header_separator == len(header_separator) * ["\n"]
        self.header_separator = header_separator

    def components(self):
        return [self.header_lines, self.header_separator]

class AttributeEntry(Node):
    def __init__(self, opening, attribute_name, attribute_value, closing):
        assert opening == [":"]
        self.attribute_name = "".join(attribute_name)
        assert attribute_value == "nothing"
        assert closing == [":", "\n"]

    def components(self):
        return [":", self.attribute_name, ":\n"]


class BlockSeparator(Node):
    def __init__(self, content):
        assert content == ["\n", "\n"]

    def components(self):
        return ["\n\n"]


class UnorderedList(Node):
    def __init__(self, items):
        self.items = prolog_to_object(items)

    def components(self):
        return [self.items]

class UnorderedListItem(Node):
    def __init__(self, bullet, sep, content):
        assert bullet == ["*"]
        assert sep == [" "]
        self.content = prolog_to_object(content)

    def components(self):
        return ["* ", self.content]


class ListSeparator(Node):
    def __init__(self, content):
        assert content == ["\n"]

    def components(self):
        return ["\n"]


class Paragraph(Node):
    def __init__(self, content):
        self.content = prolog_to_object(content)

    def components(self):
        return [self.content]


class ConstrainedFormattingMark(Node):
    def __init__(self, open, content, close):
        self.open = "".join(open)
        self.content = prolog_to_object(content)
        self.close = "".join(close)

    def components(self):
        return [self.open, self.content, self.close]

class UnconstrainedFormattingMark(Node):
    def __init__(self, open, open2, content, close, close2):
        assert open == open2
        self.open = "".join(open) + "".join(open2)
        self.content = prolog_to_object(content)
        assert close == close2
        self.close = "".join(close) + "".join(close2)

    def components(self):
        return [self.open, self.content, self.close]

class Text(Node):
    def __init__(self, text):
        self.text = text

    def components(self):
        return [self.text]

FUNCTOR_TO_CLASS = {
    "doc": Document,
    "h": Header,
    "attr_en": AttributeEntry,
    "bs": BlockSeparator,
    "uol": UnorderedList,
    "uoli": UnorderedListItem,
    "ls": ListSeparator,
    "para": Paragraph,
    "cfm": ConstrainedFormattingMark,
    "ucfm": UnconstrainedFormattingMark,
}


def parse_from_args():
    if sys.argv[1] == "-":
        stdin = sys.stdin.read()
        with tempfile.NamedTemporaryFile(delete=False) as stdin_file:
            stdin_file.write(stdin.encode("utf8"))
        try:
            docs = parse_asciidoc(stdin_file.name)
        finally:
            os.remove(stdin_file.name)
    else:
        docs = parse_asciidoc(sys.argv[1])
    return docs


def main():
    docs = parse_from_args()
    if len(docs) > 1:
        print("Ambiguous parse")
        for i, doc in enumerate(docs):
            with open(f"ambiguous.{i}", "w") as f:
                f.write(NodeJSONEncoder().encode(doc))
        sys.exit(1)
    print(NodeJSONEncoder().encode(docs[0]))


if __name__ == "__main__":
    main()
