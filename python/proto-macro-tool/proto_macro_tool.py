#!/usr/bin/env python

# format:
#   - autopep8 --in-place --aggressive proto_macro_tool.py

# usage examples:
#   - python3 proto_macro_tool.py alias_files mylib -s ~/tmp/proto-macro-test -f ~/tmp/proto-macro-test/a.mli ~/tmp/proto-macro-test/a.ml ~/tmp/proto-macro-test/b.ml ~/tmp/proto-macro-test/quux/c.ml -o ~/tmp/proto-macro-tool-test-out/
#   - python3 proto_macro_tool.py alias_map mylib -s ~/tmp/proto-macro-test -f ~/tmp/proto-macro-test/a.mli ~/tmp/proto-macro-test/a.ml ~/tmp/proto-macro-test/b.ml ~/tmp/proto-macro-test/quux/c.ml -o ~/tmp/proto-macro-tool-test-out/mylib.mli

import argparse
import os
import shutil
import sys


class ProtoMacroTool(object):
    def __init__(self):
        parser = argparse.ArgumentParser(
            usage='''proto_macro_tool.py <command> [<args>]
Commands:
alias_files    Alias modues (e.g. copy 'a.ml',.. to 'mylib__A.ml',..).
alias_map      Generate an alias map ('.mli' file) (e.g. write 'module A = Mylib__A',..).
''')
        parser.add_argument('command', help='proto_macro_tool.py commands')
        parser.add_argument(
            '-v',
            '--version',
            help='show version and   exit',
            action='version',
            version='1.0')
        args = parser.parse_args(sys.argv[1:2])
        getattr(self, args.command)()

    def alias_files(self):
        parser = argparse.ArgumentParser(description='generate alias modules')
        parser.add_argument('lib', help='lib name e.g. mylib')
        parser.add_argument(
            '-o',
            '--out',
            required=True,
            help='output directory')
        parser.add_argument(
            '-s',
            '--srcdir',
            required=True,
            help='source root directory')
        parser.add_argument(
            '-f',
            '--files',
            required=True,
            nargs='*',
            help='source modules')
        args = parser.parse_args(sys.argv[2:])

        lib = args.lib
        srcdir = os.path.join(args.srcdir, '')
        out = os.path.join(args.out, '')
        files = [f for f in args.files if os.path.isfile(f)]

        cmd_alias_files(lib, srcdir, out, files)

    def alias_map(self):
        parser = argparse.ArgumentParser(
            description='generate modue alias map')
        parser.add_argument('lib', help='lib name e.g. mylib')
        parser.add_argument(
            '-s',
            '--srcdir',
            required=True,
            help='source root directory')
        parser.add_argument(
            '-o',
            '--out',
            required=True,
            help='output map filename (.mli)')
        parser.add_argument(
            '-f',
            '--files',
            required=True,
            nargs='*',
            help='source modules')
        args = parser.parse_args(sys.argv[2:])

        lib = args.lib
        srcdir = os.path.join(args.srcdir, '')
        out = open(args.out, 'w')
        files = [f for f in args.files if os.path.isfile(f)]

        cmd_alias_map(lib, srcdir, out, files)

# --


def cmd_alias_files(lib, srcdir, out, files):
    for src, dst in zip(files, [os.path.join(os.path.dirname(f.replace(
            srcdir, out)), lib + "__" + os.path.basename(f).capitalize()) for f in files]):
        d = os.path.dirname(dst)
        if not os.path.isdir(d):
            os.makedirs(d, exist_ok=True)
        shutil.copyfile(src, dst)
        print("%s -> %s" % (src, dst))


def cmd_alias_map(lib, _srcdir, out, files):
    lib = lib.capitalize()
    modules = {os.path.splitext(os.path.basename(f).capitalize())[
        0]: None for f in files}
    content = ''.join(["module {} = {}__{}\n".format(m, lib, m)
                      for m in modules.keys()])
    out.write(content)
    print("%s" % content)

# --


if __name__ == '__main__':
    ProtoMacroTool()
