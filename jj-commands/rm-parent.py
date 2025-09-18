import argparse
import subprocess
import sys

# don't bother with traceback
sys.tracebacklimit = 0

parser = argparse.ArgumentParser(
                    prog='jj-rm-parent',
                    description='Removes a parent from the given revision')

parser.add_argument('-r', '--revision', default='@', help='default @')
parser.add_argument('parent')

args = parser.parse_args()

subprocess.run([
    'jj', 'rebase', '-s', args.revision,
    '-d', f'all:{args.revision}- ~ {args.parent}',
], check=True)
