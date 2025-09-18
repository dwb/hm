import argparse
import subprocess
import sys

# don't bother with traceback
sys.tracebacklimit = 0

parser = argparse.ArgumentParser(
                    prog='jj-add-parent',
                    description='Adds a parent to the given revision')

parser.add_argument('-r', '--revision', default='@', help='default @')
parser.add_argument('parent')

args = parser.parse_args()

subprocess.run([
    'jj', 'rebase', '-s', args.revision,
    '-o', f'parents({args.revision}) ~ ::trunk()', # existing non-trunk parents
    '-o', f'latest((parents({args.revision}) | {args.parent}) & ::trunk())', # latest trunk change, considering new parent
    '-o', f'{args.parent} ~ ::trunk()', # new parent, if non-trunk
], check=True)
