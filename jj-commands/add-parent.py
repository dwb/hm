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
    '-o', f'{args.revision}- ~ ::trunk()', # existing non-trunk parents
    '-o', f'latest({args.revision}- & ::trunk())', # latest trunk change
    '-o', args.parent, # new change
], check=True)
