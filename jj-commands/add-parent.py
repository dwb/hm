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
    'jj', 'rebase', '-s', args.revision, '-d', f'{args.revision}-',
    '-d', args.parent,
], check=True)
subprocess.run(['jj', 'simplify-parents', '-r', args.revision], check=True)
