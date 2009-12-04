#!/usr/bin/env python
from __future__ import with_statement
import os
import re
import pwd
import sqlite3
import sys
from subprocess import Popen, PIPE
from colors import fgcolor, bgcolor

UNKNOWN = "(unknown)"
SYSTEMS = []


def _shell_escape(string):
    """
    Escape double quotes, backticks and dollar signs in given ``string``.

    For example::

    >>> _shell_escape('abc$')
    'abc\\\\$'
    >>> _shell_escape('"')
    '\\\\"'
    """
    for char in ('"', '$', '`'):
        string = string.replace(char, '\%s' % char)
    return string


def vcs(function):
    SYSTEMS.append(function)
    return function


@vcs
def bzr(path):
    file = os.path.join(path, '.bzr/branch/last-revision')
    if not os.path.exists(os.path.join(path, file)):
        return None
    with open(file, 'r') as f:
        line = f.read().split(' ', 1)[0]
        return 'bzr:r' + (line or UNKNOWN)


@vcs
def fossil(path):
    # In my five minutes of playing with Fossil this looks OK
    file = os.path.join(path, '_FOSSIL_')
    if not os.path.exists(file):
        return None

    repo = UNKNOWN
    conn = sqlite3.connect(file)
    c = conn.cursor()
    repo = c.execute("""SELECT * FROM
                        vvar WHERE
                        name = 'repository' """)
    repo = repo.fetchone()[1].split('/')[-1]
    return "fossil:" + repo


@vcs
def hg(path):
    file = os.path.join(path, '.hg/branch')
    if not os.path.exists(os.path.join(path, file)):
        return None
    with open(file, 'r') as f:
        line = f.read()
        return 'hg:' + (line or UNKNOWN)


@vcs
def git(path):
    prompt = "git:"
    branch = UNKNOWN
    file = os.path.join(path, '.git/HEAD')
    if not os.path.exists(file):
        return None

    with open(file, 'r') as f:
        line = f.read()
        if re.match('^ref: refs/heads/', line.strip()):
            branch = (line.split('/')[-1] or UNKNOWN)
    return prompt + branch


@vcs
def svn(path):
    # I'm not too keen on calling an external script
    # TODO find a way to do this in pure Python without the svn bindings
    revision = UNKNOWN
    if not os.path.exists(os.path.join(path, '.svn')):
        return None
    _p = Popen(['svnversion', path], stdout=PIPE)
    revision = _p.communicate()[0]
    return 'svn:r' + revision

def vc_prompt(path=None):
    path = path or os.getcwd()
    looped = end = False
    while True:
        if looped:
            path = path.rsplit('/', 1)[0]
        else:
            looped = True
        if not path:
            if not end:
                end = True
                path = '/'
            else:
                return ""

        # get vcs
        prompt = ''
        for vcs in SYSTEMS:
            prompt = vcs(path)
            if prompt:
                return prompt.strip()

def user_prompt():
    try:
        uid = os.getuid()
        username = pwd.getpwuid(uid).pw_name
    except KeyError:
        uid = 0
        username = 'nobody'

    return fgcolor('%s@%s' % (username, os.uname()[1]), uid == 0 and 'red' or 'brown', light=True)

def directory_prompt():
    return fgcolor(os.path.abspath(os.curdir), 'blue', light=True)

def __find_virtualenv_directory():
    directory = os.path.abspath(os.curdir)
    while True:
        if os.path.exists(os.path.join(directory, 've', 'bin', 'activate')):
            return directory
        directory = os.path.dirname(directory)
        if directory == os.path.dirname(directory):
            return None

def virtualenv_prompt():
    prompt = ''
    directory = __find_virtualenv_directory()
    if directory:
        prompt = 'virtualenv: %s' % os.path.basename(directory)
    return prompt

def virtualenv_prompt_command():
    directory = __find_virtualenv_directory()
    path_parts = os.getenv('PATH', '').split(':')
    path_parts = filter(lambda x: not x.endswith(os.path.join('ve', 'bin')), path_parts)
    response = ''
    if directory:
        bin_dir = os.path.join(directory, 've', 'bin')
        path_parts = [bin_dir] + path_parts
        response += 'export VIRTUAL_ENV="%s";' % _shell_escape(os.path.join(directory, 've'))
    else:
        response += 'unset VIRTUAL_ENV;'

    response += 'export PATH=%s;' % _shell_escape(':'.join(path_parts))
    return response

def fabfile_prompt():
    directory = os.path.abspath(os.curdir)
    prompt = ''
    fabfile = os.path.join(directory, 'fabfile.py')
    if os.path.exists(fabfile):
        if not os.getenv('VIRTUAL_ENV') and 'def bootstrap()' in open(fabfile, 'r').read():
            prompt = 'You need to run "fab bootstrap" to initialize your environment.'
        else:
            prompt = 'You may run "fab --list" to see available commands.'
    return prompt

def prompt():
    version_control = vc_prompt()
    virtualenv = virtualenv_prompt()
    fabfile = fabfile_prompt()
    ps1 = r'\n%s%s%s%s%s%s\n%s' % (
        fgcolor(fabfile and r'%s\n' % fabfile or '', 'red'),
        fgcolor(virtualenv and r'%s\n' % virtualenv or '', 'purple', light=True),
        fgcolor(version_control and r'%s ' % version_control or '', 'green'),
        fgcolor(user_prompt(), 'brown', light=True),
        fgcolor(':', 'white'),
        fgcolor(directory_prompt()),
        fgcolor('$ ', 'black', light=True),
        )
    return 'export PS1="%s";' % _shell_escape(ps1)

def prompt_command():
    return '%s%s' % (
        virtualenv_prompt_command(),
        prompt(),
        )

if __name__ == '__main__':
    print prompt_command()
