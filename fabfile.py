import os
from collections import namedtuple
from itertools import ifilter, chain
from fabric.api import task, local, env # , run,
from fabric.context_managers import cd, lcd
from fabric.colors import green, red
from fabric.contrib import files
import fabutils
from operator import methodcaller

Dependency = namedtuple('Dependency', ['name', 'url', 'branch'])

DEPENDENCY_ROOT = 'deps'
LIB_ROOT = 'lib'

DEPENDENCIES = [
    Dependency(name='mochiweb',
               url='git://github.com/mochi/mochiweb.git',
               branch='v2.4.1')
    ]


def get_default_path_list():
    return chain(fabutils.get_subdirs(DEPENDENCY_ROOT),
                 fabutils.get_subdirs(LIB_ROOT))


def get_apps():
    return [fabutils.OTPApp(path) for path in
            chain(fabutils.get_subdirs(DEPENDENCY_ROOT),
                  fabutils.get_subdirs(LIB_ROOT))]


@task
def update_deps():
    with lcd(DEPENDENCY_ROOT):
        for dep in DEPENDENCIES:
            with lcd(dep.name):
                local('git pull origin %s:master' % dep.branch)


@task
def compile():
    # for app in get_apps():
    #     app.compile()
    local('rebar compile')


@task
def clean():
    for app in get_apps():
        app.clean()

STARTUP = ['application:start(sasl)',
           'c_store:init()',
           'chat:start()',
           'ok = http:start()',
           'appmon:start()',
           # 'chat:create_room(1)',
           # 'chat:create_user(1, 2)',
           # 'chat:send_message(1,2,<<"3">>)',
           ]

MNESIA_DIR = '/tmp/mnesia'
NODE_NAME = 'dev_node'


@task
def shell():
    compile()
    path_list = (app.get_path() for app in get_apps())
    include_list = (app.get_include_path() for app in get_apps())
    local(('erl -pa %(path)s -i %(include_path)s -eval "%(startup)s" '
          '-mnesia dir \'"%(mnesia_dir)s"\' '
          '-sname %(node_name)s') % {'path': ' '.join(path_list),
                                     'include_path': ' '.join(include_list),
                                      'startup': ', '.join(STARTUP),
                                      'mnesia_dir': MNESIA_DIR,
                                      'node_name': NODE_NAME})


@task
def eunit():
    local('rebar skip_deps=true eunit')


@task
def ct():
    local('rebar skip_deps=true ct')
