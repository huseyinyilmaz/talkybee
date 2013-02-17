import os
from fabric.api import task, local, env, lcd
from fabric.utils import error
from itertools import ifilter
from fabric.colors import green, red


def get_subdirs(d):
    """Returns immediate subdirectories of given path"""
    #get subdirectory and file paths
    subs = (os.path.join(d, name) for name in os.listdir(d))
    return ifilter(os.path.isdir, subs)


def get_subfiles(d):
    """Returns immediate subdirectories of given path"""
    #get subdirectory and file paths
    print(red(d))
    subs = (os.path.join(d, name) for name in os.listdir(d))
    return ifilter(os.path.isfile, subs)


def lisdir(path):
    """ local is dir"""
    return os.path.isdir(os.path.join(env['lcwd'], path))


def lexists(path):
    """ local exists"""
    return os.path.exists(os.path.join(env['lcwd'], path))


class OTPApp(object):

    def __init__(self, path):
        if not lisdir(path):
            error(red("Otp Application [%s] does not exists." %
                      path))
        self.root_dir = path
        self.src_dir = os.path.join(path, 'src')
        self.ebin_dir = os.path.join(path, 'ebin')
        self.include_dir = os.path.join(path, 'include')

    def ensure_structure(self):
        with lcd(self.root_dir):
            if not lisdir('ebin'):
                local('mkdir ebin')
            if not lisdir('src'):
                local('mkdir src')
            if not lisdir('include'):
                local('mkdir include')

    def update_app_file(self):
        with lcd(self.root_dir):
            app_sources = [f for f in get_subfiles(self.src_dir)
                           if f.endswith('.app.src')]
            for app in app_sources:
                _, file_name = os.path.split(app)
                new_name = file_name[:-4]
                local('cp %s %s' % (
                        os.path.join('src', file_name),
                        os.path.join('ebin', new_name)))

    def compile(self):
        print(green('Compiling %s' % self.root_dir))
        self.ensure_structure()
        self.update_app_file()
        with lcd(self.root_dir):
            local('erlc -o ebin -I include src/*.erl')

    def clean(self):
        with lcd(self.ebin_dir):
            local('rm *')

    def get_path(self):
        return self.ebin_dir

    def get_include_path(self):
        return self.include_dir
