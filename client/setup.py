#!/usr/bin/env python3
from setuptools import setup

setup(name = 'arclient',
      version = '0.1',
      author = 'Damir Jelić',
      author_email = 'poljar[at]termina.org.uk',
      description = ('A simple plumber'),
      install_requires = ['urwid', 'drawille'],
      packages = ['arclient'],
      license = 'ISC',
      entry_points = {
          "console_scripts" : ['arclient = arclient.gui:main']
          }
     )
