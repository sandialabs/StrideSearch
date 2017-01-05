from setuptools import setup, Extension

ext = Extension(name="StrideSearchUtilities", sources = ["StrideSearchUtilities.cpp"], language="c++")

setup(name="StrideSearchUtilities", ext_modules=[ext])

