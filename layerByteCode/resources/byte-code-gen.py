#!/bin/env python3

from os import listdir, walk, mkdir
from os.path import isfile, join, isdir
from subprocess import Popen
import sys

PHP_PATH = "/usr/bin/php"

def createByteCodeFile(filePath, shadowFilePath):
    with open(shadowFilePath,'w') as f:
        process = Popen(["/usr/bin/php7",
                         "-d","opcache.enable_cli=1",
                         "-d","opcache.opt_debug_level=0x50000",
                         "-d","opcache.log_verbosity_level=0",
                         "--syntax-check", filePath], stdout=f, stderr=f)
        exitCode = process.wait()
        if exitCode != 0:
            print("Converting file {} resulted in exit code {}".format(filePath, exitCode))


def traverseFolder(baseFolder, shadowFolder):
    print("traversing folder {}".format(baseFolder))
    for elems in listdir(baseFolder):
        if isfile(join(baseFolder,elems)):
            if join(baseFolder,elems).endswith(".php"):
                createByteCodeFile(join(baseFolder, elems),
                                   join(shadowFolder, "{}.byte".format(elems)))
        if isdir(join(baseFolder,elems)):
            if(shadowFolder != baseFolder):
                mkdir(join(shadowFolder, elems))
            traverseFolder(join(baseFolder, elems),
                           join(shadowFolder, elems))


def main(baseFolder, shadowFolder):
    traverseFolder(baseFolder, shadowFolder)


if(len(sys.argv) != 3):
    print("./byte-code-gen.py [projectFolder] [shadowFolder]")
    print("   [projectFolder] the folder of the project to be converted to bytecode")
    print("    [shadowFolder] the folder in which the byte code files should be saved")
    print("                   and imitates the folder structure of [projectFolder]")
else:
    main(sys.argv[1], sys.argv[2])
