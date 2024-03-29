#!/usr/bin/env python

import os
import logging
import sys
import getopt
import commands
import datetime
import subprocess
import time
import shutil

class PackageTool:

    def __init__(self,name):
        self.packagename = name
        self.fulldir = ''
        self.currentdir = os.getcwd()
        self.release = ''
        self.user = os.getlogin()

    def CreateTmp(self):

        user = os.getlogin()
        try:
            tmpdir = os.environ['TMPDIR']
        except:
            tmpdir = '/tmp'
        self.fulldir = tmpdir+'/fr_'+user

        # display the temporary directory
        print " -> Current directory   : " + self.currentdir
        print " -> Temporary directory : " + self.fulldir

        # if the directory exists, remove it
        if os.path.isdir(self.fulldir):
            try:
                shutil.rmtree(self.fulldir)
            except:
                print "impossible to remove the previous directory with the same name"
                sys.exit()
            
        # create the directory
        try:
            os.mkdir(self.fulldir)
        except:
            print "impossible to create the directory"
            sys.exit()

        return True

    def CopyFiles(self):
        shutil.copytree("Core/",self.fulldir+'/Core')
        shutil.copytree("Interfaces/",self.fulldir+'/Interfaces')
        shutil.copytree("Models/",self.fulldir+'/Models')
        shutil.copy("ToolBox.m",self.fulldir+"/ToolBox.m")
        shutil.copy("FeynRules.m",self.fulldir+"/FeynRules.m")
        shutil.copy("NLOCT.m",self.fulldir+"/NLOCT.m")
        shutil.copy("FeynRulesPackage.m",self.fulldir+"/FeynRulesPackage.m")
        shutil.copy("UpdateNotes.txt",self.fulldir+"/UpdateNotes.txt")
        shutil.copy("FRPalette.nb",self.fulldir+"/FRPalette.nb")

    def RemoveSVNtag(self):
        pltfrm = sys.platform
        if "darwin" in pltfrm:
            os.system('cd '+self.fulldir+'; find . -type d -name .svn -exec rm -rf {} +')
        else:
            os.system('cd '+self.fulldir+'; find -type d -name .svn -exec rm -rf {} +')

    def ExtractRelease(self,line):

        # Treat the line before splitting
        line=line.replace('=',' = ')
        line=line.replace('"',' " ')
        line=line.replace('.',' . ')
        line.lstrip()

        # Line splitting in words
        line=line.split()

        # Extract the release number
        try:
            version = int(line[len(line)-2])
        except:
            print "Error : impossible to read the release number from : "
            print "-- " + save + " --"
            self.release = "UNKNOWN"
            return False

        # release number 
        line[len(line)-2]=str(version)

        # Get the release
        WriteTag=False
        self.release=""
        for word in line:
            if word=='"' and not WriteTag:
                WriteTag=True
            elif word=='"' and WriteTag:
                WriteTag=False
            elif WriteTag:
                self.release += word

        return True        

    def DumpDate(self):
        # Get date and hour
        now = time.localtime()
        date  = time.strftime('%Y/%m/%d',now)
        clock = time.strftime('%H:%M:%S',now)
        
        # Get SVN tag
        svnversion = commands.getoutput('cd '+self.currentdir+' ; svnversion .')

    def Tar(self):
        os.system('cd '+self.fulldir+'; tar czf '+self.currentdir+'/'+self.packagename+'.tgz *')





# ------------------------------------------------------------------------------
#                                   MAIN PROGRAM
# ------------------------------------------------------------------------------

# Displaying header    
print "################################"
print "     FeynRules package"
print "################################"
print ""

if len(sys.argv)!=2:
    print "Error with the number of arguments"
    print "The correct syntax is the following : "
    print "./package.py name"
    print "which leads to the cration of the tarball 'name.tgz'"
    sys.exit()

name=sys.argv[1]
if name.endswith('.tgz'):
    name=name[:-4]


print "Tarball to create : '"+name+".tgz'"
print ""

pack = PackageTool(name)
print "Creating temporary folder ..."
pack.CreateTmp()
print "Copying files ..."
pack.CopyFiles()
print "Removing SVN tags ..."
pack.RemoveSVNtag()
print "Creating the tar ball ..."
pack.Tar()
print "Done !"
