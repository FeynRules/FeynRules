#! /usr/bin/env python

import os
import logging
import sys
import getopt
import commands
import datetime
import subprocess
import time

class CommitTool:
 
    def __init__(self,svntag):
        self.user = os.getlogin()
        self.release = ""
        now = time.localtime()
        self.date = time.strftime('%d %B %Y',now)
        self.dateshort = time.strftime('%d-%m-%y',now)
        self.svnlog = ""
        self.svntag = svntag+1
 
    def ExtractRelease(self,line):

        # Treat the line before splitting
        line=line.replace('=',' = ')
        line=line.replace('"',' " ')
        line=line.replace('.',' . ')
        line.lstrip()

        # Line splitting in words
        line=line.split()

        # Extract the version number
        try:
            version = int(line[len(line)-3])
        except:
            print "Error: impossible to read the version number from "
            print "-- " + save + " --"
            self.release = "UNKNOWN"
            return False

        # Increment the version number
        version+=1
        line[len(line)-3]=str(version)

        # Get the complete version number
        self.release=""
        self.release= line[len(line)-7] + '.' + line[len(line)-5] + '.' + \
           line[len(line)-3]

        return True        

    def UpdateFR(self):
               
           # Opening the FeynRulesPackage.m file as a read-only file
           try:
                input = open('FeynRulesPackage.m','r')
           except:
                print 'Error: file "FeynRulesPackage.m" not found'
                return False

           # Opening the FeynRulesPackage.m as a write-only file
           try:
                output = open('FeynRulesPackage.m_tmp~','w')
           except:
                print 'Error: impossible to create the temporary file ' + \
               'FeynRulesPackage.m_tmp~'
                return False

           # Reading the current FeynRulesPackage.m file
           ok=True 
           for line in input:
                if line.startswith('FR$VersionNumber = '):
                    ok=self.ExtractRelease(line)
                    output.write('FR$VersionNumber = "' + self.release + '";\n')
                elif line.startswith('FR$VersionDate = '):
                    output.write('FR$VersionDate = "' + self.date + '";\n')
                else:
                    output.write(line)

           # Closing files
           input.close()
           output.close()
           return ok

    def UpdateChangelog(self,log):
           # Opening UpdateNotes.txt as a write-only file
           try:
               output = open('UpdateNotes.txt_tmp~','w')
           except:
               print 'Error: impossible to create the temporary file ' + \
                     'UpdateNotes.txt_tmp~'
               return False

           # Writing output header
           output.write('Update notes for FeynRules 2.4.x\n')
           output.write('\n')

           # Make lines of 45 character line
           lines=[]
           currentline=""
           counter=0
           for word in log:
                if len(word)>=45:
                    if currentline!="":
                        lines.append(currentline)
                    lines.append(word)
                    currentline=""
                else:
                    if (len(currentline)+len(word)+1) >= 45:
                        lines.append(currentline)
                        currentline=""
                    currentline+=word+" "
           if currentline!="":
                lines.append(currentline)

           # Keep the first line for the svn tag
           self.svnlog = lines[0]
           if len(lines)!=1:
               self.svnlog += " ..."

           # First line
           for i in range(0,len(lines)):
               if i==0:
                   output.write('%-4s' % str(self.svntag))
                   output.write(' ')
                   output.write('%-5s' % self.release)
                   output.write(' ')
                   output.write('%-8s' % self.dateshort)
                   output.write(' ')
                   output.write('%-12s' % (self.user.replace("LeMouth","bfuks")+':'))
               else:
                   output.write('%-33s' % '')
               output.write(lines[i]+'\n')

           # Opening UpdateNotes.txt as a read-only file 
           IsThereInput=False
           try:
               input = open('UpdateNotes.txt','r')
               IsThereInput=True
           except:
               pass

           # Write files
           if IsThereInput:
               for line in input:
                   if not line.startswith('Update notes for FeynRules 2.4.x\n'):
                       output.write(line)
               input.close()

           # Close the file
           output.close()

           return True            
    
    def EnterLog(self):
        endofloop = False

        # Asking for log message
        while not endofloop:
            text=raw_input("Enter a message describing the changes\n(only the first characters are passed to the svn)\n")
            print "-------------------------------------------------------"
            print text
            print "-------------------------------------------------------"
            print "Does the message suit you ? (Y/N)"
            allowed_answers=['n','no','y','yes']
            answer=""
            while answer not in allowed_answers:
                answer=raw_input("Answer: ")
                answer=answer.lower()
                if answer=="yes" or answer=="y":

                    # Processing log text
                    text = text.lstrip()
                    log = text.split()

                    # Check empty log
                    if len(log)==0:
                        print "Error: the message is empty. Please enter a message!"
                    else:
                        endofloop=True

        # Return log
        return log

# ------------------------------------------------------------------------------
#                                   MAIN PROGRAM
# ------------------------------------------------------------------------------

# Displaying header    
print "################################"
print "     FeynRules commit tool"
print "################################"

# Updating
print "Updating the files from SVN repository ..."
os.system("svn update")

# Displaying the changes of the local folder vs the repository
print "Displaying status ..."
print "-------------------------------------------------------"
os.system("svn info")
print "-------------------------------------------------------"
os.system("svn status")
print ""
print "Legend: ? = unknown files"
print "        M = modified files"
print "        A = added files (ready to be committed)"
print "        ! = incomplete directories (svn update required)"
print "Reminder: commit.py ignore ? and ! files/directories."
print "-------------------------------------------------------"

# Get version
svnversion = commands.getoutput('svnversion .')
versions = svnversion.split(':')
if len(versions)==0:
    sys.exit("Error: problem with the version number to be commited '"+svnversion+"'")
try:
    theversion=int(versions[-1])
except:
    try:
        theversion=int(versions[-1][:-1])
    except:
        sys.exit("Error: problem with the version number to be commited '"+svnversion+"'")
print 'Current svn version = ' + str(theversion) + '; next svn version = ' + str(theversion+1)  
print ''
print "Are you sure to continue and commit ? (Y/N)"
allowed_answers=['n','no','y','yes']
answer=""
while answer not in allowed_answers:
    answer=raw_input("Answer: ")
    answer=answer.lower()
if answer=="no" or answer=="n":
    sys.exit()

# Modifying FeynRulesPackage.m
updater = CommitTool(theversion)
print "Extracting a new release number and the current date ..."
updater.UpdateFR()
if not updater.UpdateFR():
    sys.exit()

# Asking for a commit messsage
log = updater.EnterLog()

# Writing a new change log file
print "Adding a new entry in UpdateNotes.txt."
if not updater.UpdateChangelog(log):
    sys.exit()

# Replacing the current files by the new ones  
print "Saving changes and committing..."
print "Log message passed to the svn: " + updater.svnlog
os.system("mv FeynRulesPackage.m_tmp~ FeynRulesPackage.m")
os.system("mv UpdateNotes.txt_tmp~ UpdateNotes.txt") 
os.system('svn commit -m "' + updater.svnlog + '"')
print "Done"
print ""
 
