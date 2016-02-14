
import re
import os
import glob

import SCons
import SCons.Tool

#Decider('MD5-timestamp')

sconstoolsPath = os.environ['SCONSTOOLS']

def Env(tools = None):
    if not tools:
        tools = ['default']
        for t in glob.glob(os.path.join(sconstoolsPath, '*.py')):
            base = SCons.Util.splitext(os.path.basename(t))[0]
            if base == 'utils':
                continue
            tools.append(base)
    env = Environment(tools = tools, toolpath = [sconstoolsPath], ENV = {'PATH' : os.environ['PATH'], 'HOME' : os.environ['HOME']})
    return env

def newext(name, ext = False):
    (base, suffix) = SCons.Util.splitext(name)
    if ext:
        return base + '.' + ext
    return base

def grepfile(filename, regex):
    f = open(filename, 'r')
    for line in f:
        g = re.search(regex, line)
        if g:
            return g.group(1)
    f.close()
    return False

def beamer(basename):
    orig=basename+'.tex'
    env.LATEXER(orig)
    env.Command(basename+'_handout.tex', basename+'.tex', 
            'sed \'s/documentclass.*{beamer}/documentclass{article}\\n\\\\usepackage{beamerarticle}\\n\\\\usepackage{a4wide}/g\' $SOURCE > $TARGET')
    env.LATEXER(basename+'_handout.tex')
    origcontents = env.File(orig).get_contents()
    for re in env['LATEXERREGEXES']:
        if re[0].findall(origcontents):
            for se in re[1]:
                env.SideEffect(basename+'_handout.'+se, basename+'_handout.pdf')
    env.Clean(orig, basename+'_handout.tex')

def beamerAddDep(basename, dep):
    env.Depends(basename+'.pdf', dep)
    env.Depends(basename+'_handout.pdf', dep)

def svg2eps(target, source):
    env.Command(target, source, 'inkscape -z -E $TARGET $SOURCE > /dev/null')

def svg2ps(target, source):
    env.Command(target, source, 'inkscape -z -P $TARGET $SOURCE > /dev/null')

def svg2pdf(target, source):
    env.Command(target, source, 'inkscape -z -A $TARGET $SOURCE > /dev/null')

def svg2png(target, source):
    env.Command(target, source, 'inkscape -z -e $TARGET $SOURCE > /dev/null')

def png2pdf(target, source):
    env.Command(target, source, 'png2pdf $SOURCE > $TARGET')

def noalpha(target, source):
    env.Command(target, source, 'convert $SOURCE -background white -flatten +matte $TARGET')

def useCommand(command, pars, name, src, dst):
    env.Command(name+dst, name+src, command+' '+pars)
    #env.Depends(name+dst, command)

def doallCommand(command, pars, src, dst):
    for name in glob.glob('*'+src):
        name = name.replace(src, '', 1)
        useCommand(command, pars, name, src, dst)

def ps2pdf(basename):
    env.Command(name+'.pdf', name+'.ps', 'epstopdf $SOURCE')

# vim: ft=scons

