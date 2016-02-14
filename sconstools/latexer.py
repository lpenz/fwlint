
import re

import SCons.Action
import SCons.Builder
import SCons.Util

bib_re = re.compile(r'^\s*\\bibliography{([^}]+)}', re.M)
input_re = re.compile(r'\\input\s*{(\S+)}', re.M)

side_res = [
    (bib_re, ['bbl', 'blg'])
    ,(re.compile(r'^\s*\\tableofcontents', re.M), ['toc'])
    ,(re.compile(r'^\s*\\documentclass.*{beamer}', re.M), ['nav', 'out', 'snm', 'toc'])
    ,(re.compile(r'^\s*\\makeindex', re.M), ['idx'])
    ,(re.compile(r'^\s*\\printindex', re.M), ['ilg', 'ind'])
    ,(re.compile(r'^\s*\\listoffigures', re.M), ['lof'])
    ,(re.compile(r'^\s*\\listoftables', re.M), ['lot'])
    ,(re.compile(r'^\s*\\listofalgorithms', re.M), ['loa'])
    ]

def checkContents(env, base, target, contents, input):
    for re in side_res:
        if re[0].findall(contents):
            for se in re[1]:
                env.SideEffect(base + '.' + se, target)

def checkInputs(env, base, target, contents, debug):
    for input in input_re.findall(contents):
        inputnode = env.File(input)
        inputcontents = inputnode.get_contents()
        checkInputs(env, base, target, inputcontents, input)
        checkContents(env, base, target, inputcontents, input)

def latexerEmitter(target, source, env):
    base = SCons.Util.splitext(str(source[0]))[0]
    for e in [ '.aux', '.log' ]:
        env.SideEffect(base + e, target)
    contents = source[0].get_contents()
    checkContents(env, base, target, contents, base)
    checkInputs(env, base, target, contents, base)
    return target, source

verbatim_re = re.compile(r'\\verbatiminput\s*{(\S+)}', re.M)

images_re = [
        re.compile(r'{([a-zA-Z0-9._]+\.pdf)}', re.M),
        re.compile(r'imagefit{([a-zA-Z0-9._]+)}', re.M),
        re.compile(r'imagenorm{([a-zA-Z0-9._]+)}', re.M),
]

def latexerScannerFunc(node, env, path):
    contents = node.get_contents()
    rv = []
    for input in input_re.findall(contents):
        rv.append(input)
        inputnode = env.File(input)
        inputcontents = inputnode.get_contents()
        for b in bib_re.findall(inputcontents):
            rv.append(b)
    for verbatim in verbatim_re.findall(contents):
        rv.append(verbatim)
    for b in bib_re.findall(contents):
        rv.append(b)
    for img_re in images_re:
        for i in img_re.findall(contents):
            rv.append(i)
    return rv

def generate(env):
    """Add Builders and construction variables for LATEXER to an Environment."""

    LatexerAction = SCons.Action.Action('$LATEXERCOM', '$LATEXERCOMSTR')

    LatexerScanner = SCons.Scanner.Base(
            name = 'latexerScanner',
            function = latexerScannerFunc,
            skeys = ['.tex'],
            recursive = True)

    bld = SCons.Builder.Builder(
            action = LatexerAction,
            src_suffix = '.tex',
            suffix = '.pdf',
            emitter = latexerEmitter,
            source_scanner = LatexerScanner)

    env['BUILDERS']['LATEXER'] = bld

    env['LATEXER']      = SCons.Util.CLVar('latexer -p -k')
    env['LATEXERCOM']   = '$LATEXER $SOURCE'
    env['LATEXERREGEXES'] = side_res

def exists(env):
    return env.Detect('LATEXER')

# vim: ft=scons

