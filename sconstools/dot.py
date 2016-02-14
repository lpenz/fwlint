
import SCons.Action
import SCons.Builder
import SCons.Util
import os

def dotEmitter(target, source, env):
    targetbase = SCons.Util.splitext(str(target[0]))[0]
    if target[0].get_suffix()[1:] == 'pdf':
        env.SideEffect(targetbase + '.ps', target[0])
    return target, source

def dotGenerator(source, target, env, for_signature):
    targetbase = SCons.Util.splitext(str(target[0]))[0]
    sourcebase = SCons.Util.splitext(str(target[0]))[0]
    if target[0].get_suffix()[1:] == 'pdf':
        return '${SOURCE.get_suffix()[1:]} $DOTFLAGS -Tps $SOURCE > %s && epstopdf %s'%(targetbase + '.ps', targetbase + '.ps')
    else:
        return '${SOURCE.get_suffix()[1:]} $DOTFLAGS -T${TARGET.get_suffix()[1:]} $SOURCE > $TARGET'

def generate(env):
    """Add Builders and construction variables for DOT to an Environment."""
    bld = SCons.Builder.Builder(
            emitter = dotEmitter,
            generator = dotGenerator,
            )

    env['BUILDERS']['DOT'] = bld

    env['DOT']      = 'dot'
    env['DOTFLAGS'] = SCons.Util.CLVar('')

def exists(env):
    return env.Detect('dot')

# vim: ft=scons

