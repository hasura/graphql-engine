import yaml
import re
import os

# Match '${env_name}'
path_matcher = re.compile(r'\$\{([^}^{]+)\}')

def path_constructor(loader, node):
    ''' 
    Extract the name of the environmental variable, 
    and replace it with its value
    '''
    value = node.value
    match = path_matcher.match(value)
    env_var = match.group()[2:-1]
    return os.environ[env_var] + value[match.end():]

def yaml_process_env_vars_conf():
    '''
    Add resolver and constructor required for 
    processing environmental vairables in yaml
    '''
    yaml.add_implicit_resolver('!env', path_matcher, None, yaml.SafeLoader)
    yaml.add_constructor('!env', path_constructor, yaml.SafeLoader)

yaml_process_env_vars_conf()

def load(f):
    '''Perform safe load'''
    return yaml.safe_load(f)

def dump(o):
    '''Dump yaml without aliases'''
    return yaml.dump(o, Dumper=ExplicitDumper)

class ExplicitDumper(yaml.SafeDumper):
    """
    A dumper that will never emit aliases.
    """

    def ignore_aliases(self, data):
        return True
