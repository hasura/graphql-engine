__all__ = ['get_config_vars', 'get_path']

try:
    # Python 2.7 or >=3.2
    from sysconfig import get_config_vars, get_path
except ImportError:
    from distutils.sysconfig import get_config_vars, get_python_lib
    def get_path(name):
        if name not in ('platlib', 'purelib'):
            raise ValueError("Name must be purelib or platlib")
        return get_python_lib(name=='platlib')
