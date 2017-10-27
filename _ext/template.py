import os.path
import re
import sys
import re

from docutils import nodes, statemachine, utils
from docutils.utils.error_reporting import ErrorString
from docutils.parsers.rst import directives
from sphinx.util.compat import Directive

import fett
import yaml

if sys.version_info.major >= 3:
    basestring = str

# Maps source filenames (.txt) to a set of directives registered within that
# scope. Makes sure that directives are always registered in each file, to
# prevent inconsistent errors.
REGISTERED = {}

PAT_SUBSTITUTION = re.compile(r'^\$[\w\.]+$')

BUILT_IN_PATH = '[built_in]'

def should_substitute(value):
    """Return true if a value should be substituted."""
    return isinstance(value, basestring) and PAT_SUBSTITUTION.match(value)


def populate(obj, options):
    """Apply reference substitutions and add magical values to objects."""
    if isinstance(obj, list):
        for i, item in enumerate(obj):
            if isinstance(item, dict):
                populate(item, options)
            elif should_substitute(item):
                obj[i] = options.get_foreign(item)
            else:
                populate(obj[i], options)
    elif isinstance(obj, dict):
        # Add references
        if 'inherit' in obj:
            foreign_dict = obj['inherit']
            for name, path in foreign_dict.items():
                path = options.get_asset_path(path)
                with options.open_file(path) as f:
                    try:
                        options.foreign[name] = yaml.safe_load(f)
                    except yaml.YAMLError as error:
                        raise self.severe(u'Error parsing inherited YAML file {}:\n{}.'.format(
                            path, ErrorString(error)))
                    populate(options.foreign[name], options)

        # Add magic values
        obj['heading'] = options.heading_character * 500

        # Resolve references
        for key, value in obj.items():
            if should_substitute(value):
                obj[key] = options.get_foreign(value)
            else:
                populate(value, options)


class Options:
    HEADING_LEVELS = ['=', '-', '~', '`', '^', '\'']

    def __init__(self, state, source_path, obj):
        self.project_root = os.path.dirname(os.path.abspath(source_path))
        self.level = 3
        self.state = state
        self.env = state.document.settings.env
        self.obj = obj

        # Foreign values inherited from another file.
        self.foreign = {}

    def get_foreign(self, path):
        """Returns a field in a defined reference field using a
           dot-delimited path."""
        if should_substitute(path):
            path = path[1:]

        obj = self.foreign
        components = path.split('.')
        for component in components:
            obj = obj[component]

        return obj

    def open_file(self, path):
        """Open a file and register it as a dependency."""
        path = os.path.normpath(path)
        path = utils.relative_path(None, path)
        path = nodes.reprunicode(path)
        self.state.document.settings.record_dependencies.add(path)
        return open(path, 'r')

    @property
    def heading_character(self):
        """Return the character associated with the current heading level."""
        return self.HEADING_LEVELS[self.level]

    def get_asset_path(self, path):
        _, path = self.env.relfn2path(path)
        return path


def create_directive(name, template, defined_in, is_yaml):
    template = fett.Template(template)

    # Quick-and-dirty fixup. In giza projects, people shouldn't be including
    # things underneath build/<branch>
    if defined_in.startswith('build/'):
        defined_in = '/'.join(defined_in.split('/')[2:])

    class CustomDirective(Directive):
        has_content = True
        required_arguments = 0
        optional_arguments = 1
        final_argument_whitespace = True
        option_spec = {'level': int}

        def run(self):
            source_path = self.state.document.get('source')
            if defined_in != BUILT_IN_PATH and (source_path not in REGISTERED or name not in REGISTERED[source_path]):
                raise self.severe(u'Unknown directive: {}. '
                                  u'Try including {}'.format(self.name, defined_in))

            contents = '\n'.join(self.content)

            if is_yaml:
                data = self.process_yaml(contents)
            else:
                title = self.arguments[0]
                data = {'directive': name, 'body': contents, 'title': title}

            try:
                rendered = template.render(data)
            except Exception as error:
                raise self.severe('Failed to render template: {}'.format(ErrorString(error)))

            rendered_lines = statemachine.string2lines(
                rendered, 4, convert_whitespace=1)
            self.state_machine.insert_input(rendered_lines, '')

            return []

        def process_yaml(self, contents):
            level = self.options.get('level', None)
            source_path = self.state_machine.input_lines.source(
                self.lineno - self.state_machine.input_offset - 1)
            try:
                data = yaml.safe_load(contents)
            except yaml.YAMLError as error:
                raise self.severe(u'Error parsing YAML:\n{}.'.format(ErrorString(error)))
            options = Options(self.state, source_path, data)

            if level:
                options.level = level

            try:
                populate(data, options)
            except IOError as error:
                raise self.severe(u'Problems with "{}" directive path:\n{}.'.format(
                    self.name, ErrorString(error)))

            return data


    return CustomDirective


def create_template_factory(app):
    class TemplateDefinition(Directive):
        has_content = True
        required_arguments = 1
        optional_arguments = 1
        final_argument_whitespace = True
        option_spec = {'yaml': directives.flag}

        def run(self):
            name = self.arguments[0]
            defined_in = self.state.document.current_source
            source_path = self.state.document.get('source')

            # Register this directive as existing in the current document.
            if not source_path in REGISTERED:
                REGISTERED[source_path] = set()

            if not name in REGISTERED[source_path]:
                template = '\n'.join(self.content)
                is_yaml = 'yaml' in self.options
                directive = create_directive(name, template, defined_in, is_yaml)
                app.add_directive(name, directive)

                REGISTERED[source_path].add(name)

            return []

    return TemplateDefinition


def setup(app):
    app.add_directive('register-template', create_template_factory(app))

    return {'parallel_read_safe': True,
            'parallel_write_safe': True}


