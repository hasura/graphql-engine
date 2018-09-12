import fett
from docutils import statemachine
from docutils.utils.error_reporting import ErrorString
from sphinx.util.compat import Directive
import yaml

# List of tabs ( ID, Display Name)
TABS_RAW = [
    ('linux', 'Linux'),
    ('mac', 'Mac'),
    ('windows', 'Windows'),
]
TABS_IDS = [tab[0] for tab in TABS_RAW]
TABS_DISPLAY = [tab[1] for tab in TABS_RAW]


class GlobalTabsDirective(Directive):
    has_content = True
    required_arguments = 0
    optional_arguments = 0
    final_argument_whitespace = True

    TABS_TEMPLATE = '''
.. raw:: html

   <div class="global-tabs">
     <ul class="tab-strip tab-strip--singleton" role="tablist">
       {{ for tab in tabs sortTabs }}
       <li class="tab-strip__element" data-tabid="{{ tab.id }}" role="tab" aria-selected="{{ if i zero }}true{{ else }}false{{ end }}">{{ tab.name }}</li>
       {{ end }}
     </ul>
     <div class="tabs__content" role="tabpanel">
       {{ for tab in tabs sortTabs}}
       <div class="tabpanel-{{ tab.id }}">

{{ tab.content }}

.. raw:: html

       </div>
       {{ end }}
     </div>
   </div>
'''

    def run(self):
        contents = '\n'.join(self.content)

        try:
            data = yaml.safe_load(contents)
        except yaml.YAMLError as error:
            raise self.severe(u'Error parsing YAML:\n{}.'.format(ErrorString(error)))

        raw_template = fett.Template(self.TABS_TEMPLATE)
        try:
            rendered_template = raw_template.render(data)
        except Exception as error:
            raise self.severe('Failed to render template: {}'.format(ErrorString(error)))

        rendered_lines = statemachine.string2lines(rendered_template, 4, convert_whitespace=1)

        self.state_machine.insert_input(rendered_lines, '')

        return []


def setup(app):
    app.add_directive('global-tabs', GlobalTabsDirective)

    return {'parallel_read_safe': True,
            'parallel_write_safe': True}


def numberOfTabs(tabData):
    return len(TABS_RAW)


fett.Template.FILTERS['numberOfTabs'] = numberOfTabs


def getTabNames(tabData):
    for tab in tabData:
        index = TABS_IDS.index(tab['id'])
        tab['name'] = TABS_DISPLAY[index]

    return tabData


fett.Template.FILTERS['getTabNames'] = getTabNames


def sortTabs(tabData):
    # Create a list for the sorted data
    sorted_tabs = [None] * len(TABS_RAW)

    for tab in tabData:
        index = TABS_IDS.index(tab['id'])
        tab['name'] = TABS_DISPLAY[index]
        sorted_tabs[index] = tab

    # Fill in any missing tabs with empty content
    for index in range(len(sorted_tabs)):
        if sorted_tabs[index] is None:
            sorted_tabs[index] = {'id': TABS_IDS[index], 'name': TABS_DISPLAY[index], 'content': ''}

    return sorted_tabs


fett.Template.FILTERS['sortTabs'] = sortTabs
