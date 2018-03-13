import re
import fett
import template

PAT_RST_SECTION = re.compile(r'(.*)\n((?:^----+$)|(?:^====+$)|(?:^~~~~+$)|(?:^````+$))', re.M)
# List of tuples with language tab ( ID, Display Name)
LANGUAGES_RAW = [
    ('linux', 'Linux'),
    ('mac', 'Mac'),
    ('windows', 'Windows'),
    # ('shell', 'Shell'),
    # ('python', 'Python'),
    # ('nodejs', 'Nodejs'),
    # ('haskell', 'Haskell'),
    # ('go', 'Go'),
    # ('java-async', 'Java (Async)'),
    # ('c', 'C'),
    # ('cpp11', 'C++11'),
    # ('csharp', 'C#'),
    # ('perl', 'Perl'),
    # ('ruby', 'Ruby'),
    # ('scala', 'Scala')
]
LANGUAGES_IDS = [lang[0] for lang in LANGUAGES_RAW]
LANGUAGES_DISPLAY = [lang[1] for lang in LANGUAGES_RAW]

TABS_TEMPLATE = '''
.. raw:: html

   <div class="tabs">
     <ul class="tab-strip tab-strip--singleton" role="tablist">
       {{ for tab in tabs sortLanguages }}
       {{ # Only render the tab here if i < 5 }}
       {{ if i lessThan(5) }}
       <li class="tab-strip__element" data-tabid="{{ tab.id }}" role="tab" aria-selected="{{ if i zero }}true{{ else }}false{{ end }}">{{ tab.name }}</li>
       {{ end }}
       {{ end }}
       {{ if tabs numberOfLanguages greaterThan(5) }}
       <li class="tab-strip__element dropdown">
         <a class="dropdown-toggle" data-toggle="dropdown">Other <span class="caret"></span></a>
         <ul class="dropdown-menu tab-strip__dropdown" role="menu">
           {{ for tab in tabs sortLanguages }}
           {{ # Only render the tab here if i >= 5 }}
           {{ if i greaterThanOrEqual(5) }}
           <li data-tabid="{{ tab.id }}" aria-selected="{{ if i zero }}true{{ else }}false{{ end }}">{{ tab.name }}</li>
           {{ end }}
           {{ end }}
         </ul>
       </li>
       {{ end }}
     </ul>
     <div class="tabs__content" role="tabpanel">
       {{ for tab in tabs sortLanguages}}
       <div class="tabpanel-{{ tab.id }}">

{{ tab.content convertSections }}

.. raw:: html

       </div>
       {{ end }}
     </div>
   </div>
'''

TABSGS_TEMPLATE = '''
.. raw:: html

   <div class="tabs">
     <ul class="tab-strip tab-strip--singleton" role="tablist">
       {{ for tab in tabs }}
       {{ # Only render the tab here if i < 5 }}
       {{ if i lessThan(5) }}
       <li class="tab-strip__element" data-tabid="{{ tab.id }}" role="tab" aria-selected="{{ if i zero }}true{{ else }}false{{ end }}">{{ tab.name }}</li>
       {{ end }}
       {{ end }}
     </ul>
     <div class="tabs__content" role="tabpanel">
       {{ for tab in tabs }}
       <div class="tabpanel-{{ tab.id }}">

{{ tab.content convertSections }}

.. raw:: html

       </div>
       {{ end }}
     </div>
   </div>
'''

H1_TEMPLATE = '''
.. raw:: html

   <h1>{{ title }}</h1>
'''

H2_TEMPLATE = '''
.. raw:: html

   <h2>{{ title }}</h2>
'''

H3_TEMPLATE = '''
.. raw:: html

   <h3>{{ title }}</h3>
'''

H4_TEMPLATE = '''
.. raw:: html

   <h4>{{ title }}</h4>
'''


def setup(app):
    directive = template.create_directive('h1', H1_TEMPLATE, template.BUILT_IN_PATH, True)
    app.add_directive('h1', directive)

    directive = template.create_directive('h2', H2_TEMPLATE, template.BUILT_IN_PATH, True)
    app.add_directive('h2', directive)

    directive = template.create_directive('h3', H3_TEMPLATE, template.BUILT_IN_PATH, True)
    app.add_directive('h3', directive)

    directive = template.create_directive('h4', H4_TEMPLATE, template.BUILT_IN_PATH, True)
    app.add_directive('h4', directive)

    directive = template.create_directive('tabs', TABS_TEMPLATE, template.BUILT_IN_PATH, True)
    app.add_directive('tabs', directive)

    directive = template.create_directive('tabs-gs', TABSGS_TEMPLATE, template.BUILT_IN_PATH, True)
    app.add_directive('tabs-gs', directive)

    return {'parallel_read_safe': True,
            'parallel_write_safe': True}


def convertSections(tabContent):
    """Convert rst-style sections into custom directives that ONLY insert
       the HTML header tags."""
    return PAT_RST_SECTION.sub(
        lambda match: '.. h{}:: {}'.format(Options.HEADING_LEVELS.index(match.group(2)[0]) + 1, match.group(1)),
        tabContent)


fett.Template.FILTERS['convertSections'] = convertSections


def numberOfLanguages(tabData):
    return len(LANGUAGES_RAW)


fett.Template.FILTERS['numberOfLanguages'] = numberOfLanguages


def getLanguageNames(tabData):
    for tab in tabData:
        index = LANGUAGES_IDS.index(tab['id'])
        tab['name'] = LANGUAGES_DISPLAY[index]

    return tabData


fett.Template.FILTERS['getLanguageNames'] = getLanguageNames


def sortLanguages(tabData):
    # Create a list for the sorted data
    sorted = [None] * len(LANGUAGES_RAW)

    for tab in tabData:
        index = LANGUAGES_IDS.index(tab['id'])
        tab['name'] = LANGUAGES_DISPLAY[index]
        sorted[index] = tab

    # Fill in any missing languages with empty content
    for index in range(len(sorted)):
        if sorted[index] is None:
            sorted[index] = {'id': LANGUAGES_IDS[index], 'name': LANGUAGES_DISPLAY[index], 'content': ''}

    return sorted


fett.Template.FILTERS['sortLanguages'] = sortLanguages
