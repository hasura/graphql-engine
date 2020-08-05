import fett
from docutils import statemachine
from docutils.utils.error_reporting import ErrorString
from docutils.parsers.rst import Directive

class GraphiQLDirective(Directive):
    has_content = False
    required_arguments = 0
    optional_arguments = 0
    final_argument_whitespace = True
    option_spec = {"query": str, "response": str, "variables": str}

    GRAPHIQL_TEMPLATE = '''
.. raw:: html

   <div class="graphiql{{ if variables }} with-vars{{end}}">

   <div class="graphiql-input">
   
.. code-block:: graphql
   :class: graphiql-section graphiql-query

   {{ query }}

{{ if variables }}

.. raw:: html

   <div class="graphiql-title">Variables</div>

.. code-block:: json
   :class: graphiql-section graphiql-vars

   {{ variables }}
   
{{ end }}

.. raw:: html
    
    </div>

{{ if response }}
   
.. raw:: html

   <div class="graphiql-output">

.. code-block:: json
   :class: graphiql-section graphiql-response

   {{ response }}

.. raw:: html
    
    </div>
   
{{ end }}
 
.. raw:: html
   
   </div>
'''

    def run(self):
        raw_template = fett.Template(self.GRAPHIQL_TEMPLATE)
        try:
            rendered_template = raw_template.render(self.options)
        except Exception as error:
            raise self.severe('Failed to render template: {}'.format(ErrorString(error)))

        rendered_lines = statemachine.string2lines(rendered_template, 4, convert_whitespace=1)

        self.state_machine.insert_input(rendered_lines, '')

        return []

def setup(app):
    app.add_directive('graphiql', GraphiQLDirective)

    return {'parallel_read_safe': True,
            'parallel_write_safe': True}
