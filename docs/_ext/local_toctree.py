# -*- coding: utf-8 -*-
from sphinx import addnodes

"""
``local_toctree``: A callable yielding the global TOC tree that contains
list of all the content below the specified page. ``local_toctree`` need
pagename specifying as like as ``{{ local_toctree(pagename) }}`` and
optional keyword arguments are available:

* maxdepth (defaults to the max depth selected in the toctree directive):
  the maximum depth of the tree; set it to -1 to allow unlimited depth
"""


def init_local_toctree(app):
    def _get_local_toctree(docname, **kwds):
        doctree = app.env.get_doctree(docname)
        if 'maxdepth' not in kwds:
            kwds['maxdepth'] = 0
        toctrees = []
        for toctreenode in doctree.traverse(addnodes.toctree):
            toctree = app.env.resolve_toctree(
                docname, app.builder, toctreenode, **kwds)
            toctrees.append(toctree)
        if not toctrees:
            return None
        result = toctrees[0]
        for toctree in toctrees[1:]:
            result.extend(toctree.children)
        return app.builder.render_partial(result)['fragment']

    ctx = app.env.config['html_context']
    if 'local_toctree' not in ctx:
        ctx['local_toctree'] = _get_local_toctree


def setup(app):
    app.connect('builder-inited', init_local_toctree)
