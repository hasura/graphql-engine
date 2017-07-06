# -*- coding: utf-8 -*-
from sphinx import addnodes

"""
``local_toctree``: A callable yielding the global TOC tree that contains
list of all the content below the specified page. ``local_toctree`` need
pagename specifing as like as ``{{ local_toctree(pagename) }}`` and
optional keyword arguments are available:

* maxdepth (defaults to the max depth selected in the toctree directive):
  the maximum depth of the tree; set it to -1 to allow unlimited depth
"""

import docutils
import json
from bs4 import BeautifulSoup

indexObjs = []

def init_local_toctree(app):

    def _get_local_toctree(docname, **kwds):
        doctree = app.env.get_doctree(docname)
        if 'maxdepth' not in kwds:
            kwds['maxdepth'] = 0
        toctrees = []
        """
        print ('length of doctree\n\n')
        print ((doctree))
        print (doctree.__dict__)
        print (doctree.traverse())
        print('*** \n\n')
        """

        for toctreenode in doctree.traverse(addnodes.toctree):
            toctreenode.parent.remove(toctreenode)

        for pending_xhrefnode in doctree.traverse(addnodes.pending_xref ):
            pending_xhrefnode.parent.remove(pending_xhrefnode)

        # doctree = app.env.get_and_resolve_doctree(docname, app.builder, doctree)

        htmlString = docutils.core.publish_from_doctree(doctree, writer_name='html')
        soup = BeautifulSoup(htmlString, 'html.parser')

        docpath = app.env.doc2path(docname, None)

        imgs = soup.findAll("img", { "class" : "featured-image" })

        # if the imgs has something  return else return default image


        # title = soup.findAll('title')[0].text
        content = soup.findAll('body')[0].text
        url = '/' + docpath.split('.rst')[0] + '.html'
        category = docpath.split('/')[0]
        image = ''
        # descriptions = ''
        title = 'Hasura Authentication'
        description = 'Hasura authentication helps you implement sign/signup'

        if ( len(imgs) > 0 ):
            image = imgs[0]['src'].split('/')[-1]
            indexObj = { "title": title, "content": content, "url": url, "category": category, "image": image, "description": description }

            indexObjs.append(indexObj)

            print ('\n')

            # print (title)
            # print (content)
            # print (url)
            # print (category)
            # print (image)
            # print (descriptions)
            # print ('\n')
        else:
            indexObj = { "title": title, "content": content, "url": url, "category": category, "image": image, "description": description }

            indexObjs.append(indexObj)

        """
        for toctreenode in doctree.traverse(addnodes.toctree):
            toctree = app.env.resolve_toctree(
                            docname, app.builder, toctreenode, **kwds)
            toctrees.append(toctree)
        if not toctrees:
            return None
        result = toctrees[0]
        for toctree in toctrees[1:]:
            result.extend(toctree.children)

        print ('doctree \n\n')
        print (doctree)
        print ('\n')
        print ('\n\nresult is\n\n')
        print (result)
        print ('\n\n')
        print ('builder fragment is')
        print (app.builder.render_partial(result)['fragment'])
        return app.builder.render_partial(result)['fragment']
        """

    ctx = app.env.config['html_context']
    if 'local_toctree' not in ctx:
        ctx['local_toctree'] = _get_local_toctree


def callme(app, exception):
    f = open('./sample.json', 'w+')
    f.write(json.dumps(indexObjs))
    print('I am called')

def setup(app):
    app.connect('builder-inited', init_local_toctree)
    app.connect('build-finished', callme)
