# -*- coding: utf-8 -*-
from sphinx import addnodes

"""
``generate_index``: A callable yielding the global TOC tree that contains
list of all the content below the specified page. ``generate_index`` need
pagename specifing as like as ``{{ generate_index(pagename) }}`` and
optional keyword arguments are available:

* maxdepth (defaults to the max depth selected in the toctree directive):
  the maximum depth of the tree; set it to -1 to allow unlimited depth
"""

import docutils
import json
from bs4 import BeautifulSoup

indexObjs = []

def init_generate_index(app):

    def _get_generate_index(docname, **kwds):
        print ('\n')
        print (docname)
        doctree = app.env.get_doctree(docname)
        if 'maxdepth' not in kwds:
            kwds['maxdepth'] = 0
        toctrees = []

        for toctreenode in doctree.traverse(addnodes.toctree):
            toctreenode.parent.remove(toctreenode)

        for pending_xhrefnode in doctree.traverse(addnodes.pending_xref ):
            pending_xhrefnode.parent.remove(pending_xhrefnode)

        doctree = app.env.get_and_resolve_doctree(docname, app.builder, doctree)

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
        title = 'Hasura Authentication'
        description = 'Hasura authentication helps you implement sign/signup'

        if ( len(imgs) > 0 ):
            image = imgs[0]['src'].split('/')[-1]
            indexObj = { "title": title, "content": content, "url": url, "category": category, "image": image, "description": description }

            indexObjs.append(indexObj)
        else:
            indexObj = { "title": title, "content": content, "url": url, "category": category, "image": image, "description": description }

            indexObjs.append(indexObj)

    ctx = app.env.config['html_context']
    if 'generate_index' not in ctx:
        ctx['generate_index'] = _get_generate_index


def callme(app, exception):
    f = open('./sample.json', 'w+')
    f.write(json.dumps(indexObjs))
    print('I am called')

def callmeAgain(app, pagename, templatename, context, doctree):
    print ('\n Values')
    print ('\n')
    print (pagename)
    print (templatename)
    print (context)
    # print (doctree)

    title = ''

    if ( 'title' in context ):
        title = context['title']

    content = ''

    image = ''

    if ( 'body' in context ):
        content = context['body']

        soup = BeautifulSoup(content, 'html.parser')

        imgs = soup.findAll("img", { "class" : "featured-image" })

        if ( len(imgs) > 0 ):
            image = imgs[0]['src'].split('/')[-1]

    url = pagename + '.html'
    category = pagename.split('/')[0]
    description = 'Sample description'

    indexObj = { "title": title, "content": content, "url": url, "category": category, "image": image, "description": description }

    indexObjs.append(indexObj)

def setup(app):
    app.connect('builder-inited', init_generate_index)
    app.connect('build-finished', callme)
    app.connect('html-page-context', callmeAgain)
