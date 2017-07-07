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

def callme(app, exception):
    f = open('./sample.json', 'w+')
    f.write(json.dumps(indexObjs))
    print('I am called')

def generateIndexFile(app, pagename, templatename, context, doctree):

    title = ''
    if ( 'title' in context ):
        title = context['title']
    content = ''
    image = ''

    # If the page name is not the part of the below list
    if ( pagename not in ['ref/index', 'tutorials/index', 'guides/index'] ):
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
    app.connect('build-finished', callme)
    app.connect('html-page-context', generateIndexFile)
