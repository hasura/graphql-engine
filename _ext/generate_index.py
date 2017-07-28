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
import os
import json
from bs4 import BeautifulSoup

import datetime

indexObjs = []

def callme(app, exception):
    file_path = "./_build/algolia_index/index.json"
    directory = os.path.dirname(file_path)
    try:
        if not os.path.exists(directory):
            os.makedirs(directory)
    except OSError as e:
        raise
    f = open(file_path, 'w+')
    f.write(json.dumps(indexObjs))

def generateIndexFile(app, pagename, templatename, context, doctree):

    title = ''
    keyword = ''
    description = ''
    tagsVal = ''
    createdVal = 0

    if ( 'title' in context ):
        title = context['title']

    if ( 'metatags' in context ):
        metatags = context['metatags']
        if ( len(metatags) > 0 ):
            soup = BeautifulSoup(metatags, 'html.parser')
            descriptions = soup.findAll("meta", { "name" : "description" })
            keywords = soup.findAll("meta", { "name" : "keywords" })
            tags = soup.findAll("meta", { "name": "content-tags" })
            created_at = soup.findAll("meta", { "name": "created-on" })

            if ( len(descriptions) > 0 ):
                description = descriptions[0]['content']

            if ( len(keywords) > 0 ):
                keyword = keywords[0]['content']

            if ( len(tags) > 0 ):
                tagsVal = tags[0]['content']

            if ( len ( created_at ) > 0 ):
                createdVal = created_at[0]['content']
                createdVal = datetime.datetime.strptime(createdVal, "%Y-%m-%dT%H:%M:%S.%fZ")
                createdVal = createdVal.timestamp()
            else:
                createdVal = 0

    content = ''
    image = ''

    # If the page name is not the part of the below list
    if ( pagename not in ['ref/index', 'tutorials/index', 'guides/index', 'search', 'genindex'] ):
        if ( 'body' in context ):
            content = context['body']

            soup = BeautifulSoup(content, 'html.parser')

            imgs = soup.findAll("img", { "class" : "featured-image" })

            if ( len(imgs) > 0 ):
                image = imgs[0]['src'].split('/')[-1]

        url = pagename + '.html'
        category = pagename.split('/')[0]

        indexObj = { "title": title, "content": content, "url": url, "category": category, "image": image, "description": description, "keywords": keyword, "tags": tagsVal, "created_at": createdVal }

        indexObjs.append(indexObj)

def setup(app):
    app.connect('build-finished', callme)
    app.connect('html-page-context', generateIndexFile)
