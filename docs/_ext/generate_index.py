# -*- coding: utf-8 -*-
from sphinx import addnodes

"""
``generate_index``: A callable yielding the global TOC tree that contains
list of all the content below the specified page. ``generate_index`` need
pagename specifying as like as ``{{ generate_index(pagename) }}`` and
optional keyword arguments are available:

* maxdepth (defaults to the max depth selected in the toctree directive):
  the maximum depth of the tree; set it to -1 to allow unlimited depth
"""

import os
import json
from bs4 import BeautifulSoup

import datetime
import calendar

import re

import xml.etree.ElementTree as ET

indexObjs = []


def check_directory(path):
    directory = os.path.dirname(path)
    try:
        if not os.path.exists(directory):
            os.makedirs(directory)
    except OSError as e:
        raise


def on_finish_building(app, exception):
    current_version = app.env.config["version"]
    if "latest_docs_version" in app.env.config["html_context"].keys():
        latest_version = app.env.config["html_context"]["latest_docs_version"]
    else:
        latest_version = "dev"
    base_domain = app.env.config["html_context"]["SITEMAP_DOMAIN"]

    index_file_path = "./_build/algolia_index/index.json"
    sitemap_path = "./_build/sitemap/sitemap_" + current_version + ".xml"

    check_directory(index_file_path)
    check_directory(sitemap_path)

    f = open(index_file_path, 'w+')

    root = ET.Element("urlset")
    root.set("xmlns", "http://www.sitemaps.org/schemas/sitemap/0.9")

    for link in indexObjs:
        url = ET.SubElement(root, "url")
        ET.SubElement(url, "loc").text = base_domain + str(current_version) + "/" + link["url"]
        ET.SubElement(url, "changefreq").text = "daily"
        ET.SubElement(url, "priority").text = "1" if (current_version == latest_version) else "0.5"

    ET.ElementTree(root).write(sitemap_path)

    f.write(json.dumps(indexObjs))


def generate_index_file(app, pagename, templatename, context, doctree):
    # If the page name is not part of the below list and is present in toc-tree
    if (pagename not in ['manual/index', 'index', 'search', 'genindex']
            and not (pagename.startswith("ref/") or pagename.startswith("tutorials/") or pagename.startswith("guides/"))
            and re.search('<a[^>]*class="[^"]*current[^"]*"[^>]*>', context['toc_full'])):
        title = ''
        keyword = ''
        description = ''
        tags_val = ''
        content = ''
        image = ''
        created_val = 0

        if 'title' in context:
            title = context['title']

        if 'metatags' in context:
            metatags = context['metatags']
            if len(metatags) > 0:
                soup = BeautifulSoup(metatags, 'html.parser')
                descriptions = soup.findAll("meta", {"name": "description"})
                keywords = soup.findAll("meta", {"name": "keywords"})
                tags = soup.findAll("meta", {"name": "content-tags"})
                created_at = soup.findAll("meta", {"name": "created-on"})

                if len(descriptions) > 0:
                    description = descriptions[0]['content']

                if len(keywords) > 0:
                    keyword = keywords[0]['content']

                if len(tags) > 0:
                    tags_val = tags[0]['content']

                if len(created_at) > 0:
                    created_val = created_at[0]['content']
                    created_val = datetime.datetime.strptime(created_val, "%Y-%m-%dT%H:%M:%S.%fZ")
                    created_val = calendar.timegm(created_val.utctimetuple())
                else:
                    created_val = 0

        if 'body' in context:
            body = context['body']
            soup = BeautifulSoup(body, 'html.parser')

            content = soup.get_text()

            imgs = soup.findAll("img", {"class": "featured-image"})
            if len(imgs) > 0:
                image = imgs[0]['src'].split('/')[-1]

        url = pagename + '.html'
        category = pagename.split('/')[0]

        index_obj = {
            "title": title,
            "content": content,
            "url": url,
            "category": category,
            "image": image,
            "description": description,
            "keywords": keyword,
            "tags": tags_val,
            "created_at": created_val
        }

        indexObjs.append(index_obj)
    else:
        print('\t ** IGNORED FOR INDEXING **')


def setup(app):
    app.connect('build-finished', on_finish_building)
    app.connect('html-page-context', generate_index_file)
