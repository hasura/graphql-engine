import sys
import os
import json
import requests

from algoliasearch import algoliasearch

APPLICATION_ID = os.environ["ALGOLIA_APPLICATION_ID"]
SEARCH_ONLY_KEY = os.environ["ALGOLIA_SEARCH_KEY"]
ADMIN_KEY = os.environ["ALGOLIA_ADMIN_KEY"]

client = algoliasearch.Client(APPLICATION_ID, ADMIN_KEY)

algolia_read_endpoint = "https://" + APPLICATION_ID + "-dsn.algolia.net"
algolia_write_endpoint = "https://" + APPLICATION_ID + ".algolia.net"
algolia_search_headers = {"X-Algolia-API-Key": SEARCH_ONLY_KEY, "X-Algolia-Application-Id": APPLICATION_ID}
#algolia_admin_headers = {"X-Algolia-API-Key": ADMIN_KEY, "X-Algolia-Application-Id": APPLICATION_ID}

def manage_indexes(index_name, data, settings):
    '''
    manage index with settings
    '''
    index = client.init_index(index_name)

    index_delete = client.delete_index(index_name)
    #index.clear_index()

    docs_by_desc = client.init_index("docs_by_date_desc")

    index = client.init_index(index_name)

    index.set_settings(
        {
            "searchableAttributes": ["title", "category", "content", "tags",
            "description", "keywords", "url", "image"],
            "replicas": [
                "docs_by_date_desc",
            ],
            "distinct": 1,
            "attributeForDistinct": "title"
        })

    # index.set_settings({
    #     "ranking": [
    #         "desc(created_at)",
    #     ]
    # });

    docs_by_desc.set_settings({
        "searchableAttributes": ["category", "content", "title", "tags",
            "description", "keywords", "url", "image"]
    })

    docs_by_desc.set_settings({
        "ranking": [
            "desc(created_at)",
        ]
    })

    index.add_objects(data)

    for setting in settings:
        index.set_settings(setting)

    #settings = index.get_settings()
    #print(settings)

def read_indexed_data(index_name):
    browse_url = algolia_read_endpoint + "/1/indexes/" + index_name + "/browse"
    #params_encode = requests.utils.quote("Data")
    browse_data = {}
    browse_content = requests.post(browse_url, data=json.dumps(browse_data), headers=algolia_search_headers)
    print ('Heere')
    print(browse_content.text)


def docs_index(data_source):
    '''
    make a search/filter query to fetch content
    '''
    index_name = "docs_search"

    #recreate_index if settings changed
    json_data = open(data_source, 'r')
    json_d = json.loads(json_data.read())

    settings = []
    # print (json_d)
    newJson = []

    for n in json_d:
        if ( len ( n['content'] ) < 20000 ):
            newJson.append(n)
        else:
            newObj = json.loads(json.dumps(n))

            MAX_LENGTH = 18000

            splittedContents = [ newObj['content'][i:i+MAX_LENGTH] for i in range(0, len(newObj['content']), MAX_LENGTH)]

            for elem in splittedContents:
                newObj = json.loads(json.dumps(n))
                newObj['content'] = elem
                newJson.append(newObj)

    manage_indexes(index_name, newJson, settings)
    read_indexed_data(index_name)

# docs_index('./sample.json')

if __name__ == "__main__":
    if len(sys.argv[1:]) == 0:
        print ("INDEX FILE REQUIRED!! usage: algolia_index.py <index_file>")
        exit(0)
    docs_index( sys.argv[1] )
