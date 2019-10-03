import sys
import os
import json
import time

from algoliasearch import algoliasearch

APPLICATION_ID = os.environ["ALGOLIA_APPLICATION_ID"]
ADMIN_KEY = os.environ["ALGOLIA_ADMIN_KEY"]
ALGOLIA_INDEX_NAME = os.environ["ALGOLIA_INDEX_NAME"]

client = algoliasearch.Client(APPLICATION_ID, ADMIN_KEY)
index = client.init_index(ALGOLIA_INDEX_NAME)


def update_index(data):
    index.clear_index()
    print("\nINDEX CLEARED!\n")

    index.add_objects(data)
    print("INDEX REPOPULATED!\n")


def output_indexed_data():
    res = index.browse_all({"query": ""})

    count = 0
    # print("INDEXED PAGES:")
    for hit in res:
        count += 1
        # print('\t' + hit['title'] + ' (' + hit['url'] + ')')

    print('\nTOTAL INDEXED: ' + str(count))


def process_data(json_data):
    processed_data = []

    CONTENT_MAX_LENGTH = 17500

    print('TRIMMED:')
    for json_obj in json_data:
        if len(json_obj['content']) < CONTENT_MAX_LENGTH:
            processed_data.append(json_obj)
        else:
            obj = json.loads(json.dumps(json_obj))

            split_content = [obj['content'][i:i + CONTENT_MAX_LENGTH] for i in
                             range(0, len(obj['content']), CONTENT_MAX_LENGTH)]

            for content_piece in split_content:
                obj = json.loads(json.dumps(json_obj))
                obj['content'] = content_piece
                processed_data.append(obj)
                break  # ignoring other pieces as unique title limitation

            print('\t' + obj['title'] + ' (' + obj['url'] + ')')

    return processed_data


def docs_index(data_source):
    json_data = open(data_source, 'r')
    json_d = json.loads(json_data.read())

    processed_json_d = process_data(json_d)

    update_index(processed_json_d)
    time.sleep(2)
    output_indexed_data()


if __name__ == "__main__":
    if len(sys.argv[1:]) == 0:
        print("INDEX FILE REQUIRED!! usage: algolia_index.py <index_file>")
        exit(0)

    docs_index(sys.argv[1])
