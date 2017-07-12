from jinja2 import Template, Environment, FileSystemLoader
import os
import sys

def render(tpl_path, context):
    path, filename = os.path.split(tpl_path)
    return Environment(
        loader=FileSystemLoader(path or './')
    ).get_template(filename).render(context)

context = {
    'locations': sys.argv[1].strip().split(" ")
}

result = render('./script/conf/nginx.conf.j2', context)

file_path = "./_build/conf/nginx.conf"
directory = os.path.dirname(file_path)
try:
    if not os.path.exists(directory):
        os.makedirs(directory)
except OSError as e:
    raise

with open("./_build/conf/nginx.conf", "wb") as fh:
    fh.write(result)
