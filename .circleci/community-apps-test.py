import yaml
import subprocess

APPS_DATA = {}
with open(".circleci/community-apps.yaml", 'r') as stream:
    try:
        APPS_DATA = yaml.load(stream)
    except yaml.YAMLError as exc:
        print(exc)

for app in APPS_DATA['apps']:
    print("Running test for app {}".format(app['name']))
    subprocess.call(['.circleci/community-apps-test.sh', '{}'.format(app['name']), app['path'], app['dockerfile']])
