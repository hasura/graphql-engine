import yaml
import subprocess

APPS_DATA = {}
with open(".circleci/community-apps.yaml", 'r') as stream:
    try:
        APPS_DATA = yaml.load(stream)
    except yaml.YAMLError as exc:
        print(exc)
print(APPS_DATA)

for app in APPS_DATA['apps']:
    print(app)
    subprocess.call(['.circleci/community-apps-test.sh', '{}'.format(app['name']), app['path'], app['dockerfile']])
