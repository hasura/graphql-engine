from selenium.webdriver.common.keys import Keys
from seleniumwire import webdriver
import pyperclip
import time
import json
import re
from ruamel.yaml.scalarstring import PreservedScalarString
from ruamel.yaml.comments import CommentedMap

class ElementNotFound(Exception):
    pass


def wait_for_elem(f, value, tries=20):
    if tries <=0:
        raise ElementNotFound("Could not find element by name " + value)
    elements = f(value)
    if elements:
        return elements[0]
    else:
        time.sleep(0.5)
        return wait_for_elem(f, value, tries-1)


def wait_for_element_by_xpath(driver, xpath, tries=20):
    return wait_for_elem(driver.find_elements_by_xpath, xpath, tries)


def wait_for_element_by_name(driver, name, tries=20):
    return wait_for_elem(driver.find_elements_by_name, name, tries)

def type_hge_pwd(driver, hge_key):
    pwdElem = wait_for_element_by_name(driver, "password", 60)
    pwdElem.send_keys("abcdef")
    pwdElem.submit()

def exec_query(driver):
    wait_for_element_by_xpath(driver, "//*[@class='execute-button']").click()

def get_query_editor(driver):
    return wait_for_element_by_xpath(driver,
        "//*[@class='query-editor']/div/div/textarea")

def get_variable_editor(driver):
    return wait_for_element_by_xpath(driver,
        "//*[@class='variable-editor']/div/div/div/textarea")

def get_response_textarea(driver):
    return wait_for_element_by_xpath(driver,
        "//*[@class='result-window']/div/div/textarea")

to_discard_headers = [
    'content-type',
    'x-hasura-access-key',
    'x-hasura-admin-secret',
    'authorization'
]

def get_headers(driver):
    headers = {}
    i = 0
    while i < 100:
        (key, value, checked) = get_header_at_index(driver, i)
        if not key:
            return headers
        elif checked and key.lower() not in to_discard_headers:
            headers[key] = str(value)
        i = i + 1

def get_header_at_index(driver, index):
    (key_form, val_form, checkbox) = get_header_forms_at_index(driver, index)
    if not key_form:
        return (None, None, False)
    (key, value) = [ elem.get_attribute('value') for elem in (key_form, val_form) ] 
    checked = checkbox.get_attribute('checked') if checkbox else False
    print(key, value, checked)
    return (key, value, checked)

def get_header_forms_at_index(driver, index):
    xpath_checkbox = "//*[contains(@data-header-id,{}) and contains(@type, 'checkbox')]".format(index)
    xpath_key   = "//*[contains(@data-test, 'header-key-{}')]".format(index)
    xpath_value = "//*[contains(@data-test, 'header-value-{}')]".format(index)
    if index <1:
        # The first header is Content-Type header, which should be present
        # Waiting for it to be avaialble
        key_forms = [wait_for_element_by_xpath(driver,xpath_key)]
    else:
        key_forms = driver.find_elements_by_xpath(xpath_key)
    if not key_forms:
        return (None, None, False)
    else:
        value_form = driver.find_elements_by_xpath(xpath_value)[0]
        checkboxes = driver.find_elements_by_xpath(xpath_checkbox)
        return (key_forms[0], value_form, checkboxes[0] if checkboxes else None)

def get_last_header_index(driver):
    i=0
    while i<100:
        (hdr_key_form, _, _) = get_header_forms_at_index(driver, i)
        if not hdr_key_form:
            return i-1
        i=i+1

def type_headers(driver, headers):
    i = get_last_header_index(driver)
    assert i > 0
    for (key, value) in headers.items():
        (key_form, value_form, _) = get_header_forms_at_index(driver, i)
        for (val, form) in [(key, key_form), (value, value_form)]:
            form.send_keys(val)
            time.sleep(0.1)
        i=i+1


def type_variables(driver, variables):
    varEditor = get_variable_editor(driver)
    set_text(varEditor, json.dumps(variables))

def type_query(driver, query):
    queryEditor = get_query_editor(driver)
    set_text(queryEditor, query)

def get_text(text_area):
    text_area.send_keys(Keys.CONTROL, "a")
    text_area.send_keys(Keys.CONTROL, "c")
    return pyperclip.paste()

def set_text(text_area, text):
    pyperclip.copy(text)
    text_area.send_keys(Keys.CONTROL, "a")
    text_area.send_keys(Keys.CONTROL, "v")

def prettify(driver):
    wait_for_element_by_xpath(driver,
        "//*[@class='topBar']/div[3]/a[1]").click()

def get_all_sub_elems(conf):

    all_objs = []

    def append_sub_elems_of(c):
        all_sub_elems = get_all_sub_elems(c)
        for elem in all_sub_elems:
            if not elem in all_objs:
                all_objs.append(elem)

    def make_anchor_refs_key(key, val, conf):
        conf[key] = next(e for e in all_objs if e == val)

    def make_anchor_refs(conf):
        if isinstance(conf, list):
            for (index, val) in enumerate(conf):
                make_anchor_refs_key(index, val, conf)
        elif isinstance(conf, dict):
            for (key, val) in conf.items():
                make_anchor_refs_key(key, val, conf)

    if isinstance(conf, list):
        for (i,c) in enumerate(conf):
            append_sub_elems_of(c)
            make_anchor_refs(c)

    if isinstance(conf, dict):
        for (k,v) in conf.items():
            append_sub_elems_of(v)
            make_anchor_refs(v)

    all_objs.append(conf)
    return all_objs

def make_all_possible_anchors(conf):
    return get_all_sub_elems(conf)

def apply_explorer_conf(driver, conf, max_wait=600):
    start_time = time.time()
    queryEditor = get_query_editor(driver)
    varEditor = get_variable_editor(driver)
    contents = ''
    while not contents.endswith('DONE') and time.time() - start_time < max_wait:
        contents = queryEditor.get_attribute('value')
        time.sleep(1)
    prettify(driver)
    exec_query(driver)
    time.sleep(1)
    query = get_text(queryEditor)
    variables = get_text(varEditor)
    conf['query'] = { 'query': PreservedScalarString(query) }
    try:
        variables = json.loads(variables, object_pairs_hook=CommentedMap)
        conf['query']['variables'] = variables
    except Exception:
        pass
    respTextArea = get_response_textarea(driver)
    response = get_text(respTextArea)
    conf['response'] = json.loads(response, object_pairs_hook=CommentedMap)
    headers = get_headers(driver)
    if headers:
        conf['headers'] = headers
    make_all_possible_anchors(conf)


def run_query_on_selenium(hge_ctx, conf):
    new_conf = conf
    driver = webdriver.Firefox()
    driver.rewrite_rules = [
        (r'(http://)localhost(.*)', r'\1hasura.me\2'),
        (r'(http://)127.0.0.1(.*)', r'\1hasura.me\2')
    ]
    query = conf.get('query',{})
    helpMsg = '''
# Type #DONE at the bottom of this box once
# you are done with making changes\n
'''
    try:
        url = hge_ctx.hge_url
        for (r,c) in driver.rewrite_rules:
            url = re.sub(r, c, url)

        driver.get(url + '/console/api-explorer')
        if hge_ctx.hge_key:
            type_hge_pwd(driver, hge_ctx.hge_key)
        type_query(driver, helpMsg + query.get('query',''))
        if 'variables' in query:
            type_variables(driver, query['variables'])
        if 'headers' in conf:
            type_headers(driver, conf['headers'])
        time.sleep(0.5)
        exec_query(driver)
        apply_explorer_conf(driver, new_conf)
        metaqs = get_all_metadata_queries(driver, url)
        return (metaqs, new_conf)
    finally:
        driver.close()

def get_all_metadata_queries(driver, url):
    all_reqs = []
    for request in driver.requests:
        if request.path == url + '/v1/query':
            try:
                body = json.loads(request.body, object_pairs_hook=CommentedMap)
                if body.get('type') == 'bulk':
                    for oper in body.get('args',[]):
                        if oper['type'] in ['select','count']:
                            continue
                        elif oper['type'] == 'run_sql':
                            sql = oper['args']['sql']
                            if 'hdb_catalog' in sql:
                                continue
                            if 'information_schema' in sql:
                                continue
                        all_reqs.append(oper)
            except Exception:
                pass
    return all_reqs
