# Various testing utility functions

import time

# Loop a function 'tries' times, until all assertions pass. With a 0.3 second
# pause after each. This re-raises AssertionError in case we run out of tries
def until_asserts_pass(tries, func):
    for x in range(0, tries):
        if x == tries-1:
            # last time; raise any assertions in caller:
            func()
        else:
            try:
                func()
                break
            except AssertionError:
                time.sleep(0.3)
                pass

def insert(hge_ctx, table, row, returning=[], headers = {}):
    return insert_many(hge_ctx, table, [row], returning, headers)

def insert_many(hge_ctx, table, rows, returning=[], headers = {}):
    q = {
        "type": "insert",
        "args": {
            "table": table,
            "objects": rows,
            "returning": returning
        }
    }
    return hge_ctx.v1q(q, headers = headers)


def update(hge_ctx, table, where_exp, set_exp, headers = {}):
    q = {
        "type": "update",
        "args": {
            "table": table,
            "where": where_exp,
            "$set": set_exp
        }
    }
    return hge_ctx.v1q(q, headers = headers)


def delete(hge_ctx, table, where_exp, headers = {}):
    q = {
        "type": "delete",
        "args": {
            "table": table,
            "where": where_exp
        }
    }
    return hge_ctx.v1q(q, headers = headers)
