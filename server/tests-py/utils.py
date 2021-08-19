# Various testing utility functions

import time

# Loop a function 'tries' times, until all assertions pass. With a 0.3 second
# pause after each. This re-raises AssertionError in case we run out of tries
def until_asserts_pass(tries, func):
    for x in range(0, tries):
        print(x)
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
