import contextlib
import re
import socket
import time
from typing import Optional

_WORKER_ID_PATTERN = re.compile('^gw(\\d+)$')

"""
A set of the ports reserved by this file.

It is never cleared; we simply expect to not run out before the tests finish.
"""
_allocated_ports: set[int] = set()

def find_free_port(worker_id: str) -> int:
    """
    Finds a free port in the range allocated to the given worker.

    There is no lock placed on the port, so something else could claim the port
    between this function finding a port and it being used for its intended
    purpose. To mitigate this:

    1. we do not use the usual dynamic port range (above 32768), and
    2. we never return the same port twice from this function.

    We use the worker ID to construct the port range because we do not share
    the set of allocated ports between workers. This means we need to ensure
    that the ranges do not overlap.

    Note that the worker ID should be provided by the `worker_id` fixture.
    More details can be found here:
    https://pytest-xdist.readthedocs.io/en/latest/how-to.html#identifying-the-worker-process-during-a-test

    The worker ID is string in the form "gw<N>", where N is the worker number.
    For example, if you have 4 workers, they will be called "gw0", "gw1",
    "gw2", and "gw3". We parse the number back out of the string in order to
    construct the port range.
    """
    # Use a different port range per xdist worker. The range is 1000 ports,
    # starting at port 10000. So worker ID 7, for example, will use the ports
    # 17000 (inclusive) to 18000 (exclusive).
    match = _WORKER_ID_PATTERN.match(worker_id)
    if not match:
        raise Exception(f'Invalid worker ID: {worker_id!r}')
    worker_number = int(match.group(1))
    port_range = port_range = range((worker_number + 10) * 1000, (worker_number + 11) * 1000)

    for port in port_range:
        if port not in _allocated_ports:
            with contextlib.closing(socket.socket(socket.AF_INET, socket.SOCK_STREAM)) as s:
                try:
                    s.bind(('', port))
                    s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
                    _allocated_ports.add(port)
                    return port
                except OSError:
                    # try the next one
                    pass

    raise Exception('No available port found.')

def is_port_in_use(port: int) -> bool:
    """
    Checks whether a local port is in use.
    """
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        return s.connect_ex(('localhost', port)) == 0

def wait_for_port(port: int, timeout: Optional[float] = None) -> None:
    """
    Waits until a port is opened, with an optional timeout.
    """
    start_time = time.monotonic()
    while timeout is None or (time.monotonic() - start_time) < timeout:
        if is_port_in_use(port):
            return
        time.sleep(0.2)
    raise TimeoutError(f'Timed out waiting for port {port}.')
