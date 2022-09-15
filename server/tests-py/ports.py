import contextlib
import socket
import time
from typing import Optional

def find_free_port() -> int:
    """
    Finds a free port.

    There is no lock placed on the port, so something else could claim the port
    between this function finding a port and returning.
    """
    with contextlib.closing(socket.socket(socket.AF_INET, socket.SOCK_STREAM)) as s:
        s.bind(('', 0))
        s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        return s.getsockname()[1]

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
