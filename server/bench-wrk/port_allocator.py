import threading
import socket


class PortAllocator:

    def __init__(self):
        self.allocated_ports = set()
        self.lock = threading.Lock()

    def get_unused_port(self, start):
        port = start
        if self.is_port_open(port) or port in self.allocated_ports:
            return self.get_unused_port(port + 1)
        else:
            return port

    def is_port_open(self, port):
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
            res = sock.connect_ex(('127.0.0.1', port))
            return res == 0

    def allocate_port(self, start):
        with self.lock:
            port = self.get_unused_port(start)
            self.allocated_ports.add(port)
            return port
