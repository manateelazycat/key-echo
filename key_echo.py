#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (C) 2022 Andy Stewart
#
# Author:     Andy Stewart <lazycat.manatee@gmail.com>
# Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
import queue
import threading
import traceback
import sys
import platform
import time
from pynput.keyboard import Listener as kbListener
from epc.server import ThreadingEPCServer
from utils import *

class KeyEcho:
    def __init__(self, args):
        # Init EPC client port.
        init_epc_client(int(args[0]))

        # Build EPC server.
        self.server = ThreadingEPCServer(('127.0.0.1', 0), log_traceback=True)
        # self.server.logger.setLevel(logging.DEBUG)
        self.server.allow_reuse_address = True

        # ch = logging.FileHandler(filename=os.path.join(key-echo_config_dir, 'epc_log.txt'), mode='w')
        # formatter = logging.Formatter('%(asctime)s | %(levelname)-8s | %(lineno)04d | %(message)s')
        # ch.setFormatter(formatter)
        # ch.setLevel(logging.DEBUG)
        # self.server.logger.addHandler(ch)
        # self.server.logger = logger

        self.server.register_instance(self)  # register instance functions let elisp side call

        # Start EPC server with sub-thread, avoid block Qt main loop.
        self.server_thread = threading.Thread(target=self.server.serve_forever)
        self.server_thread.start()
        
        # All Emacs request running in event_loop.
        self.event_queue = queue.Queue()
        self.event_loop = threading.Thread(target=self.event_dispatcher)
        self.event_loop.start()

        self.emacs_xid = None
        
        if not self.isDarwin():
            # When system type is not darwin, init xlib vars.
            self.initXlib()

        # Init key event vars.
        self.last_press_key = None
        self.last_release_key = None
        self.last_press_time = 0

        # Start key event listener thread.
        self.key_event_listener = threading.Thread(target=self.listen_key_event)
        self.key_event_listener.start()

        # Pass epc port and webengine codec information to Emacs when first start key-echo.
        eval_in_emacs('key-echo--first-start', self.server.server_address[1])

        # event_loop never exit, simulation event loop.
        self.event_loop.join()

    def initXlib(self):
        from Xlib import X
        from Xlib.display import Display
        self.disp = Display()
        self.root = self.disp.screen().root
        self.NET_ACTIVE_WINDOW = self.disp.intern_atom('_NET_ACTIVE_WINDOW')        

    def isDarwin(self):
        return platform.system().lower() == "darwin"

    def listen_key_event(self):
        while True:
            with kbListener(
                    on_press=self.key_press,
                    on_release=self.key_release) as listener:
                listener.join()

    def get_current_time(self):
        return time.time() * 1000

    def key_press(self, key):
        if self.get_active_window_id() == self.get_emacs_xid():
            self.last_press_key = key

            self.last_press_time = self.get_current_time()

    def key_release(self, key):
        if self.get_active_window_id() == self.get_emacs_xid():
            self.last_release_key = key

            if self.last_press_key == key and self.get_current_time() - self.last_press_time < 200:
                eval_in_emacs("key-echo-single-key-trigger", str(key))

    def get_emacs_xid(self):
        if self.emacs_xid is None:
            self.emacs_xid = get_emacs_func_result("get-emacs-xid")

        return self.emacs_xid

    def get_active_window_id_osx(self):
        from AppKit import NSWorkspace
        return NSWorkspace.sharedWorkspace().activeApplication()['NSApplicationProcessIdentifier']            
    def get_active_window_id(self):
        if self.isDarwin():
            # When system type is Darwin, return MacOS pid of active application
            return self.get_active_window_id_osx()
        
        response = self.root.get_full_property(self.NET_ACTIVE_WINDOW,
                                      X.AnyPropertyType)
        win_id = response.value[0]

        return win_id

    def event_dispatcher(self):
        try:
            while True:
                message = self.event_queue.get(True)
                print(message)
                self.event_queue.task_done()
        except:
            logger.error(traceback.format_exc())

    def cleanup(self):
        """Do some cleanup before exit python process."""
        close_epc_client()

if __name__ == "__main__":
    if len(sys.argv) >= 3:
        import cProfile
        profiler = cProfile.Profile()
        profiler.run("KeyEcho(sys.argv[1:])")
    else:
        KeyEcho(sys.argv[1:])
