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

        # Init vars.
        self.emacs_id = None
        self.last_press_key = None
        self.last_press_time = 0
        self.keyboard_quit_key = None

        # Start key event listener thread.
        self.key_event_listener = threading.Thread(target=self.listen_key_event)
        self.key_event_listener.start()

        # Pass epc port and webengine codec information to Emacs when first start key-echo.
        eval_in_emacs('key-echo--first-start', self.server.server_address[1])

        # event_loop never exit, simulation event loop.
        self.event_loop.join()

    def is_darwin(self):
        return platform.system().lower() == "darwin"

    def listen_key_event(self):
        if get_emacs_func_result("emacs-running-in-wayland-native"):
            from libinput import LibInput, ContextType, KeyState, EventType, DeviceCapability
            import libevdev

            kbd_devnode = []

            li = LibInput(context_type=ContextType.UDEV)
            li.assign_seat("seat0")
            for event in li.events:
                if (event.type == EventType.DEVICE_ADDED):
                    if (event.device.capabilities == (DeviceCapability.KEYBOARD,)
                        and event.device.keyboard.has_key(1)):
                        kbd_devnode.append(event.device.sysname)
                else:
                    break

            del li
            li = LibInput(context_type=ContextType.PATH)
            for devnode in kbd_devnode:
                li.add_device("/dev/input/" + devnode)

            device = libevdev.Device()
            device.name = "Key Echo"
            device.enable(libevdev.EV_KEY.KEY_G)
            device.enable(libevdev.EV_KEY.KEY_LEFTCTRL)
            self.uinput = device.create_uinput_device()

            for event in li.events:
                if (event.type == EventType.KEYBOARD_KEY
                    and self.get_active_window_id() == self.get_emacs_id()):
                    if event.key_state == KeyState.PRESSED:
                        self.key_press_wayland(event)
                    else:
                        self.key_release_wayland(event)


        from pynput.keyboard import Listener as kbListener
        from pynput.keyboard import Controller

        self.keyboard = Controller()

        while True:
            with kbListener(
                    on_press=self.key_press,
                    on_release=self.key_release) as listener:
                listener.join()

    def get_current_time(self):
        return time.time() * 1000

    def key_press(self, key):
        if self.get_active_window_id() == self.get_emacs_id():
            self.last_press_key = key

            self.last_press_time = self.get_current_time()

    def key_press_wayland(self, event):
        self.last_press_key = event.key
        self.last_press_time = self.get_current_time()

    def key_release(self, key):
        if self.get_active_window_id() == self.get_emacs_id():
            if self.last_press_key == key and self.get_current_time() - self.last_press_time < 200:
                if str(self.last_press_key) == self.get_keyboard_quit_key():
                    from pynput.keyboard import Key

                    with self.keyboard.pressed(Key.ctrl):
                        self.keyboard.press('g')
                        self.keyboard.release('g')
                else:
                    eval_in_emacs("key-echo-single-key-trigger", str(key))

    def key_release_wayland(self, event):
        if self.last_press_key == event.key and self.get_current_time() - self.last_press_time < 200:
            import libevdev
            from libevdev import InputEvent

            key_name = libevdev.evbit(1, event.key).name
            if key_name == self.get_keyboard_quit_key():
                event = [InputEvent(libevdev.EV_KEY.KEY_LEFTCTRL, value=1),
                         InputEvent(libevdev.EV_SYN.SYN_REPORT, value=0),
                         InputEvent(libevdev.EV_KEY.KEY_G, value=1),
                         InputEvent(libevdev.EV_SYN.SYN_REPORT, value=0),
                         InputEvent(libevdev.EV_KEY.KEY_G, value=0),
			 InputEvent(libevdev.EV_SYN.SYN_REPORT, value=0),
                         InputEvent(libevdev.EV_KEY.KEY_LEFTCTRL, value=0),
                         InputEvent(libevdev.EV_SYN.SYN_REPORT, value=0)]

                self.uinput.send_events(event)

                self.last_press_key = None
            else:
                eval_in_emacs("key-echo-single-key-trigger", key_name)

    def get_keyboard_quit_key(self):
        if self.keyboard_quit_key is None:
            self.keyboard_quit_key = get_emacs_var("key-echo-keyboard-quit-key")

        return self.keyboard_quit_key

    def get_emacs_id(self):
        if platform.system() == "Windows":
            import pygetwindow as gw
            return gw.getActiveWindow()._hWnd
        elif current_desktop == "sway":
            if self.emacs_id is None:
                self.emacs_id = get_emacs_func_result("get-emacs-pid")

            return self.emacs_id
        else:
            if self.emacs_id is None:
                self.emacs_id = get_emacs_func_result("get-emacs-id")

            return self.emacs_id

    def get_active_window_id(self):
        if self.is_darwin():
            from AppKit import NSWorkspace
            return NSWorkspace.sharedWorkspace().activeApplication()['NSApplicationProcessIdentifier']
        elif current_desktop == "sway":
            import subprocess

            win_id = subprocess.Popen("swaymsg -t get_tree | jq -r '..|try select(.focused == true).pid'",
                                      stdout=subprocess.PIPE,
                                      shell=True)
            return int(win_id.stdout.read().decode().strip())
        else:
            from Xlib import X
            from Xlib.display import Display

            if not hasattr(self, "NET_ACTIVE_WINDOW"):
                self.disp = Display()
                self.root = self.disp.screen().root
                self.NET_ACTIVE_WINDOW = self.disp.intern_atom('_NET_ACTIVE_WINDOW')

            response = self.root.get_full_property(self.NET_ACTIVE_WINDOW, X.AnyPropertyType)
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
