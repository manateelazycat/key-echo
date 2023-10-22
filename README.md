English | [简体中文](./README.zh-CN.md)

## Key-Echo
Key-Echo is an Emacs plugin that uses XRecord technology to listen to system key events. It can listen to some special single key events, such as when the user only presses the Shift key without pressing other letter keys, it sends a signal to Emacs to execute some Elisp code, such as switching input methods.

## Installation
1. Install Emacs 28 or above
2. Install Python dependencies:
 - Linux:
   - X: `pip3 install epc sexpdata six pynput`
   - Wayland: `pip3 install epc sexpdata six libinput libevdev`
 - Windows: `pip3 install epc sexpdata six pynput pygetwindow`
 - macOS: `pip3 install epc sexpdata six pynput pyobjc`
3. Use `git clone` to download this repository, and replace the load-path path in the configuration below
4. Add the following code to your configuration file ~/.emacs:

```elisp
(add-to-list 'load-path "<path-to-key-echo>")

(require 'key-echo)
(key-echo-enable)

;; X/Windows/macOS
(defun key-echo-shift-to-switch-input-method (key)
  (interactive)
  (when (string-equal key "Key.shift")
    (toggle-input-method)
    ))

;; Wayland
(defun key-echo-shift-to-switch-input-method (key)
  (interactive)
  (when (or (string-equal key "KEY_LEFTSHIFT")
	    (string-equal key "KEY_RIGHTSHIFT"))
    (toggle-input-method)
    ))

(setq key-echo-single-key-trigger-func 'key-echo-shift-to-switch-input-method)
```

After adding the above settings, you can freely switch the input method by pressing Shift.

## Options
* key-echo-keyboard-quit-key: We cannot implement the `Ctrl + g` function by calling the `keyboard-quit` function, so the default setting for `key-echo-keyboard-quit-key` is `Key.alt_r` to achieve the function of sending the `Ctrl + g` key to Emacs when the user presses the right Alt key, to achieve the goal of quickly pressing Ctrl + g.
