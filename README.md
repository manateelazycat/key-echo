English | [简体中文](./README.zh-CN.md)

## Key-Echo
Key-Echo is an Emacs plugin that uses XRecord technology to listen to system key events. It can listen to some special single key events, such as when the user only presses the Shift key without pressing other letter keys, it sends a signal to Emacs to execute some Elisp code, such as switching input methods.

## Installation
1. Install Emacs 28 or above
2. Install Python dependencies: epc, sexpdata, six, pynput, pyobjc (for macos): `pip3 install epc sexpdata six pynput`
3. Download this repository with `git clone` and replace the load-path path in the configuration below
4. Add the following code to your configuration file ~/.emacs:

```elisp
(add-to-list 'load-path "<path-to-key-echo>")

(require 'key-echo)
(key-echo-enable)

(defun key-echo-shift-to-switch-input-method (key)
  (interactive)
  (when (string-equal key "Key.shift")
    (toggle-input-method)
    ))

(setq key-echo-single-key-trigger-func 'key-echo-shift-to-switch-input-method)
```

After adding the above settings, you can freely switch the input method by pressing Shift.

## Remarks
Currently supports Linux and macOS, theoretically it can support Windows, contributions are welcome.
