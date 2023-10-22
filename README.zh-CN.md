[English](./README.md) | 简体中文

## Key-Echo
Key-Echo 是一个利用 XRecord 技术监听系统按键事件的 Emacs 插件， 它可以监听一些特殊的单按键事件， 比如用户只是按了一下 Shift 辅助键而没有按其他字母按键时， 发送信号给 Emacs， 让 Emacs 执行一些 Elisp 代码， 比如切换输入法。

## 安装
1. 安装 Emacs 28 及以上版本
2. 安装 Python 依赖:
 - Linux:
   - X: `pip3 install epc sexpdata six pynput`
   - Wayland: `pip3 install epc sexpdata six libinput libevdev`
 - Windows: `pip3 install epc sexpdata six pynput pygetwindow`
 - macOS: `pip3 install epc sexpdata six pynput pyobjc`
3. 用 `git clone` 下载此仓库， 并替换下面配置中的 load-path 路径
4. 把下面代码加入到你的配置文件 ~/.emacs 中：

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

加上上面设置后， 按 Shift 可以自由切换输入法。

## 选项
* key-echo-keyboard-quit-key: 我们无法通过调用 `keyboard-quit` 函数来实现 `Ctrl + g` 的功能， 所以默认设置 `key-echo-keyboard-quit-key` 为 `Key.alt_r` 来实现当用户按右边的 Alt 键时， 发送 `Ctrl + g` 按键给 Emacs， 来实现快速按 Ctrl + g 的目标
