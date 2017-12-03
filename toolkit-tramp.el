;;; private/xwu157/toolkit-tramp.el -*- lexical-binding: t; -*-

(require 'tramp)
(add-to-list 'tramp-methods  '("toolkit"
                               (tramp-login-program "ssh")
                               (tramp-login-args
                                (("-p" "%p")
                                 ("-t")
                                 ("-t")
                                 ("-o" "ControlPath=~/.ssh/%%u@v5devgateway.bdns.bloomberg.com:%%p")
                                 ("-o" "ControlMaster=auto")
                                 ("-o" "ControlPersist=800")
                                 ("-e" "none")
                                 ("v5devgateway.bdns.bloomberg.com")
                                 ("inline")
                                 ("%h")))
                               (tramp-async-args
                                (("-q")))
                               (tramp-remote-shell "/bin/sh")
                               (tramp-remote-shell-args
                                ("-c"))
                               (tramp-gw-args
                                (("-o" "GlobalKnownHostsFile=/dev/null")
                                 ("-o" "UserKnownHostsFile=/dev/null")
                                 ("-o" "StrictHostKeyChecking=no")
                                 ))
                               (tramp-default-port 22)))

(add-to-list 'tramp-remote-path "/opt/bb/bin")

(provide 'toolkit-tramp)

