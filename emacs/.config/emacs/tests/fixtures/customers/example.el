;;; example.el --- Fictional private config fixture -*- lexical-binding: t -*-

(nyy/agent-shell-mcp-register-server
 '((name . "customer-mcp")
   (command . "unused")
   (args . ("https://mcp.example.invalid/"))
   (env . (((name . "EXAMPLE_TOKEN")
            (value . (lambda () (error "Credential evaluated during setup")))))))
 '("/tmp/customer/"))
