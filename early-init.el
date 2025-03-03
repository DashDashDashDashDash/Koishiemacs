(setenv "LSP_USE_PLISTS" "true")

(if (string-match (regexp-quote system-type) "windows-nt")
    (prefer-coding-system "UTF-8"))
