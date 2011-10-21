(load "cl-macs")                        ; for cl-function-arglist
(load "autoload")

(defun xwem-batch-update-directory ()
  (let* ((generate-autoload-cookie ";;;###xwem-autoload")
         (gendir "lisp")
         (genldfile "xwem-loaddefs")
         (generated-autoload-file (expand-file-name (concat genldfile ".el") gendir))
         (enable-local-eval nil))	; Don't query in batch mode.
    (message "Updating internal XWEM autoloads for directory %s..." gendir)
    (if (fboundp 'update-autoloads-from-directory)
        (update-autoloads-from-directory gendir)
      (update-autoload-files (list gendir) nil
                             generated-autoload-file t))
    (cond ((fboundp 'fixup-autoload-buffer)
           (fixup-autoload-buffer genldfile))
          ((fboundp 'autoload-featurep-protect-autoloads)
           (autoload-featurep-protect-autoloads genldfile)))

    (save-some-buffers t)
    (message "Done")))
