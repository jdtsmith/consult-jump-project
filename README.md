# consult-jump-project

Quickly jump between projects, their files and buffers with [consult](https://github.com/minad/consult).

<img width="520" alt="image" src="https://user-images.githubusercontent.com/93749/163687169-f244d194-84af-41c5-8214-75d846c8c65b.png">

```elisp
;; Consult-jump-project provides a single interactive command of the
;; same name which provides a narrowable list of project-related
;; targets, ala consult-buffer (which it uses):
;; 
;;   - project files (f)
;;   - project buffers (b)
;;   - other projects (p)
;;
;; If the file from which consult-jump-project is invoked is not in
;; any project, you are prompted only with the full list of projects
;; known to Emacs (via project.el).  While selecting projects, a
;; preview is provided in the form of a dired buffer visiting its
;; root.  Information on the number of recent files and buffers, and
;; the abbreviated age of the newest of these is shown.
;;
;; Upon selecting a project, the interface is re-entered, allowing you
;; to select of of its recent files of open buffers (or switch to yet
;; another project).
;;
;; Usage: simply bind `consult-jump-project` to a convenient key.
;;
;; Inspiration from: consult, consult-project-extra.
```

See also [consult-project-extra](https://github.com/Qkessler/consult-project-extra).
