;;; gtags port/host settings for the corp cluster

(load "gtags.el")

(setq google-tag-host "gtags.corp.google.com")

(setq google-callgraph-tag-host  "gtags2.corp.google.com")

(setq google-tags-host-port-alist
      `((c++-mode . (,google-tag-host . 2223))
	(java-mode . (,google-tag-host . 2224))
	(jde-mode . (,google-tag-host . 2224))
	(python-mode . (,google-tag-host . 2225))))

(setq google-tags-callgraph-host-port-alist
      `((c++-mode . (,google-callgraph-tag-host . 2233))
       (java-mode . (,google-callgraph-tag-host . 2234))
       (jde-mode . (,google-callgraph-tag-host . 2234))
       (python-mode . (,google-callgraph-tag-host . 2235))))
