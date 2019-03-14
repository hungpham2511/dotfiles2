emacs -batch -l ~/.emacs.d/init.el -eval '(org-batch-agenda "j")' > ~/Dropbox/agenda
date >> /tmp/cron_job_out
