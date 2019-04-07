# provision a new machine
1. Install ubuntu 16.04 or 18.04
2. Run `provision-machine.sh`
3. Configure `awesomewm`

# to run cron job

edit crontab 

``` shell
# crontab -e
SHELL=/bin/bash
MAILTO=root@example.com
PATH=/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin

# Example of job definition:
# .---------------- minute (0 - 59)
# |  .------------- hour (0 - 23)
# |  |  .---------- day of month (1 - 31)
# |  |  |  .------- month (1 - 12) OR jan,feb,mar,apr ...
# |  |  |  |  .---- day of week (0 - 6) (Sunday=0 or 7) OR sun,mon,tue,wed,thu,fri,sat
# |  |  |  |  |
# *  *  *  *  * user-name  command to be executed

*/20 * * * * /home/hung/dotfiles2/cron_agenda_export.sh

```

then start cron service with 

``` shell
sudo service start cron
```
