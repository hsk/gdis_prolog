#
# Regular cron jobs for the gdis-prolog package
#
0 4	* * *	root	[ -x /usr/bin/gdis-prolog_maintenance ] && /usr/bin/gdis-prolog_maintenance
