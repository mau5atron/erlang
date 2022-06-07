% 19.5 Storing tuples on Disk
	
	% ETS tables store tuples in memory. DETS (short for Disk ETS) provides
	% Erlang tuple storage on disk. DETS files have a maximum size of 2GB.
	% DETS files must be opened before they can be used, and they should be
	% properly closed when finished with. If they are not properly closed,
	% then they will be automatically repaired the next time they are
	% opened. Since the repair can take a long time, it's important to close
	% them properly before finishing your application.

	% DETES tables have different sharign properties fro ETS tables. When a
	% DETS table is opened, it must be given a global name, If two or more
	% local processes open a DETS table with the same name and options, then
	% they will share the table. The table will remain open until all
	% processers have closed the table (or crashed).
