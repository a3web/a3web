diag_log "leoext: try to initialize...";

_return = "libleoext" callExtension "foooo";
execVM "\leoservermod\marker.sqf";

leoext_stop = false;

[] spawn {
	while {!leoext_stop} do {
		sleep 1;
		private _markers = call leoext_map_markers_information;
		private _units = call leoext_units_pos;
		"libleoext" callExtension ["foo", [[_markers, _units]]];
	};
}

diag_log format ["leoext: initialize complete! return: %1", _return];