leoext_map_markers_information = {
	private _info_list = [];
	{
		private _text = markerText _x;
		private _icon_type = markerType _x; // "Empty" == not icon
		private _pos = markerPos _x;
		private _color = markerColor _x;

		private _marker_info = [_text, _icon_type, _color, _pos];
		if (_icon_type != "") then {
			_info_list pushBack _marker_info;
		};
	} forEach allMapMarkers;

	_info_list;
};

leoext_units_pos = {
	private _units_pos = [];
	{
		private _side = str (side _x);
		private _pos = position _x;

		private _unit = [_side, _pos];
		_units_pos pushBack _unit;
	} forEach allUnits;

	_units_pos;
};

leoext_markers_start = {
	[] spawn {
		while {!leoext_stop} do {
			sleep 1;
			private _markers = call leoext_map_markers_information;
			private _units = call leoext_units_pos;
			"libleoext" callExtension ["foo", [[_markers, _units]]];
		};
	};
};

call leoext_markers_start;