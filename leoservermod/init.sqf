diag_log "leoext: try to initialize...";

_return = "libleoext" callExtension "foooo";
execVM "\leoservermod\marker.sqf";

diag_log format ["leoext: initialize complete! return: %1", _return];