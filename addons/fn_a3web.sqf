[] spawn {
  //if (!hasInterface) exitWith {};
  if (!isServer) exitWith {};
  //if (!isDedicated) exitWith {};

  addMissionEventHandler ["ExtensionCallback", {
	  params ["_name", "_function", "_data"];
	  if (_name isEqualTo "a3web") then 
	  { 
      diag_log format ["ExtensionCallback %1, %2, %3", _name, _function, _data];
	  };
  }];

  waitUntil {time > 0};
 
  a3web_interval = 1;

  while {true} do {
    sleep a3web_interval;
	  private _units = allUnits apply {
		  [netid _x, isPlayer _x, name _x, str (side _x), getPos _x];
	  };
		"liba3web" callExtension ["http:post:/units-info", [_units]];
  };
};
