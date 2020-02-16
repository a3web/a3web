class CfgPatches
{
	class leoservermod
	{
		// Meta information for editor
		name = "leoservermod";
		author = "leo";
		url = "https://leosongwei.github.io/";

		requiredVersion = 1.69; 

		requiredAddons[] = {};

		units[] = {};
		weapons[] = {};
		// init = "diag_log 'loading leoext...'; call compile preprocessFileLineNumbers '\leoservermod\init.sqf'";
	};
};

class CfgFunctions
{
	class leoservermod
	{
		class functions
		{
			class leoservermodInit {
				file="\leoservermod\init.sqf";
				postInit=1;
			};
		};
	};
};