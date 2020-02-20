class CfgPatches
{
  class a3web
  {
    name = "Arma 3 Realtime Map";
    author = "jack77213";
    url = "https://arma.huashui.cf";

    requiredVersion = 1.92;
    requiredAddons[] = {"A3_Functions_F"};
    units[] = {};
    weapons[] = {};
  };
};

class CfgFunctions
{
  class huashui
  {
    class a3web
    {
      file = "x\huashui\a3web";
      class a3web { postInit=1; };
    };
  };
};

