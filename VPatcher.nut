if (!("VPatcherData" in getroottable()))
{
    ::VPatcherData <- {
        Patches = {},
        OrigMethods = {},
        NewMethods = {
            "*": {},
            "weap": {}
        }
    }
}

::VPatcher <- {
    classes = [
        CBaseEntity,
        CBaseAnimating, 
        CBaseFlex,
        CBaseCombatCharacter,
        CBasePlayer,
        CEconEntity,
        CTFPlayer,
        CTFBot,
        CTFBaseBoss,
        CEnvEntityMaker,
        CFuncTrackTrain,
        CSceneEntity,
        NextBotCombatCharacter,
        CTFWeaponBase,
        CBaseCombatWeapon
    ],

    PatchEntityMethod = function(clsnm, mthdnm, func) {
        if (mthdnm == "GetClassname")
            throw "Cannot patch GetClassname since it is used for patching.";

        foreach (cls in classes)
        {
            if (mthdnm in cls)
            {
                if (!(cls in VPatcherData.OrigMethods))
                    VPatcherData.OrigMethods[cls] <- {};
                if (!(mthdnm in VPatcherData.OrigMethods[cls]))
                    VPatcherData.OrigMethods[cls][mthdnm] <- cls[mthdnm];

                if (!(cls in VPatcherData.Patches))
                    VPatcherData.Patches[cls] <- {};
                if (!(clsnm in VPatcherData.Patches[cls]))
                    VPatcherData.Patches[cls][clsnm] <- {};
                if (!(mthdnm in VPatcherData.Patches[cls][clsnm]))
                    VPatcherData.Patches[cls][clsnm][mthdnm] <- cls[mthdnm];
                
                local origMthd;
                if (cls in VPatcherData.Patches && clsnm in VPatcherData.Patches[cls] && mthdnm in VPatcherData.Patches[cls][clsnm] && VPatcherData.Patches[cls][clsnm][mthdnm] != null)
                    origMthd = VPatcherData.Patches[cls][clsnm][mthdnm]
                else
                    origMthd = VPatcherData.OrigMethods[cls][mthdnm];

                cls[mthdnm] <- function(...)
                {
                    if (GetClassname() in VPatcherData.Patches[cls] && mthdnm in VPatcherData.Patches[cls][GetClassname()])
                    {
                        vargv.insert(0, origMthd);
                        vargv.insert(0, this);
                        return func.bindenv(this).acall(vargv);
                    }
                    // yes, this does mean that if it finds a specified patch for the method this'll be overridden. This is the best implimentation I could do, this whole project has been a headache.
                    else if ("*" in VPatcherData.Patches[cls] && mthdnm in VPatcherData.Patches[cls]["*"])
                    {
                        vargv.insert(0, origMthd);
                        vargv.insert(0, this);
                        return func.bindenv(this).acall(vargv);
                    }
                    else
                    {
                        return VPatcherData.OrigMethods[cls][mthdnm].bindenv(this).acall(vargv);
                    }
                };
                VPatcherData.Patches[cls][clsnm][mthdnm] = cls[mthdnm];
            }
        }
    },

    UnPatchEntityMethod = function(clsnm, mthdnm) {
        if (mthdnm == "GetClassname")
            throw "Cannot unpatch GetClassname since it cannot be patched.";
        
        foreach (cls in classes)
        {
            if (cls in VPatcherData.OrigMethods && mthdnm in VPatcherData.OrigMethods[cls])
            {
                cls[mthdnm] <- VPatcherData.OrigMethods[cls][mthdnm];
            }

            if (cls in VPatcherData.Patches && clsnm in VPatcherData.Patches[cls])
            {
                VPatcherData.Patches[cls][clsnm].rawdelete(mthdnm);
            }
        }
    },

    AddEntityMethod = function(clsnm, mthdnm, func) {
        if (!(clsnm in VPatcherData.NewMethods))
            VPatcherData.NewMethods[clsnm] <- {};
        VPatcherData.NewMethods[clsnm][mthdnm] <- func;
        foreach (cls in classes)
        {
            // it's better to use a get so I can dynamically check if the classname is correct and only allow calling the function on the correct entity type.
            // I don't believe I can use this for patching but if I can that'd be perfered for the same reason as above.
            if (!("_get" in cls)){
                cls._get <- function(key)
                {
                    if (key in ::VPatcherData.NewMethods["*"])
                        return ::VPatcherData.NewMethods["*"][key].bindenv(this);

                    if (GetClassname() in ::VPatcherData.NewMethods && key in ::VPatcherData.NewMethods[GetClassname()])
                        return ::VPatcherData.NewMethods[GetClassname()][key].bindenv(this);

                    throw null;
                }
            }
        }
    }
}

// add VPatcher docs
RegisterFunctionDocumentation(VPatcher.PatchEntityMethod, "VPatcher::PatchEntityMethod", "VPatcher::PatchEntityMethod(string entityClass, string methodName, function<func origMethod, ...> patch)", "Monkey patch a method for the specified entity.");
RegisterFunctionDocumentation(VPatcher.UnPatchEntityMethod, "VPatcher::UnPatchEntityMethod", "VPatcher::PatchEntityMethod(string entityClass, string methodName)", "Removes all patches from the method on the specified entity and restores original funcitonality.");
RegisterFunctionDocumentation(VPatcher.AddEntityMethod, "VPatcher::AddEntityMethod", "VPatcher::AddEntityMethod(string entityClass, string methodName, function func)", "Adds a method to the specified entity class.");