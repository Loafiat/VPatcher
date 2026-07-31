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
    // don't ask why but I need to patch all of them despite them all being based on CBaseEntity.
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

    PatchClassMethod = function(cls, mthdnm, func) {
        if (mthdnm == "GetClassname")
            throw "Cannot patch GetClassname since it is used for patching.";

        if (!(mthdnm in cls))
            cls[mthdnm](); // attempt to execute it to throw the not found error

        if (!(cls in VPatcherData.OrigMethods))
            VPatcherData.OrigMethods[cls] <- {};

        if (!(mthdnm in VPatcherData.OrigMethods[cls]))
            VPatcherData.OrigMethods[cls][mthdnm] <- cls[mthdnm];

        if (!(cls in VPatcherData.Patches))
            VPatcherData.Patches[cls] <- {};

        if (!(mthdnm in VPatcherData.Patches[cls]))
        {
            VPatcherData.Patches[cls][mthdnm] <- [];
            
            AssignPatch(cls, mthdnm);
        }

        VPatcherData.Patches[cls][mthdnm].append({
            classname = "*",
            func = func
        });
    },

    // Genuinely did not have the brain capacity to understand what I'm writing anymore and
    // didn't want a 3rd almost total rewrite so I'm slopping it up with ChatGPT, sorry lol.

    // Though to be fair it didn't write everything. I'm using it as a tool you can't be mad at me 😇

    PatchEntityMethod = function(clsnm, mthdnm, func) {
        if (mthdnm == "GetClassname")
            throw "Cannot patch GetClassname since it is used for patching.";

        foreach (cls in classes)
        {
            if (!(mthdnm in cls))
                continue;

            if (!(cls in VPatcherData.OrigMethods))
                VPatcherData.OrigMethods[cls] <- {};

            if (!(mthdnm in VPatcherData.OrigMethods[cls]))
                VPatcherData.OrigMethods[cls][mthdnm] <- cls[mthdnm];

            if (!(cls in VPatcherData.Patches))
                VPatcherData.Patches[cls] <- {};

            if (!(mthdnm in VPatcherData.Patches[cls]))
            {
                VPatcherData.Patches[cls][mthdnm] <- [];

                AssignPatch(cls, mthdnm);
            }

            VPatcherData.Patches[cls][mthdnm].append({
                classname = clsnm,
                func = func
            });
        }
    },

    AssignPatch = function(cls, mthdnm) {
        cls[mthdnm] <- function(...)
        {
            local callArgs = clone vargv;
            callArgs.insert(0, this);

            local patches = VPatcherData.Patches[cls][mthdnm];
            local self = this;
            local clsname = self.GetClassname();

            local Invoke = null;

            Invoke = function(index, args)
            {
                while (index >= 0)
                {
                    local patch = patches[index];

                    if (patch.classname == "*" || patch.classname == clsname)
                    {
                        local origMethod = function(...)
                        {
                            local newArgs = clone vargv;
                            newArgs.insert(0, self);
                            return Invoke(index - 1, newArgs);
                        };

                        // mistuh white put dat shit on index 1 so it doesn't overwrite the scope yo
                        local argv = clone args;
                        argv.insert(1, origMethod);

                        return patch.func.acall(argv);
                    }
                    
                    index--;
                }

                return VPatcherData.OrigMethods[cls][mthdnm].acall(args);
            };
            
            return Invoke(patches.len() - 1, callArgs);
        };
    },

    UnPatchEntityMethod = function(clsnm, mthdnm) {
        if (mthdnm == "GetClassname")
            throw "Cannot unpatch GetClassname since it cannot be patched.";

        foreach (cls in classes)
        {
            if (!(cls in VPatcherData.Patches))
                continue;

            if (!(mthdnm in VPatcherData.Patches[cls]))
                continue;

            local patches = VPatcherData.Patches[cls][mthdnm];

            for (local i = patches.len() - 1; i >= 0; --i)
            {
                if (patches[i].classname == clsnm)
                    patches.remove(i);
            }
    
            if (patches.len() == 0)
            {
                if (cls in VPatcherData.OrigMethods &&
                    mthdnm in VPatcherData.OrigMethods[cls])
                {
                    cls[mthdnm] <- VPatcherData.OrigMethods[cls][mthdnm];
                }

                VPatcherData.Patches[cls].rawdelete(mthdnm);

                if (VPatcherData.Patches[cls].len() == 0)
                    VPatcherData.Patches.rawdelete(cls);
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

RegisterFunctionDocumentation(VPatcher.PatchEntityMethod, "VPatcher::PatchEntityMethod", "VPatcher::PatchEntityMethod(string entityClass, string methodName, function<func origMethod, ...> patch)", "Monkey patch a method for the specified entity.");
RegisterFunctionDocumentation(VPatcher.UnPatchEntityMethod, "VPatcher::UnPatchEntityMethod", "VPatcher::PatchEntityMethod(string entityClass, string methodName)", "Removes all patches from the method on the specified entity and restores original funcitonality.");
RegisterFunctionDocumentation(VPatcher.AddEntityMethod, "VPatcher::AddEntityMethod", "VPatcher::AddEntityMethod(string entityClass, string methodName, function func)", "Adds a method to the specified entity class.");