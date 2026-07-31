# VPatcher
A library to help with monkey patching functions on TF2 entities aiming to prevent conflicts.

## How to use
include VPatcher in script:
```
DoIncludeScript("VPatcher", null);
```

to Patch/add a method to an entity:
```
// Patching Method
VPatcher.PatchEntityMethod("player"/*entity name*/, "GetModelName"/*method name*/, function (originalMethod)
{
    return originalMethod() + "_test";
});

// Adding Method
VPatcher.AddEntityMethod("tf_wearable"/*entity name*/, "Unusualify"/*method name*/, function(particle_index){
    this.AddAttribute("attach particle effect", particle_index, -1);
    return this;
});
```