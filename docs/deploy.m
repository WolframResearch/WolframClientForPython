(* ::Package:: *)

ClearAll[DeployDocumentation];
DeployDocumentation[root_String, target_String, setperm : True | False : False] := (
  Parallelize@Scan[
   With[{relpath = 
       First@StringCases[#, root ~~ "/" ... ~~ relative__ :> relative,
          1]},
     PrintTemporary["Copying " <> relpath];
     CopyFile[#, CloudObject[FileNameJoin[{target,relpath}]], 
      OverwriteTarget -> True];
     If[setperm,
      SetOptions[CloudObject[FileNameJoin[{target,relpath}]], 
       Permissions -> "Public"]
      ];
     ]
    &,
   FileNames[{"*.js", "*.html", "*.css", "*.png", "*.svg"}, root, Infinity]
   ];
   If[setperm,
      SetOptions[CloudObject[target],Permissions -> "Public"]
    ];
   CloudObject[FileNameJoin[{target,"index.html"}]]
);

$doc = "/Users/dorianb/Work/Mathematica/Workspaces/WolframClientForPython/docs";
$root = FileNameJoin[{$doc, "_build","html"}]
DeployDocumentation[$root, "lcl/python/doc/", True]


$doc = "/Users/dorianb/Work/Matematica/Workspaces/wxfparser/doc";
DeployDocumentation[$doc,"wxf/java/doc/", True]


SetPermissions[CloudObject["wxf/java/doc"],"Public"]
