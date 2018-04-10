(* ::Package:: *)

If[Not@$CloudConnected,
	Return[$Failed]
]

co = CloudObject["api/private/requesterid"];
CloudDeploy[
	APIFunction[
		{}, 
		$RequesterWolframID &
	], 
	co
]

co = CloudObject["api/private/stringreverse"];
CloudDeploy[
	APIFunction[
		{"str" -> "String"}, 
		StringReverse[#str] &
	], 
	co
]

co = CloudObject["api/private/range/formated/json"];
CloudDeploy[APIFunction[
  {"max" -> "Integer", "min" -> "Integer" -> 1, 
   "step" -> "Integer" -> 1},
  Range[#min, #max, #step] &,
  "JSON"],
 co
 ]

co = CloudObject["api/private/range/wlerror"];
CloudDeploy[APIFunction[
  {"i"},
  Range[#i] &,
  "JSON"],
 co
 ]
