(* ::Package:: *)

(* Not useful since we apparently never receive multipart messages,
no matter the total size (tested with 80MB) *)

(* If[$VersionNumber < 12,
	ByteArrayJoin[ba1_ByteArray,ba2_ByteArray,rest___ByteArray]:=
		baJoin[ByteArrayToString[ba1, "ISOLatin1"], ba2,rest];
		
	baJoin[buffer:__String, ba_ByteArray, rest__ByteArray]:=
		baJoin[buffer, ByteArrayToString[ba, "ISOLatin1"], rest];

	baJoin[buffer:__String, ba_ByteArray]:= StringToByteArray[
		StringJoin[{buffer, ByteArrayToString[ba, "ISOLatin1"]}],
		"ISOLatin1"]
	,
	ByteArrayJoin = Join
];
 *)

Needs["GeneralUtilities`"]

Begin["ClientLibrary`"];

debug;
info;
warn;
error;


Begin["`Private`"];

$DEBUG=1;
$INFO=2;
$WARN=3;
$ERROR=4;

ClientLibrary`debug[msg__String] := log[msg, $DEBUG];
ClientLibrary`info[msg__String] := log[msg, $INFO];
ClientLibrary`warn[msg__String] := log[msg, $WARN];
ClientLibrary`error[msg__String] := log[msg, $ERROR];

log[msg_String, level_Integer:$INFO] := If[$LoggerSocket=!=None, 
	BinaryWrite[
		$LoggerSocket, 
		Developer`WriteRawJSONString[<|"msg"->msg, "level"->level|>, "ToByteString"->True]
	],
	Print[msg]
];
log[msg__String, level_Integer:$INFO] := log[StringJoin[{msg}], level];
log[msg__String, False, level_Integer:$INFO] := Map[log[#, level]&, {msg}];


$MaxLoggedStringLength = 1014;
short[string_String] /; StringLength[string] >= $MaxLoggedStringLength := StringTake[string, $MaxLoggedStringLength];
short[string_String] := string;

SetAttributes[timed, HoldAllComplete];
timed[expr_] := timed[Unevaluated[expr], short[ToString[Unevaluated[expr], InputForm]]];
timed[expr_, label_String] := Block[
	{timer, eval},
	{timer, eval} = AbsoluteTiming[expr];
	ClientLibrary`info[label, " took: ", readableTiming[timer]];
	eval
];

readableTiming[timing_] := Which[
	timing < 10*^-3,
	ToString[N[timing/10.*^-6]] <> "\[Micro]s",
	timing < 1,
	ToString[N[timing/10.*^-3]] <> "ms",
	True,
	ToString[N[timing]] <> "s"
];

evaluate[data_String] := ToExpression[data];
evaluate[data_ByteArray] := evaluate[ByteArrayToString[data]];

serialize[expr_] := ToString[expr, InputForm];


$LoggerSocket=None;
$OutputSocket=None;
$InputSocket=None;

SlaveKernelPrivateStart[inputsocket_String, outputsocket_String, logsocket_String] := (
	$LoggerSocket=SocketConnect[logsocket,"ZMQ_Push"];
	If[FailureQ[$LoggerSocket],
		Print["Failed to connect to logging socket: ", logsocket]
		,
		ClientLibrary`info["Connected to logging socket: ", logsocket];
		SlaveKernelPrivateStart[inputsocket, outputsocket]
	]
);

SlaveKernelPrivateStart[inputsocket_String, outputsocket_String] := Block[
	{listener},
	$InputSocket = SocketConnect[inputsocket, "ZMQ_Pull"];
	$OutputSocket = SocketConnect[outputsocket, "ZMQ_Push"];
	If[TrueQ@FailureQ[$OutputSocket],
		Print["Failed to connect to output socket ", outputsocket]
		Quit[]
	];
	If[TrueQ@FailureQ[$InputSocket],
		Print["Failed to connect to input socket ", inputsocket]
		Quit[]
	];
	listener = SocketListen[
		$InputSocket,
		GeneralUtilities`EvaluateChecked[
			Block[{data, multi, expr},
				data = Lookup[#,"DataByteArray", None];
				expr = timed[evaluate[data], "Expression evaluation"];
				WriteString[
					$OutputSocket,
					timed[serialize[expr], "Expression serialization"]
				];
			]
			,
			Block[{err = GeneralUtilities`FailureString[#]},
				ClientLibrary`error[err];
				WriteString[$OutputSocket, "$Failed"]
			] &
		]&
		,
		HandlerFunctionsKeys->{"DataByteArray"}
	];
	If[TrueQ@FailureQ[listener], 
		ClientLibrary`error["Failed to listen to input socket ", inputsocket];
		Quit[]
	];
	WriteString[
		$OutputSocket,
		"OK"
	];
];

End[];
End[];
