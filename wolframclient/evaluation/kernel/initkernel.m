(* ::Package:: *)


$NotSupportedVersionErrNo = 10;
$MinVersionSupported = 11.3;
If[$VersionNumber < $MinVersionSupported, Exit[$NotSupportedVersionErrNo]];

$IgnoreEOF=True;
$HistoryLength=0;

Needs["ZeroMQLink`"];

Begin["ClientLibrary`"];

debug;
info;
warn;
error;
disconnect;

Begin["`Private`"];


(*Define the most efficient pair of write bytearray and non-blocking read, for various WL versions. *)
{SocketWriteByteArrayFunc, SocketReadByteArrayFuncNoWait} = Which[
	$VersionNumber < 12,
	{
		Function[{socketOut, ba}, ZeroMQLink`Private`ZMQWriteInternal[socketOut, Normal[ba]]],
		Function[{socketIn},
			Block[
				{data=ByteArray@iRecvSingleMultipartMessageSocket[First@socketIn, 1(*Flag NOWAIT*)]},
				If[Length[data] >= 3, Part[data,4;;], {}]
			]
		]
	},
	$VersionNumber < 12.1,
	{
		Function[{socketOut, ba}, ZMQSocketWriteMessage[socketOut, ba]],
		Function[{socketIn},
			Block[
				{data = iRecvSingleMultipartBinaryMessageSocket[First@socketIn, 1(*Flag NOWAIT*)]},
				If[Length[data] >= 3, Part[data,4;;], {}]
			]
		]
	},
	PacletFind["ZeroMQLink" -> "1.2*"] == {},
	{
		ZMQSocketWriteMessage,
		SocketReadMessage[#, "Blocking"->False] &
	},
	True,
	{
		SocketWriteMessage[##, "Blocking"->False] &,
		SocketReadMessage[#, "Blocking"->False] &
	}
];

$DEBUG=1;
$INFO=2;
$WARN=3;
$ERROR=4;
$NOTSET=Infinity;

$LogLevel = $DEBUG;

ClientLibrary`debug[args__] := log[$DEBUG, args];
ClientLibrary`info[args__] := log[$INFO, args];
ClientLibrary`warn[args__] := log[$WARN, args];
ClientLibrary`error[args__] := log[$ERROR, args];

ClientLibrary`SetDebugLogLevel[] := setLogLevel[$DEBUG];
ClientLibrary`SetInfoLogLevel[] := setLogLevel[$INFO];
ClientLibrary`SetWarnLogLevel[] := setLogLevel[$WARN];
ClientLibrary`SetErrorLogLevel[] := setLogLevel[$ERROR];
ClientLibrary`DisableKernelLogging[] := setLogLevel[$NOTSET];

(* for clarity loglevel cannot be changed and remains $NOTSET if log is disable.*)
setLogLevel[level_Integer] /; $DEBUG <= level <= $ERROR := If[
	$LoggerSocket =!= None,
	($LogLevel = level)
];
setLogLevel[$NOTSET] := ($LogLevel = $NOTSET);


log[level_Integer, msg_String] /; level >= $LogLevel  := If[$LoggerSocket =!= None, 
   SocketWriteByteArrayFunc[
		$LoggerSocket,
		StringToByteArray[
			Developer`WriteRawJSONString[<|"msg" -> msg, "level" -> level|>, 
			"ToByteString" -> True, "Compact" -> True],
			"ISOLatin1"
		]
	]
];

log[level_Integer, args__] /; level >= $LogLevel := log[
   level, 
   StringJoin[
		StringRiffle[
			Map[format, {args}],
			" "
		]
	]
];

log[___] := Null;

format[e_] := ToString[e, InputForm];
format[str_String] := str;

$MaxLoggedStringLength = 1014;
short[string_String] /; StringLength[string] >= $MaxLoggedStringLength := StringTemplate["``...(`` more)"][StringTake[string, $MaxLoggedStringLength], StringLength[string] - $MaxLoggedStringLength];
short[string_String] := string;

SetAttributes[timed, HoldAllComplete];
timed[expr_] := timed[Unevaluated[expr], "Evaluation"];
timed[expr_, label_String] := Block[
	{timer, eval},
	{timer, eval} = AbsoluteTiming[expr];
	ClientLibrary`info[label, "took:", readableTiming[timer]];
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


SetAttributes[fmtmsg, HoldRest];

fmtmsg[msg_String, args___] := TemplateApply[msg, {args}];

fmtmsg[msg_MessageName, args___] := Module[
	{genmsg = MessageName[General, Last@msg]}, 
	If[StringQ[genmsg],
		fmtmsg[genmsg, args],
		TemplateApply["Undefined message `` with arguments ``", {msg, {args}}]
	]
];

fmtmsg[msg_, args___] := TemplateApply[
	"Invalid message `` with arguments ``", 
	{ToString[Unevaluated[msg]], {args}}];

fmtmsg[Hold[Message[msg_MessageName, args___], ___]] := fmtmsg[msg, args];

addMessageHandler[] := Internal`AddHandler[
	"Message",
 	Function[msg,
  	If[TrueQ[Last[msg]], ClientLibrary`warn[fmtmsg[msg]]]
 	]
];

addPrintHandler[] := Internal`AddHandler[
	"Wolfram.System.Print",
 	Composition[ClientLibrary`info, ReleaseHold]
];

socketEventHandler[data_] := Block[
	{expr},
	ClientLibrary`debug["Evaluating a new expression."];
	expr = EvaluationData[BinaryDeserialize[data]];
	(* Produce inline InputForm string messages. *)
	AssociateTo[
		expr, {
			"Result" -> BinarySerialize[expr["Result"]],
			"MessagesText" -> Map[
				fmtmsg,
				expr["MessagesExpressions"]
			]
		}
	];
	ClientLibrary`debug["Done evaluating."];
	SocketWriteByteArrayFunc[
		$OutputSocket,
		BinarySerialize[expr]
	];
	ClientLibrary`debug["Done responding."];
];

SendAck[] := WriteString[$OutputSocket, "OK"]

$MaxIdlePause=.001;
$MinIdlePause=0.0001;
$PauseIncrement=0.0001;
(*$ListenerSupportMinVersion = 12.3;*)
$ListenerSupportMinVersion = Infinity;
Which[
	$VersionNumber < $ListenerSupportMinVersion,
	(* Low CPU wait but need synchronous loop. *)
	evaluationLoop[socketIn_SocketObject]:= With[
		{maxPause=$MaxIdlePause, minPause=$MinIdlePause, incr=$PauseIncrement, poller={socketIn}},
		Block[{msg},
			SendAck[];
			While[True,
				msg = SocketReadByteArrayFuncNoWait[socketIn];
				If[Length[msg]>0,
					socketEventHandler[msg];
					,
					SocketWaitNext[poller];
				]
			]
		]
	],
	(* Version with SocketListen, code is cleaner is 3 times slower as the version above.
	Possibly during the async events and pre-emptive evaluation.
	 *)
	True,
	evaluationLoop[socketIn_SocketObject]:= (
		$SocketListener = SocketListen[
			socketIn,
			socketEventHandler[#DataByteArray]&
		];
		SendAck[];
		Pause[2^60];
	);
	(*
	 Version with fixed asynchronous tasks
	evaluationLoop[socketIn_SocketObject]:= With[
		{maxPause=$MaxIdlePause, minPause=$MinIdlePause, incr=$PauseIncrement, poller={socketIn}},
		$Task = SessionSubmit[
			ScheduledTask[
			(
				msg = SocketReadByteArrayFuncNoWait[socketIn];
				If[Length[msg]>0,
					socketEventHandler[msg];
					,
					SocketWaitNext[poller];
				]
			),
			0.0001 negligeable compared to IO operations ~1ms. We basically need 0 but can't use this value.
			],
			Method->"Idle",
			HandlerFunctions-><|"TaskStarted"->SendAck[]|>
		];
	];
	*)
];
(* can be useful for loopback connections which are available only if a task can be used. 
Does not kill the kernel *)
ClientLibrary`disconnect[] := Quit[];
ClientLibrary`disconnect[] /; ($SocketListener =!= None) := (
	DeleteObject[$SocketListener];
	Scan[
		If[# =!= None, Close[#]] &,
		{$LoggerSocket, $OutputSocket, $InputSocket}
	]
);
$SocketListener = None;
$LoggerSocket=None;
$OutputSocket=None;
$InputSocket=None;

$MaxMessagesReturned = 31;
$NoMessage = ByteArray[{0}];

KernelPrivateStart[inputsocket_String, outputsocket_String, logsocket_String, loglevel_Integer] := (
	$LoggerSocket=SocketConnect[logsocket,"ZMQ_PUB"];
	If[FailureQ[$LoggerSocket],
		Print["Failed to connect to logging socket: ", logsocket]
		,
		ClientLibrary`info["Connected to logging socket:", logsocket];
		setLogLevel[loglevel];
		addMessageHandler[];
		addPrintHandler[];
		KernelPrivateStart[inputsocket, outputsocket]
	];
);


KernelPrivateStart[inputsocket_String, outputsocket_String] := Block[
	{listener, msg},
	$InputSocket = SocketConnect[inputsocket, "ZMQ_Pull"];
	$OutputSocket = SocketConnect[outputsocket, "ZMQ_Push"];
	If[TrueQ@FailureQ[$OutputSocket],
		Print["Failed to connect to output socket ", outputsocket];
		Quit[]
	];
	If[TrueQ@FailureQ[$InputSocket],
		Print["Failed to connect to input socket ", inputsocket];
		Quit[]
	];
	If[$LoggerSocket==None,
		ClientLibrary`DisableKernelLogging[]
	];
	evaluationLoop[$InputSocket];
]

End[];
End[];
