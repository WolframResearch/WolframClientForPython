(* ::Package:: *)

(* Not useful since we apparently never receive multipart messages,
no matter the total size (tested with 80MB) *)


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

{SocketWriteByteArrayFunc, SocketReadByteArrayFunc} = If[
	$VersionNumber < 12,
	iSocketWriteByteArray[socket_,ba_ByteArray] := ZeroMQLink`Private`ZMQWriteInternal[socket, Normal[ba]];
	iSocketReadByteArray[uuid_String, flags_Integer]:= ByteArray@iRecvSingleMultipartMessageSocket[uuid, flags];
	{iSocketWriteByteArray, iSocketReadByteArray}
	,
	{ZMQSocketWriteMessage, iRecvSingleMultipartBinaryMessageSocket}
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
	];
	Print[msg]
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

writemsg[Hold[Message[msg_MessageName, args___], _]] := writemsg[
	ToString[Unevaluated[msg], InputForm], 
	fmtmsg[msg, args]
];
writemsg[msgname_String, msg_String] := (
	ClientLibrary`warn[msg];
	WriteString[$OutputSocket, 
		Developer`WriteRawJSONString[{msgname, msg}, "Compact"->True]
	];
);

serialize[expr_] := BinarySerialize[expr];

socketEventHandler[data_] := Block[
	{expr=$Failed, msgs = Internal`Bag[], msgCount, $MessageList={}},
	(* Setup a handler for all messages, and keep only those that haven't been silenced.
	The handler must deal with expressions of the form: 
		Hold[msg_, True|False]
	The boolean value indicates the silenced status On/Off. *)
	Internal`HandlerBlock[
		{"Message", Function[msg, If[TrueQ[Last[msg]],Internal`StuffBag[msgs,msg]]]},
		expr = timed[BinaryDeserialize[data], "Expression evaluation"];
		(* If[$LogLevel <= $DEBUG,  *)
			ClientLibrary`debug["deserialized expr:", expr]
		(* ]; *)
	];
	(* Check how many messages were thrown during evaluation.
	Cap it with a default value to avoid overflow. *)
	msgCount = Internal`BagLength[msgs];
	If[msgCount > 0, 
		ClientLibrary`info["Message count: ", msgCount]
	];
	Which[
		msgCount == 0,
		WriteString[$OutputSocket, "0"]
		,
		msgCount <= $MaxMessagesReturned,
		WriteString[$OutputSocket, ToString[msgCount]];
		Scan[
			writemsg,
			Internal`BagPart[msgs, All]
		]
		,
		msgCount > $MaxMessagesReturned,
		WriteString[$OutputSocket, ToString[$MaxMessagesReturned+1]];
		Scan[
			writemsg,
			Internal`BagPart[msgs, ;;$MaxMessagesReturned]
		];
		writemsg[TemplateApply["`` more messages issued during evaluation.", {msgCount-$MaxMessagesReturned}]];
		,
		_,
		ClientLibrary`error["Unexpected message count. Ignoring all messages."];
		WriteString[$OutputSocket, "0"];
	];
	SocketWriteByteArrayFunc[
		$OutputSocket,
		serialize[expr]
	];
	ClientLibrary`debug["End of evaluation."];
];

SendAck[] := WriteString[$OutputSocket, "OK"]

$MaxIdlePause=.001;
$MinIdlePause=0.0001;
$PauseIncrement=0.0001;
$TaskSupportMinVersion = Infinity;

Which[
	$VersionNumber < $TaskSupportMinVersion,
	(* Low CPU wait but need synchronous loop. *)
	evaluationLoop[socketIn_SocketObject]:= With[
		{maxPause=$MaxIdlePause, minPause=$MinIdlePause, incr=$PauseIncrement, 
		uuidIn=First@socketIn, poller={socketIn}},
		Block[{msg},
			SendAck[];
			While[True,
				msg = SocketReadByteArrayFunc[uuidIn, 1 (* NOWAIT *)];
				If[Length[msg]>3, 
					socketEventHandler[msg[[4;;]]];
					,
					SocketWaitNext[poller];
				]
			]
		]
	],
	True,
	(* Version with fixed asynchronous tasks *)
	evaluationLoop[socketIn_SocketObject]:= With[
		{maxPause=$MaxIdlePause, minPause=$MinIdlePause, incr=$PauseIncrement, 
		uuidIn=First@socketIn, poller={socketIn}}, 
		$Task = SessionSubmit[
			ScheduledTask[
			(
				msg = SocketReadByteArrayFunc[uuidIn, 1 (* NOWAIT *)];
				If[Length[msg]>3, 
					socketEventHandler[msg[[4;;]]];
					,
					SocketWaitNext[poller];
				]
			),
			0.0001 (*negligeable compared to IO operations ~1ms. We basically need 0 but can't use this value. *)
			],
			Method->"Idle",
			HandlerFunctions-><|"TaskStarted"->SendAck[]|>
		];
	];
];
(* can be useful for loopback connections which are available only if a task can be used. 
Does not kill the kernel *)
ClientLibrary`disconnect[] := Quit[];
ClientLibrary`disconnect[] /; ($Task =!= None) := (
	TaskRemove[$Task];
	Scan[
		If[# =!= None, Close[#]] &,
		{$LoggerSocket, $OutputSocket, $InputSocket}
	]
);
$Task = None;
$LoggerSocket=None;
$OutputSocket=None;
$InputSocket=None;

$MaxMessagesReturned = 31;
$NoMessage = ByteArray[{0}];

SlaveKernelPrivateStart[inputsocket_String, outputsocket_String, logsocket_String, loglevel_Integer] := (
	$LoggerSocket=SocketConnect[logsocket,"ZMQ_PUB"];
	If[FailureQ[$LoggerSocket],
		Print["Failed to connect to logging socket: ", logsocket]
		,
		ClientLibrary`info["Connected to logging socket:", logsocket];
		setLogLevel[loglevel];
		SlaveKernelPrivateStart[inputsocket, outputsocket]
	];
);


SlaveKernelPrivateStart[inputsocket_String, outputsocket_String] := Block[
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
	If[$LoggerSocket==None, ClientLibrary`DisableKernelLogging[]];
	evaluationLoop[$InputSocket];
]

End[];
End[];
