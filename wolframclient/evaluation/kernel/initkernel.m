(* ::Package:: *)

(* Not useful since we apparently never receive multipart messages,
no matter the total size (tested with 80MB) *)

$IgnoreEOF=True
$HistoryLength=0

Needs["ZeroMQLink`"];

Begin["ClientLibrary`"];

debug;
info;
warn;
error;

Begin["`Private`"];

SocketWriteFunc = If[
	$VersionNumber < 12,
	BinaryWrite,
	ZMQSocketWriteMessage
];

$DEBUG=1;
$INFO=2;
$WARN=3;
$ERROR=4;
$NOTSET=Infinity;

$LogLevel = $DEBUG;

ClientLibrary`debug[msg__String] := log[msg, $DEBUG];
ClientLibrary`info[msg__String] := log[msg, $INFO];
ClientLibrary`warn[msg__String] := log[msg, $WARN];
ClientLibrary`error[msg__String] := log[msg, $ERROR];

ClientLibrary`SetDebugLogLevel[] := setLogLevel[$DEBUG];
ClientLibrary`SetInfoLogLevel[] := setLogLevel[$INFO];
ClientLibrary`SetWarnLogLevel[] := setLogLevel[$WARN];
ClientLibrary`SetErrorLogLevel[] := setLogLevel[$ERROR];
ClientLibrary`DisableKernelLogging[] := setLogLevel[$NOTSET];

setLogLevel[level:_Integer] /; $DEBUG <= level <= $ERROR := 
	($LogLevel = level);
setLogLevel[$NOTSET] := ($LogLevel = $NOTSET);

log[msg_String, level_Integer:$INFO] /; level>=$LogLevel := If[$LoggerSocket=!=None, 
	BinaryWrite[
		$LoggerSocket, 
		Developer`WriteRawJSONString[<|"msg"->msg, "level"->level|>, "ToByteString"->True]
	],
	Print[msg]
];
log[msg__String, level_Integer:$INFO] /; level>=$LogLevel := 
	log[StringJoin[{msg}], level];
log[msg__String, False, level_Integer:$INFO] /; level>=$LogLevel := 
	Map[log[#, level]&, {msg}];


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

(* evaluate[data_String] := ToExpression[data];
evaluate[data_ByteArray] := evaluate[ByteArrayToString[data]];

serialize[expr_] := ToString[expr, InputForm]; *)

evaluate[data_ByteArray] := BinaryDeserialize[data];

serialize[expr_] := BinarySerialize[expr];

$LoggerSocket=None;
$OutputSocket=None;
$InputSocket=None;

$MaxMessagesReturned = 31;
$NoMessage = ByteArray[{0}];

SlaveKernelPrivateStart[inputsocket_String, outputsocket_String, logsocket_String] := (
	$LoggerSocket=SocketConnect[logsocket,"ZMQ_PUB"];
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
		Print["Failed to connect to output socket ", outputsocket];
		Quit[]
	];
	If[TrueQ@FailureQ[$InputSocket],
		Print["Failed to connect to input socket ", inputsocket];
		Quit[]
	];
	If[$LoggerSocket==None, ClientLibrary`DisableKernelLogging[]];
	listener = SocketListen[
		$InputSocket,
	
		Block[{data, expr=$Failed, msgs = Internal`Bag[], msgCount, $MessageList={}},
			(* Setup a handler for all messages, and keep only those that haven't been silenced.
			The handler must deal with expressions of the form: 
				Hold[msg_, True|False]
			The boolean value indicates the silenced status On/Off. *)
			Internal`HandlerBlock[
				{"Message", If[TrueQ[Last[#]],Internal`StuffBag[msgs,#]] &},
				data = Lookup[#,"DataByteArray", None];
				expr = timed[evaluate[data], "Expression evaluation"];
				If[$LogLevel >= $DEBUG, ClientLibrary`debug["deserialized expr: ", ToString[expr]]];
			];
			(* Check how many messages were thrown during evaluation.
			Cap it with a default value to avoid overflow. *)
			msgCount = Internal`BagLength[msgs];
			If[msgCount > 0, 
				ClientLibrary`info["Message count: ", ToString[msgCount]]
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
				ClientLibrary`fatal["Unexpected message count. Ignoring all messages."];
				WriteString[$OutputSocket, "0"];
			];
			SocketWriteFunc[
				$OutputSocket,
				serialize[expr]
			];
			If[$LogLevel >= $DEBUG, ClientLibrary`debug["End of evaluation."]]
		] &
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
