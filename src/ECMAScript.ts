import { grammars } from "./Grammar";
import { any, nextIsNot, oneOf, text, token } from "./Matcher";

// https://tc39.es/ecma262/2022/multipage/ecmascript-language-lexical-grammar.html#sec-ecmascript-language-lexical-grammar
const ECMAScriptLexicalGrammar = grammars`
// https://tc39.es/ecma262/2022/multipage/ecmascript-language-lexical-grammar.html#prod-InputElementDiv
InputElementDiv ::
	WhiteSpace
	LineTerminator
	Comment
	CommonToken
	DivPunctuator
	RightBracePunctuator

// https://tc39.es/ecma262/2022/multipage/ecmascript-language-lexical-grammar.html#sec-white-space
WhiteSpace ::
	${/[\u0009\u000B\u000C\uFEFF\p{Space_Separator}]/u}

// https://tc39.es/ecma262/2022/multipage/ecmascript-language-lexical-grammar.html#sec-line-terminators
LineTerminator ::
	${/[\u000A\u000D\u2028\u2029]/u}
LineTerminatorSequence ::
	${/[\u000A\u000D\u2028\u2029]|\u000D\u000A/u}
	
// https://tc39.es/ecma262/2022/multipage/ecmascript-language-lexical-grammar.html#sec-comments
Comment ::
	MultiLineComment
	SingleLineComment
MultiLineComment ::
	"/*" MultiLineCommentCharsopt "*/"
MultiLineCommentChars ::
	MultiLineNotAsteriskChar MultiLineCommentCharsopt
	"*" ${nextIsNot(text("/"))} PostAsteriskCommentCharsopt
PostAsteriskCommentChars ::
	MultiLineNotForwardSlashOrAsteriskChar MultiLineCommentCharsopt
	"*" ${nextIsNot(text("/"))} PostAsteriskCommentCharsopt
MultiLineNotAsteriskChar ::
	${any(1).andNot(text("*"))}
MultiLineNotForwardSlashOrAsteriskChar ::
	${any(1).andNot(/[\/*]/)}
SingleLineComment ::
	"//" SingleLineCommentCharsopt
SingleLineCommentChars ::
	SingleLineCommentChar SingleLineCommentCharsopt
SingleLineCommentChar ::
	${/[^\u000A\u000D\u2028\u2029]/u}
	
// https://tc39.es/ecma262/2022/multipage/ecmascript-language-lexical-grammar.html#prod-CommonToken
CommonToken ::
	IdentifierName
	PrivateIdentifier
	Punctuator
	NumericLiteral
	StringLiteral
	Template
	
// https://tc39.es/ecma262/2022/multipage/ecmascript-language-lexical-grammar.html#prod-IdentifierName
PrivateIdentifier ::
	"#" IdentifierName
IdentifierName ::
	IdentifierStart
	IdentifierName IdentifierPart
IdentifierStart ::
	IdentifierStartChar
	"\\" UnicodeEscapeSequence
IdentifierPart ::
	IdentifierPartChar
	"\\" UnicodeEscapeSequence
IdentifierStartChar ::
	UnicodeIDStart
	"$"
	"_"
IdentifierPartChar ::
	UnicodeIDContinue
	"$"
	"\u200C"
	"\u200D"
UnicodeIDStart ::
	${/\p{ID_Start}/u}
UnicodeIDContinue ::
	${/\p{ID_Continue}/u}
	
// https://tc39.es/ecma262/2022/multipage/ecmascript-language-lexical-grammar.html#prod-Punctuator
Punctuator ::
	OptionalChainingPunctuator
	OtherPunctuator
OptionalChainingPunctuator ::
	"?." ${nextIsNot("DecimalDigit")}
OtherPunctuator ::
	"{"
	"("
	")"
	"["
	"]"
	"."
	"..."
	";"
	","
	"<"
	">"
	"<="
	">="
	"=="
	"!="
	"==="
	"!=="
	"+"
	"-"
	"*"
	"%"
	"**"
	"++"
	"--"
	"<<"
	">>"
	">>>"
	"&"
	"|"
	"^"
	"!"
	"~"
	"&&"
	"||"
	"??"
	"?"
	":"
	"="
	"+="
	"-="
	"*="
	"%="
	"**="
	"<<="
	">>="
	">>>="
	"&="
	"|="
	"^="
	"&&="
	"||="
	"??="
	"=>"
DivPunctuator ::
	"/"
	"/="
RightBracePunctuator ::
	"}"

// https://tc39.es/ecma262/2022/multipage/ecmascript-language-lexical-grammar.html#sec-null-literals
NullLiteral ::
	"null"

// https://tc39.es/ecma262/2022/multipage/ecmascript-language-lexical-grammar.html#sec-null-literals
BooleanLiteral ::
	"true"
	"false"

// https://tc39.es/ecma262/2022/multipage/ecmascript-language-lexical-grammar.html#prod-NumericLiteral
NumericLiteralSeparator ::
	"_"
NumericLiteral ::
	DecimalLiteral
	DecimalBigIntegerLiteral
	NonDecimalIntegerLiteral[+Sep]
	NonDecimalIntegerLiteral[+Sep] BigIntLiteralSuffix
	LegacyOctalIntegerLiteral
DecimalBigIntegerLiteral ::
	"0" BigIntLiteralSuffix
	NonZeroDigit DecimalDigits[+Sep]opt BigIntLiteralSuffix
	NonZeroDigit NumericLiteralSeparator DecimalDigits[+Sep] BigIntLiteralSuffix
NonDecimalIntegerLiteral[Sep] ::
	BinaryIntegerLiteral[?Sep]
	OctalIntegerLiteral[?Sep]
	HexIntegerLiteral[?Sep]
BigIntLiteralSuffix ::
	"n"
DecimalLiteral ::
	DecimalIntegerLiteral "." DecimalDigits[+Sep]opt ExponentPart[+Sep]opt
	"." DecimalDigits[+Sep] ExponentPart[+Sep]opt
	DecimalIntegerLiteral ExponentPart[+Sep]opt
DecimalIntegerLiteral ::
	"0"
	NonZeroDigit
	NonZeroDigit NumericLiteralSeparatoropt DecimalDigits[+Sep]
	NonOctalDecimalIntegerLiteral
DecimalDigits[Sep] ::
	DecimalDigit
	DecimalDigits[?Sep] DecimalDigit
	[+Sep] DecimalDigits[+Sep] NumericLiteralSeparator DecimalDigit
DecimalDigit ::
	${/[0-9]/}
NonZeroDigit ::
	${/[1-9]/}
ExponentPart[Sep] ::
	ExponentIndicator SignedInteger[?Sep]
ExponentIndicator ::
	${/[eE]/}
SignedInteger[Sep] ::
	DecimalDigits[?Sep]
	"+" DecimalDigits[?Sep]
	"-" DecimalDigits[?Sep]
BinaryIntegerLiteral[Sep] ::
	"0b" BinaryDigits[?Sep]
	"0B" BinaryDigits[?Sep]
BinaryDigits[Sep] ::
	BinaryDigit
	BinaryDigits[?Sep] BinaryDigit
	[+Sep] BinaryDigits[+Sep] NumericLiteralSeparator BinaryDigit
BinaryDigit ::
	${/[01]/}
OctalIntegerLiteral[Sep] ::
	"0o" OctalDigits[?Sep]
	"0O" OctalDigits[?Sep]
OctalDigits[Sep] ::
	OctalDigit
	OctalDigits[?Sep] OctalDigit
	[+Sep] OctalDigits[+Sep] NumericLiteralSeparator OctalDigit
LegacyOctalIntegerLiteral ::
	"0" OctalDigit
	LegacyOctalIntegerLiteral OctalDigit
NonOctalDecimalIntegerLiteral ::
	"0" NonOctalDigit
	LegacyOctalLikeDecimalIntegerLiteral NonOctalDigit
	NonOctalDecimalIntegerLiteral DecimalDigit
LegacyOctalLikeDecimalIntegerLiteral ::
	"0" OctalDigit
	LegacyOctalLikeDecimalIntegerLiteral OctalDigit
OctalDigit ::
	${/[0-7]/}
NonOctalDigit ::
	${/[89]/}
HexIntegerLiteral[Sep] ::
	"0x" HexDigits[?Sep]
	"0X" HexDigits[?Sep]
HexDigits[Sep] ::
	HexDigit
	HexDigits[?Sep] HexDigit
	[+Sep] HexDigits[+Sep] NumericLiteralSeparator HexDigit
HexDigit ::
	${/[0-9a-fA-F]/}
	
// https://tc39.es/ecma262/2022/multipage/ecmascript-language-lexical-grammar.html#prod-StringLiteral
StringLiteral ::
	'"' DoubleStringCharactersopt '"'
	"'" SingleStringCharactersopt "'"
DoubleStringCharacters ::
	DoubleStringCharacter DoubleStringCharactersopt
SingleStringCharacters ::
	SingleStringCharacter SingleStringCharactersopt
DoubleStringCharacter ::
	${/[^"\\]/}
	${/[\u2028\u2029]/}
	"\\" EscapeSequence
	LineContinuation
SingleStringCharacter ::
	${/[^'\\]/}
	${/[\u2028\u2029]/}
	"\\" EscapeSequence
	LineContinuation
LineContinuation ::
	"\\" LineTerminatorSequence
EscapeSequence ::
	CharacterEscapeSequence
	"0" ${nextIsNot("DecimalDigit")}
	LegacyOctalEscapeSequence
	NonOctalDecimalEscapeSequence
	HexEscapeSequence
	UnicodeEscapeSequence
CharacterEscapeSequence ::
	SingleEscapeCharacter
	NonEscapeCharacter
SingleEscapeCharacter ::
	${/['"\\bfnrtv]/}
NonEscapeCharacter ::
	${any(1).andNot("EscapeCharacter").andNot("LineTerminator")}
EscapeCharacter ::
	SingleEscapeCharacter
	DecimalDigit
	"x"
	"u"
LegacyOctalEscapeSequence ::
	"0" ${nextIsNot(/[89]/)}
	NonZeroOctalDigit ${nextIsNot("OctalDigit")}
	ZeroToThree OctalDigit ${nextIsNot("OctalDigit")}
	FourToSeven OctalDigit
	ZeroToThree OctalDigit OctalDigit
NonZeroOctalDigit ::
	${token("OctalDigit").andNot(text("0"))}
ZeroToThree ::
	${/[0123]/}
FourToSeven ::
	${/[4567]/}
NonOctalDecimalEscapeSequence ::
	${/[89]/}
HexEscapeSequence ::
	"x" HexDigit HexDigit
UnicodeEscapeSequence ::
	"u" Hex4Digits
	"u{" CodePoint "}"
Hex4Digits ::
	HexDigit HexDigit HexDigit HexDigit
	
// https://tc39.es/ecma262/2022/multipage/ecmascript-language-lexical-grammar.html#sec-literals-regular-expression-literals
RegularExpressionLiteral ::
	"/" RegularExpressionBody "/" RegularExpressionFlags
RegularExpressionBody ::
	RegularExpressionFirstChar RegularExpressionChars
RegularExpressionChars ::
	${any(0)}
	RegularExpressionChars RegularExpressionChar
RegularExpressionFirstChar ::
	${token("RegularExpressionNonTerminator").andNot(/[*\\/[]/)}
	RegularExpressionBackslashSequence
	RegularExpressionClass
RegularExpressionChar ::
	${token("RegularExpressionNonTerminator").andNot(/[\\/[]/)}
	RegularExpressionBackslashSequence
	RegularExpressionClass
RegularExpressionBackslashSequence ::
	"\\" RegularExpressionNonTerminator
RegularExpressionNonTerminator ::
	${any(1).andNot("LineTerminator")}
RegularExpressionClass ::
	"[" RegularExpressionClassChars "]"
RegularExpressionClassChars ::
	${any(0)}
	RegularExpressionClassChars RegularExpressionClassChar
RegularExpressionClassChar ::
	${token("RegularExpressionNonTerminator").andNot(/]\\/)}
	RegularExpressionBackslashSequence
RegularExpressionFlags ::
	${any(0)}
	RegularExpressionFlags IdentifierPartChar

// https://tc39.es/ecma262/2022/multipage/ecmascript-language-lexical-grammar.html#prod-Template
Template ::
	NoSubstitutionTemplate
	TemplateHead
NoSubstitutionTemplate ::
	${text("`")} TemplateCharactersopt ${text("`")}
TemplateHead ::
	${text("`")} TemplateCharactersopt ${text("${")}
TemplateSubstitutionTail ::
	TemplateMiddle
	TemplateTail
TemplateMiddle ::
	"}" TemplateCharactersopt ${text("${")}
TemplateTail ::
	"}" TemplateCharactersopt ${text("`")}
TemplateCharacters ::
	TemplateCharacter TemplateCharactersopt
TemplateCharacter ::
	${text("$")} ${nextIsNot(text("{"))}
	"\\" TemplateEscapeSequence
	"\\" NotEscapeSequence
	LineContinuation
	LineTerminatorSequence
	${any(1)
		.andNot(text("`"))
		.andNot(text("\\"))
		.andNot(text("$"))
		.andNot("LineTerminator")}
TemplateEscapeSequence ::
	CharacterEscapeSequence
	"0" ${nextIsNot("DecimalDigit")}
	HexEscapeSequence
	UnicodeEscapeSequence
NotEscapeSequence ::
	"0" DecimalDigit
	${token("DecimalDigit").andNot(text("0"))}
	"x" ${nextIsNot("HexDigit")}
	"x" HexDigit ${nextIsNot("HexDigit")}
	"u" ${nextIsNot("HexDigit")} ${nextIsNot(text("{"))}
	"u" HexDigit ${nextIsNot("HexDigit")}
	"u" HexDigit HexDigit ${nextIsNot("HexDigit")}
	"u" HexDigit HexDigit HexDigit ${nextIsNot("HexDigit")}
	"u" "{" ${nextIsNot("HexDigit")}
	"u" "{" NotCodePoint ${nextIsNot("HexDigit")}
	"u" "{" CodePoint ${nextIsNot("HexDigit")} ${nextIsNot(text("}"))}
NotCodePoint ::
	// HexDigits[~Sep] but only if MV of HexDigits > 0x10FFFF
CodePoint ::
	// HexDigits[~Sep] but only if MV of HexDigits ≤ 0x10FFFF

// https://tc39.es/ecma262/2022/multipage/ecmascript-language-lexical-grammar.html#prod-ReservedWord
ReservedWord ::
	"await" 
	"break" 
	"case" 
	"catch" 
	"class" 
	"const" 
	"continue" 
	"debugger" 
	"default" 
	"delete" 
	"do" 
	"else"
 	"enum" 
 	"export"
 	"extends"
 	"false"
	"finally"
	"for"
	"function"
	"if"
	"import"
	"in"
	"instanceof"
	"new"
	"null"
	"return"
	"super"
	"switch"
	"this"
	"throw"
	"true"
	"try"
	"typeof"
	"var"
	"void"
	"while"
	"with"
	"yield"
`;

// https://tc39.es/ecma262/2022/multipage/notational-conventions.html#sec-syntactic-grammar
const ECMAScriptSyntacticGrammarForExpression = grammars`
// https://tc39.es/ecma262/2022/multipage/ecmascript-language-expressions.html#sec-identifiers
IdentifierReference[Yield, Await] :
	Identifier
	[~Yield] "yield"
	[~Await] "await"
BindingIdentifier[Yield, Await] :
	Identifier
	"yield"
	"await"
LabelIdentifier[Yield, Await] :
	Identifier
	[~Yield] "yield"
	[~Await] "await"
Identifier :
	${token("IdentifierName").andNot("ReservedWord")}
	
// https://tc39.es/ecma262/2022/multipage/ecmascript-language-expressions.html#sec-primary-expression
PrimaryExpression[Yield, Await] :
	"this"
	IdentifierReference[?Yield, ?Await]
	Literal
	ArrayLiteral[?Yield, ?Await]
	ObjectLiteral[?Yield, ?Await]
	FunctionExpression
	ClassExpression[?Yield, ?Await]
	GeneratorExpression
	AsyncFunctionExpression
	AsyncGeneratorExpression
	RegularExpressionLiteral
	TemplateLiteral[?Yield, ?Await, ~Tagged]
	CoverParenthesizedExpressionAndArrowParameterList[?Yield, ?Await]
CoverParenthesizedExpressionAndArrowParameterList[Yield, Await] :
	"(" Expression[+In, ?Yield, ?Await] ")"
	"(" Expression[+In, ?Yield, ?Await] "," ")"
	"(" ")"
	"(" "..." BindingIdentifier[?Yield, ?Await] ")"
	"(" "..." BindingPattern[?Yield, ?Await] ")"
	"(" Expression[+In, ?Yield, ?Await] "," "..." BindingIdentifier[?Yield, ?Await] ")"
	"(" Expression[+In, ?Yield, ?Await] "," "..." BindingPattern[?Yield, ?Await] ")"

// https://tc39.es/ecma262/2022/multipage/ecmascript-language-expressions.html#sec-primary-expression-literals
Literal :
	NullLiteral
	BooleanLiteral
	NumericLiteral
	StringLiteral

// https://tc39.es/ecma262/2022/multipage/ecmascript-language-expressions.html#sec-array-initializer
ArrayLiteral[Yield, Await] :
	"[" Elisionopt "]"
	"[" ElementList[?Yield, ?Await] "]"
	"[" ElementList[?Yield, ?Await] "," Elisionopt "]"
ElementList[Yield, Await] :
	Elisionopt AssignmentExpression[+In, ?Yield, ?Await]
	Elisionopt SpreadElement[?Yield, ?Await]
	ElementList[?Yield, ?Await] "," Elisionopt AssignmentExpression[+In, ?Yield, ?Await]
	ElementList[?Yield, ?Await] "," Elisionopt SpreadElement[?Yield, ?Await]
Elision :
	","
	Elision ","
SpreadElement[Yield, Await] :
	"..." AssignmentExpression[+In, ?Yield, ?Await]
	
// https://tc39.es/ecma262/2022/multipage/ecmascript-language-expressions.html#sec-object-initializer
ObjectLiteral[Yield, Await] :
	"{" "}"
	"{" PropertyDefinitionList[?Yield, ?Await] "}"
	"{" PropertyDefinitionList[?Yield, ?Await] "," "}"
PropertyDefinitionList[Yield, Await] :
	PropertyDefinition[?Yield, ?Await]
	PropertyDefinitionList[?Yield, ?Await] "," PropertyDefinition[?Yield, ?Await]
PropertyDefinition[Yield, Await] :
	IdentifierReference[?Yield, ?Await]
	CoverInitializedName[?Yield, ?Await]
	PropertyName[?Yield, ?Await] ":" AssignmentExpression[+In, ?Yield, ?Await]
	MethodDefinition[?Yield, ?Await]
	"..." AssignmentExpression[+In, ?Yield, ?Await]
PropertyName[Yield, Await] :
	LiteralPropertyName
	ComputedPropertyName[?Yield, ?Await]
LiteralPropertyName :
	IdentifierName
	StringLiteral
	NumericLiteral
ComputedPropertyName[Yield, Await] :
	"[" AssignmentExpression[+In, ?Yield, ?Await] "]"
CoverInitializedName[Yield, Await] :
	IdentifierReference[?Yield, ?Await] Initializer[+In, ?Yield, ?Await]
Initializer[In, Yield, Await] :
	"=" AssignmentExpression[?In, ?Yield, ?Await]

// https://tc39.es/ecma262/2022/multipage/ecmascript-language-expressions.html#sec-template-literals
TemplateLiteral[Yield, Await, Tagged] :
	NoSubstitutionTemplate
	SubstitutionTemplate[?Yield, ?Await, ?Tagged]
SubstitutionTemplate[Yield, Await, Tagged] :
	TemplateHead Expression[+In, ?Yield, ?Await] TemplateSpans[?Yield, ?Await, ?Tagged]
TemplateSpans[Yield, Await, Tagged] :
	TemplateTail
	TemplateMiddleList[?Yield, ?Await, ?Tagged] TemplateTail
TemplateMiddleList[Yield, Await, Tagged] :
	TemplateMiddle Expression[+In, ?Yield, ?Await]
	TemplateMiddleList[?Yield, ?Await, ?Tagged] TemplateMiddle Expression[+In, ?Yield, ?Await]

// https://tc39.es/ecma262/2022/multipage/ecmascript-language-expressions.html#sec-left-hand-side-expressions
MemberExpression[Yield, Await] :
	PrimaryExpression[?Yield, ?Await]
	MemberExpression[?Yield, ?Await] "[" Expression[+In, ?Yield, ?Await] "]"
	MemberExpression[?Yield, ?Await] "." IdentifierName
	MemberExpression[?Yield, ?Await] TemplateLiteral[?Yield, ?Await, +Tagged]
	SuperProperty[?Yield, ?Await]
	MetaProperty
	"new" MemberExpression[?Yield, ?Await] Arguments[?Yield, ?Await]
	MemberExpression[?Yield, ?Await] "." PrivateIdentifier
SuperProperty[Yield, Await] :
	"super" "[" Expression[+In, ?Yield, ?Await] "]"
	"super" "." IdentifierName
MetaProperty :
	NewTarget
	ImportMeta
NewTarget :
	"new" "." target
ImportMeta :
	"import" "." "meta"
NewExpression[Yield, Await] :
	MemberExpression[?Yield, ?Await]
	"new" NewExpression[?Yield, ?Await]
CallExpression[Yield, Await] :
	CoverCallExpressionAndAsyncArrowHead[?Yield, ?Await]
	SuperCall[?Yield, ?Await]
	ImportCall[?Yield, ?Await]
	CallExpression[?Yield, ?Await] Arguments[?Yield, ?Await]
	CallExpression[?Yield, ?Await] "[" Expression[+In, ?Yield, ?Await] "]"
	CallExpression[?Yield, ?Await] "." IdentifierName
	CallExpression[?Yield, ?Await] TemplateLiteral[?Yield, ?Await, +Tagged]
	CallExpression[?Yield, ?Await] "." PrivateIdentifier
SuperCall[Yield, Await] :
	"super" Arguments[?Yield, ?Await]
ImportCall[Yield, Await] :
	"import" "(" AssignmentExpression[+In, ?Yield, ?Await] ")"
Arguments[Yield, Await] :
	"(" ")"
	"(" ArgumentList[?Yield, ?Await] ")"
	"(" ArgumentList[?Yield, ?Await] "," ")"
ArgumentList[Yield, Await] :
	AssignmentExpression[+In, ?Yield, ?Await]
	"..." AssignmentExpression[+In, ?Yield, ?Await]
	ArgumentList[?Yield, ?Await] "," AssignmentExpression[+In, ?Yield, ?Await]
	ArgumentList[?Yield, ?Await] "," "..." AssignmentExpression[+In, ?Yield, ?Await]
OptionalExpression[Yield, Await] :
	MemberExpression[?Yield, ?Await] OptionalChain[?Yield, ?Await]
	CallExpression[?Yield, ?Await] OptionalChain[?Yield, ?Await]
	OptionalExpression[?Yield, ?Await] OptionalChain[?Yield, ?Await]
OptionalChain[Yield, Await] :
	"?." Arguments[?Yield, ?Await]
	"?." "[" Expression[+In, ?Yield, ?Await] "]"
	"?." IdentifierName
	"?." TemplateLiteral[?Yield, ?Await, +Tagged]
	"?." PrivateIdentifier
	OptionalChain[?Yield, ?Await] Arguments[?Yield, ?Await]
	OptionalChain[?Yield, ?Await] "[" Expression[+In, ?Yield, ?Await] "]"
	OptionalChain[?Yield, ?Await] "." IdentifierName
	OptionalChain[?Yield, ?Await] TemplateLiteral[?Yield, ?Await, +Tagged]
	OptionalChain[?Yield, ?Await] "." PrivateIdentifier
LeftHandSideExpression[Yield, Await] :
	NewExpression[?Yield, ?Await]
	CallExpression[?Yield, ?Await]
	OptionalExpression[?Yield, ?Await]
	
// https://tc39.es/ecma262/2022/multipage/ecmascript-language-expressions.html#sec-update-expressions
UpdateExpression[Yield, Await] :
	LeftHandSideExpression[?Yield, ?Await]
	LeftHandSideExpression[?Yield, ?Await] ${nextIsNot("LineTerminator")} "++"
	LeftHandSideExpression[?Yield, ?Await] ${nextIsNot("LineTerminator")} "--"
	"++" UnaryExpression[?Yield, ?Await]
	"--" UnaryExpression[?Yield, ?Await]
	
// https://tc39.es/ecma262/2022/multipage/ecmascript-language-expressions.html#sec-unary-operators
UnaryExpression[Yield, Await] :
	UpdateExpression[?Yield, ?Await]
	"delete" UnaryExpression[?Yield, ?Await]
	"void" UnaryExpression[?Yield, ?Await]
	"typeof" UnaryExpression[?Yield, ?Await]
	"+" UnaryExpression[?Yield, ?Await]
	"-" UnaryExpression[?Yield, ?Await]
	"~" UnaryExpression[?Yield, ?Await]
	"!" UnaryExpression[?Yield, ?Await]
	[+Await] AwaitExpression[?Yield]
	
// https://tc39.es/ecma262/2022/multipage/ecmascript-language-expressions.html#sec-exp-operator
ExponentiationExpression[Yield, Await] :
	UnaryExpression[?Yield, ?Await]
	UpdateExpression[?Yield, ?Await] "**" ExponentiationExpression[?Yield, ?Await]

// https://tc39.es/ecma262/2022/multipage/ecmascript-language-expressions.html#sec-multiplicative-operators
MultiplicativeExpression[Yield, Await] :
	ExponentiationExpression[?Yield, ?Await]
	MultiplicativeExpression[?Yield, ?Await] MultiplicativeOperator ExponentiationExpression[?Yield, ?Await]
MultiplicativeOperator :
	"*" 
	"/" 
	"%"
	
// https://tc39.es/ecma262/2022/multipage/ecmascript-language-expressions.html#sec-additive-operators
AdditiveExpression[Yield, Await] :
	MultiplicativeExpression[?Yield, ?Await]
	AdditiveExpression[?Yield, ?Await] "+" MultiplicativeExpression[?Yield, ?Await]
	AdditiveExpression[?Yield, ?Await] "-" MultiplicativeExpression[?Yield, ?Await]

// https://tc39.es/ecma262/2022/multipage/ecmascript-language-expressions.html#sec-bitwise-shift-operators
ShiftExpression[Yield, Await] :
	AdditiveExpression[?Yield, ?Await]
	ShiftExpression[?Yield, ?Await] "<<" AdditiveExpression[?Yield, ?Await]
	ShiftExpression[?Yield, ?Await] ">>" AdditiveExpression[?Yield, ?Await]
	ShiftExpression[?Yield, ?Await] ">>>" AdditiveExpression[?Yield, ?Await]

// https://tc39.es/ecma262/2022/multipage/ecmascript-language-expressions.html#sec-relational-operators
RelationalExpression[In, Yield, Await] :
	ShiftExpression[?Yield, ?Await]
	RelationalExpression[?In, ?Yield, ?Await] "<" ShiftExpression[?Yield, ?Await]
	RelationalExpression[?In, ?Yield, ?Await] ">" ShiftExpression[?Yield, ?Await]
	RelationalExpression[?In, ?Yield, ?Await] "<=" ShiftExpression[?Yield, ?Await]
	RelationalExpression[?In, ?Yield, ?Await] ">=" ShiftExpression[?Yield, ?Await]
	RelationalExpression[?In, ?Yield, ?Await] "instanceof" ShiftExpression[?Yield, ?Await]
	[+In] RelationalExpression[+In, ?Yield, ?Await] "in" ShiftExpression[?Yield, ?Await]
	[+In] PrivateIdentifier "in" ShiftExpression[?Yield, ?Await]
	
// https://tc39.es/ecma262/2022/multipage/ecmascript-language-expressions.html#sec-equality-operators
EqualityExpression[In, Yield, Await] :
	RelationalExpression[?In, ?Yield, ?Await]
	EqualityExpression[?In, ?Yield, ?Await] "==" RelationalExpression[?In, ?Yield, ?Await]
	EqualityExpression[?In, ?Yield, ?Await] "!=" RelationalExpression[?In, ?Yield, ?Await]
	EqualityExpression[?In, ?Yield, ?Await] "===" RelationalExpression[?In, ?Yield, ?Await]
	EqualityExpression[?In, ?Yield, ?Await] "!==" RelationalExpression[?In, ?Yield, ?Await]

// https://tc39.es/ecma262/2022/multipage/ecmascript-language-expressions.html#sec-binary-bitwise-operators
BitwiseANDExpression[In, Yield, Await] :
	EqualityExpression[?In, ?Yield, ?Await]
	BitwiseANDExpression[?In, ?Yield, ?Await] "&" EqualityExpression[?In, ?Yield, ?Await]
BitwiseXORExpression[In, Yield, Await] :
	BitwiseANDExpression[?In, ?Yield, ?Await]
	BitwiseXORExpression[?In, ?Yield, ?Await] "^" BitwiseANDExpression[?In, ?Yield, ?Await]
BitwiseORExpression[In, Yield, Await] :
	BitwiseXORExpression[?In, ?Yield, ?Await]
	BitwiseORExpression[?In, ?Yield, ?Await] "|" BitwiseXORExpression[?In, ?Yield, ?Await]
	
// https://tc39.es/ecma262/2022/multipage/ecmascript-language-expressions.html#sec-binary-logical-operators
LogicalANDExpression[In, Yield, Await] :
	BitwiseORExpression[?In, ?Yield, ?Await]
	LogicalANDExpression[?In, ?Yield, ?Await] "&&" BitwiseORExpression[?In, ?Yield, ?Await]
LogicalORExpression[In, Yield, Await] :
	LogicalANDExpression[?In, ?Yield, ?Await]
	LogicalORExpression[?In, ?Yield, ?Await] "||" LogicalANDExpression[?In, ?Yield, ?Await]
CoalesceExpression[In, Yield, Await] :
	CoalesceExpressionHead[?In, ?Yield, ?Await] "??" BitwiseORExpression[?In, ?Yield, ?Await]
CoalesceExpressionHead[In, Yield, Await] :
	CoalesceExpression[?In, ?Yield, ?Await]
	BitwiseORExpression[?In, ?Yield, ?Await]
ShortCircuitExpression[In, Yield, Await] :
	LogicalORExpression[?In, ?Yield, ?Await]
	CoalesceExpression[?In, ?Yield, ?Await]	

// https://tc39.es/ecma262/2022/multipage/ecmascript-language-expressions.html#sec-conditional-operator
ConditionalExpression[In, Yield, Await] :
	ShortCircuitExpression[?In, ?Yield, ?Await]
	ShortCircuitExpression[?In, ?Yield, ?Await] "?" AssignmentExpression[+In, ?Yield, ?Await] ":" AssignmentExpression[?In, ?Yield, ?Await]
	
// https://tc39.es/ecma262/2022/multipage/ecmascript-language-expressions.html#sec-assignment-operators
AssignmentExpression[In, Yield, Await] :
	ConditionalExpression[?In, ?Yield, ?Await]
	[+Yield] YieldExpression[?In, ?Await]
	ArrowFunction[?In, ?Yield, ?Await]
	AsyncArrowFunction[?In, ?Yield, ?Await]
	LeftHandSideExpression[?Yield, ?Await] "=" AssignmentExpression[?In, ?Yield, ?Await]
	LeftHandSideExpression[?Yield, ?Await] AssignmentOperator AssignmentExpression[?In, ?Yield, ?Await]
	LeftHandSideExpression[?Yield, ?Await] "&&=" AssignmentExpression[?In, ?Yield, ?Await]
	LeftHandSideExpression[?Yield, ?Await] "||=" AssignmentExpression[?In, ?Yield, ?Await]
	LeftHandSideExpression[?Yield, ?Await] "??=" AssignmentExpression[?In, ?Yield, ?Await]
AssignmentOperator :
	"*="
	"/="
	"%="
	"+="
	"-="
	"<<="
	">>="
	">>>="
	"&="
	"^="
	"|="
	"**="
	
// https://tc39.es/ecma262/2022/multipage/ecmascript-language-expressions.html#sec-comma-operator
Expression[In, Yield, Await] :
	AssignmentExpression[?In, ?Yield, ?Await]
	Expression[?In, ?Yield, ?Await] "," AssignmentExpression[?In, ?Yield, ?Await]
`;

// https://tc39.es/ecma262/2022/multipage/notational-conventions.html#sec-syntactic-grammar
const ECMAScriptSyntacticStatementsAndDeclarations = grammars`
// https://tc39.es/ecma262/2022/multipage/ecmascript-language-statements-and-declarations.html#sec-ecmascript-language-statements-and-declarations
Statement[Yield, Await, Return] :
	BlockStatement[?Yield, ?Await, ?Return]
	VariableStatement[?Yield, ?Await]
	EmptyStatement
	ExpressionStatement[?Yield, ?Await]
	IfStatement[?Yield, ?Await, ?Return]
	BreakableStatement[?Yield, ?Await, ?Return]
	ContinueStatement[?Yield, ?Await]
	BreakStatement[?Yield, ?Await]
	[+Return] ReturnStatement[?Yield, ?Await]
	// WithStatement[?Yield, ?Await, ?Return]
	LabelledStatement[?Yield, ?Await, ?Return]
	ThrowStatement[?Yield, ?Await]
	TryStatement[?Yield, ?Await, ?Return]
	DebuggerStatement
Declaration[Yield, Await] :
	HoistableDeclaration[?Yield, ?Await, ~Default]
	ClassDeclaration[?Yield, ?Await, ~Default]
	LexicalDeclaration[+In, ?Yield, ?Await]
HoistableDeclaration[Yield, Await, Default] :
	FunctionDeclaration[?Yield, ?Await, ?Default]
	GeneratorDeclaration[?Yield, ?Await, ?Default]
	AsyncFunctionDeclaration[?Yield, ?Await, ?Default]
	AsyncGeneratorDeclaration[?Yield, ?Await, ?Default]
BreakableStatement[Yield, Await, Return] :
	IterationStatement[?Yield, ?Await, ?Return]
	SwitchStatement[?Yield, ?Await, ?Return]
	
// https://tc39.es/ecma262/2022/multipage/ecmascript-language-statements-and-declarations.html#sec-block
BlockStatement[Yield, Await, Return] :
	Block[?Yield, ?Await, ?Return]
Block[Yield, Await, Return] :
	"{" StatementList[?Yield, ?Await, ?Return]opt "}"
StatementList[Yield, Await, Return] :
	StatementListItem[?Yield, ?Await, ?Return]
	StatementList[?Yield, ?Await, ?Return] StatementListItem[?Yield, ?Await, ?Return]
StatementListItem[Yield, Await, Return] :
	Statement[?Yield, ?Await, ?Return]
	Declaration[?Yield, ?Await]
	
// https://tc39.es/ecma262/2022/multipage/ecmascript-language-statements-and-declarations.html#sec-declarations-and-the-variable-statement
LexicalDeclaration[In, Yield, Await] :
	LetOrConst BindingList[?In, ?Yield, ?Await] ";"
LetOrConst :
	"let"
	"const"
BindingList[In, Yield, Await] :
	LexicalBinding[?In, ?Yield, ?Await]
	BindingList[?In, ?Yield, ?Await] "," LexicalBinding[?In, ?Yield, ?Await]
LexicalBinding[In, Yield, Await] :
	BindingIdentifier[?Yield, ?Await] Initializer[?In, ?Yield, ?Await]opt
	BindingPattern[?Yield, ?Await] Initializer[?In, ?Yield, ?Await]

// https://tc39.es/ecma262/2022/multipage/ecmascript-language-statements-and-declarations.html#sec-variable-statement
VariableStatement[Yield, Await] :
	"var" VariableDeclarationList[+In, ?Yield, ?Await] ";"
VariableDeclarationList[In, Yield, Await] :
	VariableDeclaration[?In, ?Yield, ?Await]
	VariableDeclarationList[?In, ?Yield, ?Await] "," VariableDeclaration[?In, ?Yield, ?Await]
VariableDeclaration[In, Yield, Await] :
	BindingIdentifier[?Yield, ?Await] Initializer[?In, ?Yield, ?Await]opt
	BindingPattern[?Yield, ?Await] Initializer[?In, ?Yield, ?Await]
	
// https://tc39.es/ecma262/2022/multipage/ecmascript-language-statements-and-declarations.html#sec-destructuring-binding-patterns
BindingPattern[Yield, Await] :
	ObjectBindingPattern[?Yield, ?Await]
	ArrayBindingPattern[?Yield, ?Await]
ObjectBindingPattern[Yield, Await] :
	"{" "}"
	"{" BindingRestProperty[?Yield, ?Await] "}"
	"{" BindingPropertyList[?Yield, ?Await] "}"
	"{" BindingPropertyList[?Yield, ?Await] "," BindingRestProperty[?Yield, ?Await]opt "}"
ArrayBindingPattern[Yield, Await] :
	"[" Elisionopt BindingRestElement[?Yield, ?Await]opt "]"
	"[" BindingElementList[?Yield, ?Await] "]"
	"[" BindingElementList[?Yield, ?Await] "," Elisionopt BindingRestElement[?Yield, ?Await]opt "]"
BindingRestProperty[Yield, Await] :
	"..." BindingIdentifier[?Yield, ?Await]
BindingPropertyList[Yield, Await] :
	BindingProperty[?Yield, ?Await]
	BindingPropertyList[?Yield, ?Await] "," BindingProperty[?Yield, ?Await]
BindingElementList[Yield, Await] :
	BindingElisionElement[?Yield, ?Await]
	BindingElementList[?Yield, ?Await] "," BindingElisionElement[?Yield, ?Await]
BindingElisionElement[Yield, Await] :
	Elisionopt BindingElement[?Yield, ?Await]
BindingProperty[Yield, Await] :
	SingleNameBinding[?Yield, ?Await]
	PropertyName[?Yield, ?Await] ":" BindingElement[?Yield, ?Await]
BindingElement[Yield, Await] :
	SingleNameBinding[?Yield, ?Await]
	BindingPattern[?Yield, ?Await] Initializer[+In, ?Yield, ?Await]opt
SingleNameBinding[Yield, Await] :
	BindingIdentifier[?Yield, ?Await] Initializer[+In, ?Yield, ?Await]opt
BindingRestElement[Yield, Await] :
	"..." BindingIdentifier[?Yield, ?Await]
	"..." BindingPattern[?Yield, ?Await]

// https://tc39.es/ecma262/2022/multipage/ecmascript-language-statements-and-declarations.html#sec-empty-statement	
EmptyStatement :
	";"

// https://tc39.es/ecma262/2022/multipage/ecmascript-language-statements-and-declarations.html#sec-expression-statement	
ExpressionStatement[Yield, Await] :
	${nextIsNot(
		oneOf(
			text("{"),
			text("function"),
			text(`async ${void 0 /* no LineTerminator */} function`),
			text("class"),
			text("let"),
			text("["),
		),
	)} Expression[+In, ?Yield, ?Await] ";"
	
// https://tc39.es/ecma262/2022/multipage/ecmascript-language-statements-and-declarations.html#sec-if-statement
IfStatement[Yield, Await, Return] :
	"if" "(" Expression[+In, ?Yield, ?Await] ")" Statement[?Yield, ?Await, ?Return] "else" Statement[?Yield, ?Await, ?Return]
	"if" "(" Expression[+In, ?Yield, ?Await] ")" Statement[?Yield, ?Await, ?Return] ${nextIsNot(
		text("else"),
	)}
	
// https://tc39.es/ecma262/2022/multipage/ecmascript-language-statements-and-declarations.html#sec-iteration-statements
IterationStatement[Yield, Await, Return] :
	DoWhileStatement[?Yield, ?Await, ?Return]
	WhileStatement[?Yield, ?Await, ?Return]
	ForStatement[?Yield, ?Await, ?Return]
	ForInOfStatement[?Yield, ?Await, ?Return]
	
// https://tc39.es/ecma262/2022/multipage/ecmascript-language-statements-and-declarations.html#sec-do-while-statement
DoWhileStatement[Yield, Await, Return] :
	"do" Statement[?Yield, ?Await, ?Return] "while" "(" Expression[+In, ?Yield, ?Await] ")" ";"

// https://tc39.es/ecma262/2022/multipage/ecmascript-language-statements-and-declarations.html#sec-while-statement
WhileStatement[Yield, Await, Return] :
	"while" "(" Expression[+In, ?Yield, ?Await] ")" Statement[?Yield, ?Await, ?Return]

// https://tc39.es/ecma262/2022/multipage/ecmascript-language-statements-and-declarations.html#sec-for-statement
ForStatement[Yield, Await, Return] :
	"for" "(" ${nextIsNot(text("let ["))} Expression[~In, ?Yield, ?Await]opt ";" Expression[+In, ?Yield, ?Await]opt ";" Expression[+In, ?Yield, ?Await]opt ")" Statement[?Yield, ?Await, ?Return]
	"for" "(" "var" VariableDeclarationList[~In, ?Yield, ?Await] ";" Expression[+In, ?Yield, ?Await]opt ";" Expression[+In, ?Yield, ?Await]opt ")" Statement[?Yield, ?Await, ?Return]
	"for" "(" LexicalDeclaration[~In, ?Yield, ?Await] Expression[+In, ?Yield, ?Await]opt ";" Expression[+In, ?Yield, ?Await]opt ")" Statement[?Yield, ?Await, ?Return]

// https://tc39.es/ecma262/2022/multipage/ecmascript-language-statements-and-declarations.html#sec-for-in-and-for-of-statements
ForInOfStatement[Yield, Await, Return] :
	"for" "(" ${nextIsNot(text("let ["))} LeftHandSideExpression[?Yield, ?Await] "in" Expression[+In, ?Yield, ?Await] ")" Statement[?Yield, ?Await, ?Return]
	"for" "(" "var" ForBinding[?Yield, ?Await] "in" Expression[+In, ?Yield, ?Await] ")" Statement[?Yield, ?Await, ?Return]
	"for" "(" ForDeclaration[?Yield, ?Await] "in" Expression[+In, ?Yield, ?Await] ")" Statement[?Yield, ?Await, ?Return]
	"for" "(" ${nextIsNot(oneOf(text("let"), text("async of")))} LeftHandSideExpression[?Yield, ?Await] "of" AssignmentExpression[+In, ?Yield, ?Await] ")" Statement[?Yield, ?Await, ?Return]
	"for" "(" "var" ForBinding[?Yield, ?Await] "of" AssignmentExpression[+In, ?Yield, ?Await] ")" Statement[?Yield, ?Await, ?Return]
	"for" "(" ForDeclaration[?Yield, ?Await] "of" AssignmentExpression[+In, ?Yield, ?Await] ")" Statement[?Yield, ?Await, ?Return]
	[+Await] "for" "await" "(" ${nextIsNot("let")} LeftHandSideExpression[?Yield, ?Await] "of" AssignmentExpression[+In, ?Yield, ?Await] ")" Statement[?Yield, ?Await, ?Return]
	[+Await] "for" "await" "(" "var" ForBinding[?Yield, ?Await] "of" AssignmentExpression[+In, ?Yield, ?Await] ")" Statement[?Yield, ?Await, ?Return]
	[+Await] "for" "await" "(" ForDeclaration[?Yield, ?Await] "of" AssignmentExpression[+In, ?Yield, ?Await] ")" Statement[?Yield, ?Await, ?Return]
ForDeclaration[Yield, Await] :
	LetOrConst ForBinding[?Yield, ?Await]
ForBinding[Yield, Await] :
	BindingIdentifier[?Yield, ?Await]
	BindingPattern[?Yield, ?Await]
	
// https://tc39.es/ecma262/2022/multipage/ecmascript-language-statements-and-declarations.html#sec-continue-statement
ContinueStatement[Yield, Await] :
	"continue" ";"
	"continue" ${nextIsNot("LineTerminator")} LabelIdentifier[?Yield, ?Await] ";"
	
// https://tc39.es/ecma262/2022/multipage/ecmascript-language-statements-and-declarations.html#sec-break-statement
BreakStatement[Yield, Await] :
	"break" ;
	"break" ${nextIsNot("LineTerminator")} LabelIdentifier[?Yield, ?Await] ";"

// https://tc39.es/ecma262/2022/multipage/ecmascript-language-statements-and-declarations.html#sec-return-statement	
ReturnStatement[Yield, Await] :
	"return" ";"
	"return" ${nextIsNot("LineTerminator")} Expression[+In, ?Yield, ?Await] ";"
	
// https://tc39.es/ecma262/2022/multipage/ecmascript-language-statements-and-declarations.html#sec-switch-statement
SwitchStatement[Yield, Await, Return] :
	"switch" "(" Expression[+In, ?Yield, ?Await] ")" CaseBlock[?Yield, ?Await, ?Return]
CaseBlock[Yield, Await, Return] :
	"{" CaseClauses[?Yield, ?Await, ?Return]opt "}"
	"{" CaseClauses[?Yield, ?Await, ?Return]opt DefaultClause[?Yield, ?Await, ?Return] CaseClauses[?Yield, ?Await, ?Return]opt "}"
CaseClauses[Yield, Await, Return] :
	CaseClause[?Yield, ?Await, ?Return]
	CaseClauses[?Yield, ?Await, ?Return] CaseClause[?Yield, ?Await, ?Return]
CaseClause[Yield, Await, Return] :
	"case" Expression[+In, ?Yield, ?Await] ":" StatementList[?Yield, ?Await, ?Return]opt
DefaultClause[Yield, Await, Return] :
	"default" ":" StatementList[?Yield, ?Await, ?Return]opt
	
// https://tc39.es/ecma262/2022/multipage/ecmascript-language-statements-and-declarations.html#sec-labelled-statements
LabelledStatement[Yield, Await, Return] :
	LabelIdentifier[?Yield, ?Await] ":" LabelledItem[?Yield, ?Await, ?Return]
LabelledItem[Yield, Await, Return] :
	Statement[?Yield, ?Await, ?Return]
	FunctionDeclaration[?Yield, ?Await, ~Default]
	
// https://tc39.es/ecma262/2022/multipage/ecmascript-language-statements-and-declarations.html#sec-throw-statement
ThrowStatement[Yield, Await] :
	"throw" ${nextIsNot("LineTerminator")} Expression[+In, ?Yield, ?Await] ";"
	
// https://tc39.es/ecma262/2022/multipage/ecmascript-language-statements-and-declarations.html#sec-try-statement
TryStatement[Yield, Await, Return] :
	"try" Block[?Yield, ?Await, ?Return] Catch[?Yield, ?Await, ?Return]
	"try" Block[?Yield, ?Await, ?Return] Finally[?Yield, ?Await, ?Return]
	"try" Block[?Yield, ?Await, ?Return] Catch[?Yield, ?Await, ?Return] Finally[?Yield, ?Await, ?Return]
Catch[Yield, Await, Return] :
	"catch" "(" CatchParameter[?Yield, ?Await] ")" Block[?Yield, ?Await, ?Return]
	"catch" Block[?Yield, ?Await, ?Return]
Finally[Yield, Await, Return] :
	"finally" Block[?Yield, ?Await, ?Return]
CatchParameter[Yield, Await] :
	BindingIdentifier[?Yield, ?Await]
	BindingPattern[?Yield, ?Await]
	
// https://tc39.es/ecma262/2022/multipage/ecmascript-language-statements-and-declarations.html#sec-debugger-statement
DebuggerStatement :
	"debugger" ";"
`;

// https://tc39.es/ecma262/2022/multipage/ecmascript-language-functions-and-classes.html#sec-ecmascript-language-functions-and-classes
const ECMAScriptSyntacticGrammarForFunctionAndClass = grammars`
// https://tc39.es/ecma262/2022/multipage/ecmascript-language-functions-and-classes.html#sec-parameter-lists
UniqueFormalParameters[Yield, Await] :
	FormalParameters[?Yield, ?Await]
FormalParameters[Yield, Await] :
	${any(0)}
	FunctionRestParameter[?Yield, ?Await]
	FormalParameterList[?Yield, ?Await]
	FormalParameterList[?Yield, ?Await] ","
	FormalParameterList[?Yield, ?Await] "," FunctionRestParameter[?Yield, ?Await]
FormalParameterList[Yield, Await] :
	FormalParameter[?Yield, ?Await]
	FormalParameterList[?Yield, ?Await] "," FormalParameter[?Yield, ?Await]
FunctionRestParameter[Yield, Await] :
	BindingRestElement[?Yield, ?Await]
FormalParameter[Yield, Await] :
	BindingElement[?Yield, ?Await]

// https://tc39.es/ecma262/2022/multipage/ecmascript-language-functions-and-classes.html#sec-function-definitions
FunctionDeclaration[Yield, Await, Default] :
	"function" BindingIdentifier[?Yield, ?Await] "(" FormalParameters[~Yield, ~Await] ")" "{" FunctionBody[~Yield, ~Await] "}"
	[+Default] "function" "(" FormalParameters[~Yield, ~Await] ")" "{" FunctionBody[~Yield, ~Await] "}"
FunctionExpression :
	"function" BindingIdentifier[~Yield, ~Await]opt "(" FormalParameters[~Yield, ~Await] ")" "{" FunctionBody[~Yield, ~Await] "}"
FunctionBody[Yield, Await] :
	FunctionStatementList[?Yield, ?Await]
FunctionStatementList[Yield, Await] :
	StatementList[?Yield, ?Await, +Return]opt

// https://tc39.es/ecma262/2022/multipage/ecmascript-language-functions-and-classes.html#sec-arrow-function-definitions
ArrowFunction[In, Yield, Await] :
	ArrowParameters[?Yield, ?Await] ${nextIsNot("LineTerminator")} "=>" ConciseBody[?In]
ArrowParameters[Yield, Await] :
	BindingIdentifier[?Yield, ?Await]
	CoverParenthesizedExpressionAndArrowParameterList[?Yield, ?Await]
ConciseBody[In] :
	${nextIsNot(text("{"))} ExpressionBody[?In, ~Await]
	"{" FunctionBody[~Yield, ~Await] "}"
ExpressionBody[In, Await] :
	AssignmentExpression[?In, ~Yield, ?Await]
	
// https://tc39.es/ecma262/2022/multipage/ecmascript-language-functions-and-classes.html#sec-method-definitions
MethodDefinition[Yield, Await] :
	ClassElementName[?Yield, ?Await] "(" UniqueFormalParameters[~Yield, ~Await] ")" "{" FunctionBody[~Yield, ~Await] "}"
	GeneratorMethod[?Yield, ?Await]
	AsyncMethod[?Yield, ?Await]
	AsyncGeneratorMethod[?Yield, ?Await]
	"get" ClassElementName[?Yield, ?Await] "(" ")" "{" FunctionBody[~Yield, ~Await] "}"
	"set" ClassElementName[?Yield, ?Await] "(" PropertySetParameterList ")" "{" FunctionBody[~Yield, ~Await] "}"
PropertySetParameterList :
	FormalParameter[~Yield, ~Await]
	
// https://tc39.es/ecma262/2022/multipage/ecmascript-language-functions-and-classes.html#sec-generator-function-definitions
GeneratorDeclaration[Yield, Await, Default] :
	"function" "*" BindingIdentifier[?Yield, ?Await] "(" FormalParameters[+Yield, ~Await] ")" "{" GeneratorBody "}"
	[+Default] "function" "*" "(" FormalParameters[+Yield, ~Await] ")" "{" GeneratorBody "}"
GeneratorExpression :
	"function" "*" BindingIdentifier[+Yield, ~Await]opt "(" FormalParameters[+Yield, ~Await] ")" "{" GeneratorBody "}"
GeneratorMethod[Yield, Await] :
	"*" ClassElementName[?Yield, ?Await] "(" UniqueFormalParameters[+Yield, ~Await] ")" "{" GeneratorBody "}"
GeneratorBody :
	FunctionBody[+Yield, ~Await]
YieldExpression[In, Await] :
	"yield"
	"yield" ${nextIsNot("LineTerminator")} AssignmentExpression[?In, +Yield, ?Await]
	"yield" ${nextIsNot("LineTerminator")} "*" AssignmentExpression[?In, +Yield, ?Await]

// https://tc39.es/ecma262/2022/multipage/ecmascript-language-functions-and-classes.html#sec-async-generator-function-definitions
AsyncGeneratorDeclaration[Yield, Await, Default] :
	"async" ${nextIsNot("LineTerminator")} "function" "*" BindingIdentifier[?Yield, ?Await] "(" FormalParameters[+Yield, +Await] ")" "{" AsyncGeneratorBody "}"
	[+Default] "async" ${nextIsNot("LineTerminator")} "function" "*" "(" FormalParameters[+Yield, +Await] ")" "{" AsyncGeneratorBody "}"
AsyncGeneratorExpression :
	"async" ${nextIsNot("LineTerminator")} "function" "*" BindingIdentifier[+Yield, +Await]opt "(" FormalParameters[+Yield, +Await] ")" "{" AsyncGeneratorBody "}"
AsyncGeneratorMethod[Yield, Await] :
	"async" ${nextIsNot("LineTerminator")} "*" ClassElementName[?Yield, ?Await] "(" UniqueFormalParameters[+Yield, +Await] ")" "{" AsyncGeneratorBody "}"
AsyncGeneratorBody :
	FunctionBody[+Yield, +Await]

// https://tc39.es/ecma262/2022/multipage/ecmascript-language-functions-and-classes.html#sec-class-definitions
ClassDeclaration[Yield, Await, Default] :
	"class" BindingIdentifier[?Yield, ?Await] ClassTail[?Yield, ?Await]
	[+Default] "class" ClassTail[?Yield, ?Await]
ClassExpression[Yield, Await] :
	"class" BindingIdentifier[?Yield, ?Await]opt ClassTail[?Yield, ?Await]
ClassTail[Yield, Await] :
	ClassHeritage[?Yield, ?Await]opt "{" ClassBody[?Yield, ?Await]opt "}"
ClassHeritage[Yield, Await] :
	"extends" LeftHandSideExpression[?Yield, ?Await]
ClassBody[Yield, Await] :
	ClassElementList[?Yield, ?Await]
ClassElementList[Yield, Await] :
	ClassElement[?Yield, ?Await]
	ClassElementList[?Yield, ?Await] ClassElement[?Yield, ?Await]
ClassElement[Yield, Await] :
	MethodDefinition[?Yield, ?Await]
	"static" MethodDefinition[?Yield, ?Await]
	FieldDefinition[?Yield, ?Await] ";"
	"static" FieldDefinition[?Yield, ?Await] ";"
	ClassStaticBlock
	";"
FieldDefinition[Yield, Await] :
	ClassElementName[?Yield, ?Await] Initializer[+In, ?Yield, ?Await]opt
ClassElementName[Yield, Await] :
	PropertyName[?Yield, ?Await]
	PrivateIdentifier
ClassStaticBlock :
	"static" "{" ClassStaticBlockBody "}"
ClassStaticBlockBody :
	ClassStaticBlockStatementList
ClassStaticBlockStatementList :
	StatementList[~Yield, +Await, ~Return]opt
	
// https://tc39.es/ecma262/2022/multipage/ecmascript-language-functions-and-classes.html#sec-async-function-definitions
AsyncFunctionDeclaration[Yield, Await, Default] :
	"async" ${nextIsNot("LineTerminator")} "function" BindingIdentifier[?Yield, ?Await] "(" FormalParameters[~Yield, +Await] ")" "{" AsyncFunctionBody "}"
	[+Default] "async" ${nextIsNot("LineTerminator")} "function" "(" FormalParameters[~Yield, +Await] ")" "{" AsyncFunctionBody "}"
AsyncFunctionExpression :
	"async" ${nextIsNot("LineTerminator")} "function" BindingIdentifier[~Yield, +Await]opt "(" FormalParameters[~Yield, +Await] ")" "{" AsyncFunctionBody "}"
AsyncMethod[Yield, Await] :
	"async" ${nextIsNot("LineTerminator")} ClassElementName[?Yield, ?Await] "(" UniqueFormalParameters[~Yield, +Await] ")" "{" AsyncFunctionBody "}"
AsyncFunctionBody :
	FunctionBody[~Yield, +Await]
AwaitExpression[Yield] :
	"await" UnaryExpression[?Yield, +Await]
	
// https://tc39.es/ecma262/2022/multipage/ecmascript-language-functions-and-classes.html#sec-async-arrow-function-definitions
AsyncArrowFunction[In, Yield, Await] :
	"async" ${nextIsNot(
		"LineTerminator",
	)} AsyncArrowBindingIdentifier[?Yield] ${nextIsNot("LineTerminator")} "=>" AsyncConciseBody[?In]
	CoverCallExpressionAndAsyncArrowHead[?Yield, ?Await] ${nextIsNot(
		"LineTerminator",
	)} "=>" AsyncConciseBody[?In]
AsyncConciseBody[In] :
	${nextIsNot("{")} ExpressionBody[?In, +Await]
	"{" AsyncFunctionBody "}"
AsyncArrowBindingIdentifier[Yield] :
	BindingIdentifier[?Yield, +Await]
CoverCallExpressionAndAsyncArrowHead[Yield, Await] :
	MemberExpression[?Yield, ?Await] Arguments[?Yield, ?Await]
`;

// https://tc39.es/ecma262/2022/multipage/ecmascript-language-functions-and-classes.html#sec-ecmascript-language-functions-and-classes
const ECMAScriptSyntacticGrammarForScriptAndModules = grammars`
// https://tc39.es/ecma262/2022/multipage/ecmascript-language-scripts-and-modules.html#sec-scripts
Script :
	ScriptBodyopt
ScriptBody :
	StatementList[~Yield, ~Await, ~Return]
	
// https://tc39.es/ecma262/2022/multipage/ecmascript-language-scripts-and-modules.html#sec-modules
Module :
	ModuleBodyopt
ModuleBody :
	ModuleItemList
ModuleItemList :
	ModuleItem
	ModuleItemList ModuleItem
ModuleItem :
	ImportDeclaration
	ExportDeclaration
	StatementListItem[~Yield, +Await, ~Return]
ModuleExportName :
	IdentifierName
	StringLiteral

// https://tc39.es/ecma262/2022/multipage/ecmascript-language-scripts-and-modules.html#sec-imports
ImportDeclaration :
	"import" ImportClause FromClause ;
	"import" ModuleSpecifier ;
ImportClause :
	ImportedDefaultBinding
	NameSpaceImport
	NamedImports
	ImportedDefaultBinding "," NameSpaceImport
	ImportedDefaultBinding "," NamedImports
ImportedDefaultBinding :
	ImportedBinding
NameSpaceImport :
	"*" "as" ImportedBinding
NamedImports :
	"{" "}"
	"{" ImportsList "}"
	"{" ImportsList "," "}"
FromClause :
	"from" ModuleSpecifier
ImportsList :
	ImportSpecifier
	ImportsList "," ImportSpecifier
ImportSpecifier :
	ImportedBinding
	ModuleExportName "as" ImportedBinding
ModuleSpecifier :
	StringLiteral
ImportedBinding :
	BindingIdentifier[~Yield, +Await]
	
// https://tc39.es/ecma262/2022/multipage/ecmascript-language-scripts-and-modules.html#sec-exports
ExportDeclaration :
	"export" ExportFromClause FromClause ";"
	"export" NamedExports ";"
	"export" VariableStatement[~Yield, +Await]
	"export" Declaration[~Yield, +Await]
	"export" "default" HoistableDeclaration[~Yield, +Await, +Default]
	"export" "default" ClassDeclaration[~Yield, +Await, +Default]
	"export" "default" ${nextIsNot(
		oneOf(
			text("function"),
			text(`async ${void 0 /* no LineTerminator */} function`),
			text("class"),
		),
	)} AssignmentExpression[+In, ~Yield, +Await] ";"
ExportFromClause :
	"*"
	"*" "as" ModuleExportName
	NamedExports
NamedExports :
	"{" "}"
	"{" ExportsList "}"
	"{" ExportsList "," "}"
ExportsList :
	ExportSpecifier
	ExportsList "," ExportSpecifier
ExportSpecifier :
	ModuleExportName
	ModuleExportName "as" ModuleExportName
`;

export const ECMAScriptSyntacticGrammar = [
	...ECMAScriptLexicalGrammar,
	...ECMAScriptSyntacticGrammarForExpression,
	...ECMAScriptSyntacticStatementsAndDeclarations,
	...ECMAScriptSyntacticGrammarForFunctionAndClass,
	...ECMAScriptSyntacticGrammarForScriptAndModules,
];
