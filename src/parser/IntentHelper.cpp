/*
*   Calvin Neo
*   Copyright (C) 2016  Calvin Neo <calvinneo@calvinneo.com>
*
*   This program is free software; you can redistribute it and/or modify
*   it under the terms of the GNU General Public License as published by
*   the Free Software Foundation; either version 2 of the License, or
*   (at your option) any later version.
*
*   This program is distributed in the hope that it will be useful,
*   but WITHOUT ANY WARRANTY; without even the implied warranty of
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*   GNU General Public License for more details.
*
*   You should have received a copy of the GNU General Public License along
*   with this program; if not, write to the Free Software Foundation, Inc.,
*   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
*/

/*****************************************************************************
*	THIS FILE IS DEPRECATED
*****************************************************************************/

#include "IntentHelper.h"
#include "Intent.h"
#include "tokenizer.h"
/*
([\w-]+) *= *([+-\d]+),
, {$2, "$1"}
*/

std::map<TokenMeta_T, std::string> IntentName = {
	{ 260, "Dereference" }
	,{ 261, "Reference" }

	,{ 262, "TypeCast" }

	,{ 240, "PostInc" }
	,{ 241, "PostDec" }
	,{ 242, "Inc" }
	,{ 243, "Dec" }

	,{ 230, "Not" }
	,{ 231, "Neg" }
	,{ 232, "PreInc" }
	,{ 233, "PreDec" }
	,{ 234, "ShortWave" }

	,{ 200, "Power" }

	,{ 190, "Multiply" }
	,{ 192, "MultiplyVirtual" }
	,{ 191, "Divide" }
	,{ 193, "Mod" }

	,{ 180, "Add" }
	,{ 181, "Minus" }
	,{ 182, "MinusVirtual" }

	,{ 170, "Shl" }
	,{ 171, "Shr" }

	,{ 460, "PLET" }
	,{ 160, "GT" }
	,{ 161, "GE" }
	,{ 162, "EQ" }
	,{ 163, "LE" }
	,{ 164, "LT" }
	,{ 165, "NEQ" }
	,{ 166, "Compare" }
	,{ 167, "In" }
	,{ 168, "EQV" }
	,{ 169, "NEQV" }

	,{ 150, "And" }
	,{ 151, "AndVirtual" }

	,{ 140, "Or" }

	,{ 130, "AndAnd" }
	,{ 120, "OrOr" }

	,{ 101, "Let" }
	,{ 102, "LetAdd" }
	,{ 103, "LetMinus" }
	,{ 104, "LetMultiply" }
	,{ 105, "LetDivide" }
	,{ 106, "LetShl" }
	,{ 107, "LetShr" }
	,{ 108, "LetAnd" }
	,{ 109, "LetOr" }

	,{ 0, "Nop" }

	,{ -1, "LB" }
	,{ -2, "RB" }
	,{ -3, "SLB" }
	,{ -4, "SRB" }
	,{ -5, "Dot" }
	,{ -6, "DoubleColon" }
	,{ -7, "Comma" }
	,{ -8, "Colon" }
	,{ -9, "LBrace" }
	,{ -10, "RBrace" }
	,{ -11, "Semicolon" }

	,{ -101, "Null" }
	,{ -102, "True" }
	,{ -103, "False" }

	,{ -110, "OperatorCall" }
	,{ -111, "CallArgs" }
	,{ -112, "HyperFuncCall" }
	,{ -113, "OptionalArg" }
	,{ -114, "Lambda" }
	,{ -115, "Operator" }
	,{ -116, "Return" }
	,{ -117, "SetReturnValue" }

	,{ -120, "PushPointer" }
	,{ -121, "PushVar" }
	,{ -122, "PushLiteralConst" }
	,{ -123, "PushExtern" }
	,{ -124, "SystemFunction" }
	,{ -125, "DynamicVariable" }
	,{ -126, "UnknownVariant" }

	,{ -131, "New" }
	,{ -132, "Delete" }
	,{ -135, "Is" }
	,{ -136, "Construct" }
	,{ -137, "TypeDef" }
	,{ -138, "Define" }
	,{ -139, "Using" }
	,{ -140, "Duplicate" }

	,{ -150, "Stop" }
	, {-151, "Format"}

	,{ -201, "Virtual" }
	,{ -202, "Const" }
	,{ -203, "Static" }
	,{ -204, "Public" }
	,{ -205, "Private" }
	,{ -206, "Protected" }
	,{ -207, "Friend" }
	,{ -208, "Class" }
	,{ -209, "NameSpace" }
	,{ -211, "Extern" }
	,{ -212, "Partial" }
	,{ -213, "Final" }

	,{ -216, "Enum" }
	,{ -217, "Abstract" }

	,{ -301, "META_WORD" }
	,{ -302, "META_INTEGER or Int" }
	,{ -303, "META_STRING or String" }
	,{ -304, "META_CHARACTER or Char" }
	,{ -305, "META_REAL or Double" }
	,{ -306, "META_OPERATOR" }
	,{ -307, "META_ILLEGAL" }
	,{ -308, "META_ANY" }
	,{ -309, "META_REQ_MORE" }
	,{ -310, "META_NONTERMINAL" }
	,{ -311, "META_COMPLEX" }
	,{ -312, "Void or META_VOID" }

	,{ -321, "Obj" }
	,{ -326, "Dynamic" }
	,{ -327, "Bool" }
	,{ -328, "Pointer" }
	, {-329, "Float"}
	,{ -330, "Long" }
	,{ -331, "Complex" }
	,{ -332, "Short" }
	, { -333, "Function" }
		, { -334, "Int8" }
		, { -335, "Int16" }
		, { -336, "Int32" }
		, { -337, "Int64" }
		, {-338, "LongDouble"}

	,{ TokenMeta::Void - 100, "Void_Decl" }
	,{ TokenMeta::Obj - 100, "Obj_Decl" }
	,{ TokenMeta::Int - 100, "Int_Decl" }
	,{ TokenMeta::Char - 100, "Char_Decl" }
	,{ TokenMeta::String - 100, "String_Decl" }
	,{ TokenMeta::Double - 100, "Double_Decl" }
	//,{ TokenMeta::Dynamic - 100, "Dynamic_Decl" }
	,{ TokenMeta::Bool - 100, "Bool_Decl" }
	//,{ TokenMeta::Pointer - 100, "Pointer_Decl" }
	,{ TokenMeta::Float - 100, "Float_Decl" }
	//,{ TokenMeta::Long - 100, "Long_Decl" }
	,{ TokenMeta::Complex - 100, "Complex_Decl" }
	//,{ TokenMeta::Short - 100, "Short_Decl" }
	, { TokenMeta::Function - 100, "Function_def" }
	, { TokenMeta::Int8 - 100, "Int8_Decl" }
	, { TokenMeta::Int16 - 100, "Int16_Decl" }
	, { TokenMeta::Int32 - 100, "Int32_Decl" }
	, { TokenMeta::Int64 - 100, "Int64_Decl" }
	, { TokenMeta::LongDouble - 100, "LongDouble_Decl" }
	, { -599, "Implicit_Decl" }

	,{ -600, "If" }
	,{ -601, "Else or ElseIf" }
	,{ -602, "ElseLast" }
	,{ -603, "While" }
	,{ -604, "For" }
	,{ -605, "Goto" }
	,{ -606, "Continue" }
	,{ -607, "Break" }
	,{ -608, "JumpTrue" }
	,{ -609, "JumpFalse" }
	,{ -610, "JumpNoJudge" }
	,{ -611, "Try" }
	,{ -612, "Catch" }
	,{ -613, "Finally" }
	,{ -614, "Do" }
	,{ -615, "Switch" }
	,{ -616, "Case" }
	,{ -617, "Default" }

	,{ -1000, "CRLF" }
	,{ -1003, "Label" }
	,{ -1004, "Sharp" }
	,{ -1005, "EndOfScan" }
	,{ -1006, "QuitProgram" }
	,{ -1007, "Error" }
	,{ -1008, "Comments" }
	,{ -1009, "CommentsEnd" }
	,{ -1010, "CommentLine" }
	,{ -1011, "Include" }

	,{ -2001, "NT_PARAMTABLE" }
	,{ -2002, "NT_FORMATTER" }
	,{ -2003, "NT_AUTOFORMATTER" }
	,{ -2004, "NT_ARGTABLE" }
	,{ -2005, "NT_FUNCTIONDECLARE" }
	,{ -2006, "NT_VARIABLEDEFINE" }
	,{ -2007, "NT_STATEMENT" }
	,{ -2008, "NT_EXPRESSION" }
	,{ -2009, "NT_IF" }
	,{ -2010, "NT_ELSEIF" }
	,{ -2011, "NT_DO" }
	,{ -2012, "NT_VARIABLE_ENTITY or NT_KEYVALUE" }
	,{ -2013, "NT_DECLAREDVARIABLE" }

	,{ -2014, "NT_SUITE" }
	,{ -2015, "NT_FUCNTIONARRAY" }
	,{ -2016, "NT_ARRAYBUILDER_LAMBDA" }
	,{ -2017, "NT_ARRAYBUILDER_LIST" }
	,{ -2018, "NT_DIMENSLICE" }
	,{ -2019, "NT_PARAMTABLE_DIMENSLICE" }
	,{ -2020, "NT_SLICE" }
	,{ -2021, "NT_VARIABLEDESC" }
	,{ -2022, "NT_VARIABLEINITIALDUMMY" }

	,{ -2023, "NT_VOID" }
	,{ -2024, "NT_ARRAYBUILDER" }
	,{ -2025, "NT_ARGTABLE_PURE" }
	,{ -2026, "NT_WRAPPER" }
	,{ -2027, "NT_SELECT" }
	,{ -2028, "NT_CASE" }
	,{ -2029, "NT_CASES" }
	,{ -2030, "NT_HIDDENDO" }
	,{ -2031, "NT_WHILE"}
	,{ -2032, "NT_OPEN" }
	,{ -2033, "NT_CLOSE" }
	, {-2034, "NT_PROGRAM"}
	, {-2035, "NT_INTERFACE"}
	, {-2036, "NT_WRAPPERS"}
	, {-2037, "NT_PARAMTABLE_PURE"}
	, {-2038, "NT_FORMAT"}
	, {-2039, "NT_LABEL_STMT"}
	, {-2040, "NT_PROGRAM_EXPLICIT"}
	, {-2041, "NT_COMMONBLOCK"}
	, {-2042, "NT_COMMONBLOCKDEFINE"}
	, {-2043, "NT_VARIABLEDEFINESET"}
	, {-2044, "NT_FORMATTER_LOCATION"}
	, { -2045, "NT_READ_STMT" }
	, { -2046, "NT_WRITE_STMT" }
	, { -2047, "NT_PRINT_STMT" }
	, { -2048, "NT_DORANGE" }
	, { -2049, "NT_CONTROL_STMT"}
	,{ -9999, "NT_DUMMY"}
};


std::string get_intent_name(TokenMeta_T intent_id) {

	if (IntentName.find(intent_id) != IntentName.end()) {
		return IntentName[intent_id];
	}
	else {
		std::string res = TokenMeta::get_enum_table().from_value(intent_id);
		if (res.empty())
			return "!undefined!";
		else
			return res;
	}
}