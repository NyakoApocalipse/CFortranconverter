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

#include "gen_common.h"

void regen_select(FunctionInfo * finfo, ParseNode & select_stmt) {
	ParseNode & exp = select_stmt.get(0);
	ParseNode & case_stmt = select_stmt.get(1); 
	select_stmt.fs.CurrentTerm = Term{ TokenMeta::NT_SELECT, "" };
	string case_default;
	bool first_if = true;
	for (ParseNode * item : case_stmt)
	{
		ParseNode & case_stmt_elem = *item;
		ParseNode & dimen_slice = case_stmt_elem.get(0);
		ParseNode & body = case_stmt_elem.get(1);
		regen_suite(finfo, body, true);

		if (dimen_slice.token_equals(TokenMeta::NT_DUMMY))
		{
			sprintf(codegen_buf, "else {\n%s}\n", tabber(body.to_string()).c_str());
			case_default = string(codegen_buf);
		}
		else {
			string conditions;
			if (dimen_slice.token_equals(TokenMeta::NT_DIMENSLICE)) {
				// NT_DIMENSLICE
				conditions = make_str_list(dimen_slice.begin(), dimen_slice.end(), [&](ParseNode * px) {
					ParseNode & x = *px;
					ParseNode & from = x.get(0);
					ParseNode & to = x.get(1);
					sprintf(codegen_buf, "(%s >= %s && %s < %s)", exp.to_string().c_str(), from.to_string().c_str(), exp.to_string().c_str(), to.to_string().c_str());
					return string(codegen_buf);
				}, "||");
			}
			else if (dimen_slice.token_equals(TokenMeta::NT_ARGTABLE_PURE)) {
				// NT_ARGTABLE_PURE
				conditions = make_str_list(dimen_slice.begin(), dimen_slice.end(), [&](ParseNode * px) {
					ParseNode & x = *px;
					sprintf(codegen_buf, "%s == %s", exp.to_string().c_str(), x.to_string().c_str());
					return string(codegen_buf);
				}, "||");
			}
			if (first_if) {
				sprintf(codegen_buf, "if(%s){\n%s}\n", conditions.c_str(), tabber(body.to_string()).c_str());
				first_if = false;
			}
			else {
				sprintf(codegen_buf, "else if(%s){\n%s}\n", conditions.c_str(), tabber(body.to_string()).c_str());
			}
		}
		select_stmt.get_what() += string(codegen_buf);
	}
}