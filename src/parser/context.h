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
#pragma once
#include "../parser/parser.h"
#include "../parser/Variable.h"
#include "../parser/Function.h"
#include "../parser/Type.h"


struct TranslateContext {
	std::string current_module;
	std::map < std::string, CommonBlockInfo *> commonblocks;
	std::map < std::string, ParseNode > labels;
	std::map < std::string, VariableInfo * > variables;
	std::map < std::string, FunctionInfo * > functions;
	std::map < std::string, TypeInfo* > types;
    std::vector <TypeInfo* > types_vec;
	std::map<std::string, std::vector<KeywordParamInfo>> func_kwargs;
	ParseNode program_tree;
	std::string global_code;
	ParseConfig parse_config;
	std::map<std::string, std::vector<std::string> > end_labels; // (lineno, blockname)
	bool inited;

	void reset_context();
};


TranslateContext & get_context();
void reset_parser();