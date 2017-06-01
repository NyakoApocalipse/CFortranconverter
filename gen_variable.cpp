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
#include <map>
#include <boost/range/adaptors.hpp>
#include <boost/range/algorithm.hpp>
#include <boost/range/irange.hpp>

//5.5.2 COMMON statement
//The COMMON statement specifies blocks of physical storage, called common blocks, that may be accessed by
//any of the scoping units in an executable program.Thus, the COMMON statement provides a global data facility
//based on storage association(14.6.3).The common blocks specified by the COMMON statement may be named
//and are called named common blocks, or may be unnamed and are called blank common.

ParseNode gen_common(ARG_IN commonname_node, ARG_IN paramtable) {
	// the unique blank COMMON block must be declared in the main program.
	ParseNode kvparamtable = promote_argtable_to_paramtable(paramtable);
	string common_name = commonname_node.get_what();
	auto common_info = get_context().commonblocks.find(common_name);
	if (common_info == get_context().commonblocks.end()) {
		// add common block to context
		get_context().commonblocks[common_name] = CommonBlockInfo{ common_name, std::vector<VariableInfo>() };
		common_info = get_context().commonblocks.find(common_name);
	}
	ParseNode newnode = gen_token(Term{ TokenMeta::NT_COMMONBLOCK, "" });
	newnode.addlist(commonname_node, kvparamtable);
	return newnode;
}

void regen_common(FunctionInfo * finfo, ARG_OUT common_block) {
	ParseNode & commonname_node = common_block.get(0);
	ParseNode & kvparamtable = common_block.get(1);
	string common_name = commonname_node.get_what();
	auto common_info = get_context().commonblocks.find(common_name);
	if (common_info == get_context().commonblocks.end())
	{
		print_error("Error Common Block");
	}
	bool new_common = (common_info->second.variables.size() == 0);
	for (int i = 0; i < kvparamtable.length(); i++)
	{
		ParseNode & entity_variable = kvparamtable.get(i);
		std::string local_varname = get_variable_name(entity_variable);
		VariableInfo * local_vinfo = get_variable(get_context().current_module, finfo->local_name, local_varname);
		
		local_vinfo = check_implicit_variable(finfo, local_varname);
		local_vinfo->entity_variable = kvparamtable.get(i);; 
		local_vinfo->commonblock_index = i; 
		local_vinfo->commonblock_name = common_name; 

		if (new_common)
		{
			VariableInfo global_vinfo(*local_vinfo);
			common_info->second.variables.push_back(global_vinfo);
		}
		local_vinfo->desc.constant = false;
		local_vinfo->desc.reference = true;
	}
	// code are actually generated in gen_stmt_suite
	// if the variable is not declared by statement other than `COMMON`, it will be generated by forall_variable_in_function
}

ParseNode gen_common_definition(std::string common_name) {
	auto common_info = get_context().commonblocks.find(common_name);
	if (common_info == get_context().commonblocks.end()) {
		print_error("Common block without definition");
	}
	int i = 0;
	string struct_str = make_str_list(common_info->second.variables.begin(), common_info->second.variables.end(), [&](VariableInfo & x) {
		std::string common_varname = "_" + to_string(++i);
		sprintf(codegen_buf, "%s %s;", gen_qualified_typestr(x.type, x.desc).c_str(), common_varname.c_str());
		return string(codegen_buf);
	}, "\n");
	if (common_name == "")
	{
		sprintf(codegen_buf, "struct{\n%s\n}G;\n", tabber(struct_str).c_str());
	}
	else {
		sprintf(codegen_buf, "struct{\n%s\n}%s;\n", tabber(struct_str).c_str(), common_name.c_str());
	}
	return gen_token(Term{ TokenMeta::NT_COMMONBLOCKDEFINE, string(codegen_buf) });
}


VariableInfo * check_implicit_variable(FunctionInfo * finfo, const std::string & name) {
	/******************
	*	this function must be called AFTER the AST is finished construct, by `regen_exp` function
	*	for all variables in function body that not appear in `finfo->desc->declared_variables`, 
	*	mark them as implicit variables and generate its definition
	* NOTICE:
	* this function only works on `exp` nodes, not including variable declaration(`vardef` nodes), etc.
	*******************/
	VariableInfo * vinfo = get_variable(get_context().current_module, finfo->local_name, name);
	if (vinfo == nullptr)
	{
		vinfo = add_variable(get_context().current_module, finfo->local_name, name, VariableInfo{});
		ParseNode implicit_type = gen_implicit_type(name);
		vinfo->commonblock_index = 0; // set in regen_suite and gen_common
		vinfo->commonblock_name = ""; // set in regen_suite and gen_common
		vinfo->desc = VariableDesc(); // set in regen_vardef
		vinfo->implicit_defined = true; // set in regen_suite and regen_common
		vinfo->type = implicit_type; // set in regen_vardef
		vinfo->entity_variable = gen_keyvalue_from_name(name); // set in regen_vardef
		ParseNode vardef_node = gen_vardef_from_default(implicit_type, name);
		vinfo->vardef_node = new ParseNode(vardef_node); // set in regen_suite and gen_common
	}
	else if (vinfo->type.get_token() == TokenMeta::Implicit_Decl)
	{
		vinfo->type = gen_implicit_type(name); // set in regen_vardef
	}
	return vinfo;
}