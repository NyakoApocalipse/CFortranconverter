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
#include <regex>
// both function and array is callable

std::string get_mapped_function_name(std::string origin_name) {
	
	if (funcname_map.find(origin_name) == funcname_map.end()) {
		return origin_name;
	}
	else {
		/***********
		* some fortran intrinsic function NAME is different from
		*	its C++ implementation function NAME in for90std.h,
		*	in order to avoid possible head_name conflicts
		***********/
		return funcname_map.at(origin_name);
	}
}

void regen_function_array(FunctionInfo * finfo, ParseNode & callable) {

	// function call OR array index 
	// NOTE that array index can be A(1:2, 3:4) 
	ParseNode & callable_head = callable.get(0);
	ParseNode & argtable = callable.get(1);
	string head_name = get_mapped_function_name(callable_head.to_string());
	bool is_sysfunc = sysfunc_args.find(head_name) != sysfunc_args.end();
	if (argtable.token_equals(TokenMeta::NT_DIMENSLICE)) {
		// array section
		string array_str;
		bool is_slice = std::accumulate(argtable.begin(), argtable.end(), false, [](bool x, ParseNode * y) {
			return y->token_equals(TokenMeta::NT_SLICE) || x;
		});

		if (is_slice) {
			check_implicit_variable(finfo, head_name);
			// join all `slice.get_what()` with ","
			string slice_info_str = make_str_list(argtable.begin(), argtable.end(), [&](ParseNode * pslice) {
				ParseNode & slice = *pslice;
				regen_slice(finfo, slice);
				string res = "{" + slice.get_what() + "}";
				return res;
			});
            if(callable.father!= nullptr&&callable.father->child.size()==3&&callable.father->get(2).token_equals(TokenMeta::Let))
            {   /* if part of e.g., a(i,1:9) = (/one,zero,zero, zero,one,zero, zero,zero,one/)
                 * use assign_forslice to modify original array instead of copy
                 */
                sprintf(codegen_buf, "assign_forslice(%%s, %%s, {%s})", slice_info_str.c_str());
                callable.father->get(2).get_what() = string(codegen_buf);
                sprintf(codegen_buf, "%s", head_name.c_str());
            }
            else
			    sprintf(codegen_buf, "forslice(%s, {%s})", head_name.c_str(), slice_info_str.c_str());
			array_str = string(codegen_buf);
		}
		else {
			print_error("a dimen_slice with no slice(all scalar) is invalid", argtable);
		}
		callable.fs.CurrentTerm = Term{ TokenMeta::NT_FUCNTIONARRAY,  array_str };
	}
	else if(argtable.token_equals(TokenMeta::NT_ARGTABLE_PURE, TokenMeta::NT_PARAMTABLE_PURE) ){
		// function call(with or without kwargs) OR array section
		const std::regex pointer_regex("^\\(\\*\\(.*\\)\\)$");
		if (is_fortran_function(finfo, head_name) || callable_head.token_equals(TokenMeta::NT_DERIVED_TYPE) || std::regex_match(callable_head.get_what(), pointer_regex))
		{

		}
		else {
            /*TODO: Finer granularity to decide whether function was defined in a module of other file.
             * Need information cross files and/or conversions.
             * Current policy is loose, i.e., if `use *` is present, then function of whatever name is
             * regarded presented/defined somewhere else.
             * The sequela is that `check_implicit_variable(finfo, head_name);` here
             * will be called only if no `use *` statement
             * is encountered.
             *
             * For CALL variable | CALL functionarray
             * CALL is appended on tail, the node NT_FUNCTIONARRAY
             * will be larger by one in child size */
            if(finfo->use_stmts.empty()&&callable.child.size()<3)
            {
                // variable
                check_implicit_variable(finfo, head_name);
            }else if(callable.child.size()>3){
              finfo->use_stmts.push_back(callable.child.at(3)); /* add #include "*.h" */
			}
		}
		string argtable_str;
		/**************
		* If a function have keyword parameters, it should logged in `get_context().func_kwargs`
		* `get_context().func_kwargs` records every keyword paramters' information
		***************/
		auto map_func = get_context().func_kwargs.find(head_name); 

		/**************
		* valid_kwargs is true iif any keyword arguments 
		* come before normal arguments
		***************/
		bool valid_kwargs_test = false;
		/**************
		* number of non-kwarg parameters
		***************/
		int normal_count = 0; 
		map<string, string> kw_args;
		vector<string> normal_args;
		for (int i = 0; i < argtable.length(); i++)
		{
			ParseNode & elem = argtable.get(i);
			if (elem.token_equals(TokenMeta::NT_KEYVALUE)) {
				// MAYBE kwarg, MAYBE normal arg
				if (elem.get(1).token_equals(TokenMeta::NT_VARIABLEINITIALDUMMY))
				{
					// normal argument
					regen_exp(finfo, elem.get(0));
					normal_args.push_back(elem.get(0).get_what());
					normal_count++;
				}
				else {
					// kwarg
					regen_exp(finfo, elem.get(1));
					if (map_func == get_context().func_kwargs.end()) {
						print_error("Function " + head_name + " don't have keyword paramters: " , argtable);
					}
					else {
						string argname = elem.get(0).to_string();
						string argvalue = elem.get(1).to_string();
						kw_args[argname] = argvalue;
					}
					valid_kwargs_test = true;
				}
			}
			else {
				// normal argument
				regen_exp(finfo, elem);
				normal_args.push_back(elem.get_what());

				// generated code from normal_args
				if (valid_kwargs_test) {
					fatal_error("Keyword arguments must come after normal arguments", argtable);
				}
				else {
					normal_count++;
				}
			}
		}

		argtable_str += make_str_list(normal_args.begin(), normal_args.end(), [&](string p) {
			// origin: a, b, 1
			// TODO: this is a temporary solution
			// fortran: INOUT(a), INOUT(b), INOUT(1)
			if (is_sysfunc)
			{
				return p;
			}
			else {
				sprintf(codegen_buf, "INOUT(%s)", p.c_str());
				return string(codegen_buf);
			}
		});
		/**************
		*	if exist kwargs, must add `,` delimer between arguments
		*	if not exist, mustn't add `,`, use `)` enclose whole argument list directly
		***************/
		string tail = argtable_str.empty()?"":", ";
		// generated code of kwargs
		if (map_func != get_context().func_kwargs.end()) {
			std::vector<KeywordParamInfo> & params = map_func->second;
			if (normal_count <= params.size())
			{
				// or `params.begin() + normal_count` will overflow
				std::string kwargs_str = make_str_list(params.begin() + normal_count, params.end(), [&](KeywordParamInfo & kwparam_info) {
					string this_param_name = std::get<0>(kwparam_info);
					string this_param_type = std::get<1>(kwparam_info);
					string this_param_initial_default = std::get<2>(kwparam_info);
					map<string, string>::iterator this_arg = kw_args.find(this_param_name);
					std::string s;
					if (this_arg == kw_args.end()) {
						// if value of this keyword argument is not present
						if (this_param_initial_default == "") {
							s = "None";
						}
						else if (this_param_initial_default[0] == '\"') {
							// BUGFIX: const char * to string
							sprintf(codegen_buf, "SS(%s)", this_param_initial_default.c_str());
							s = string(codegen_buf);
						}
						else {
							// use initial defined in get_context().func_kwargs
							s = this_param_initial_default;
						}
					}
					else {
						// if value of this keyword argument is present
						if (this_param_type == "mask_wrapper_t") {
							// fortran's mask is now implemented by boolean array, not lambda functions
							s = this_arg->second;
						}
						else {
							s = this_arg->second;
						}
					}
					string tailed = tail + s;
					tail = "";
					return tailed;
				});
				argtable_str += kwargs_str;
			}
		}
		sprintf(codegen_buf, "%s(%s)", head_name.c_str(), argtable_str.c_str());
		callable.fs.CurrentTerm = Term{ TokenMeta::NT_FUCNTIONARRAY, string(codegen_buf) };
	}
	else {
		print_error("Callable generation failed", callable);
	}
	return;
}

