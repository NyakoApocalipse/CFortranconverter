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

void regen_do(FunctionInfo * finfo, ParseNode & do_stmt) {
	ParseNode & suite = do_stmt.get(0);
    ParseNode & label = do_stmt.get(1);
	regen_suite(finfo, suite, true);
    string label_line = "\n"+label.get_what()+(label.get_what().empty()?string(""):string(":"))+"nop();";
	sprintf(codegen_buf, "while(true){\n%s}", tabber(suite.get_what()).c_str());
    sprintf(codegen_buf,"%s%s",codegen_buf,label_line.c_str());
	do_stmt.fs.CurrentTerm = Term{ TokenMeta::NT_DO, string(codegen_buf) };
}

void regen_do_range(FunctionInfo * finfo, ParseNode & do_stmt){
	ParseNode & loop_variable = do_stmt.get(0);
	ParseNode & exp1 = do_stmt.get(1);
	ParseNode & exp2 = do_stmt.get(2);
	ParseNode & exp3 = do_stmt.get(3);
	ParseNode & suite = do_stmt.get(4);
    ParseNode & label = do_stmt.get(5);
	regen_exp(finfo, exp1);
	regen_exp(finfo, exp2);
	regen_exp(finfo, exp3);
	regen_suite(finfo, suite, true);
    string label_line = "\n"+label.get_what()+(label.get_what().empty()?string(""):string(":"))+"nop();";
	sprintf(codegen_buf, "for(%s = %s; %s <= %s; %s += %s){\n%s}"
		, loop_variable.get_what().c_str(), exp1.get_what().c_str()
		, loop_variable.get_what().c_str(), exp2.get_what().c_str()
		, loop_variable.get_what().c_str(), exp3.get_what().c_str(), tabber(suite.get_what()).c_str());
    sprintf(codegen_buf,"%s%s",codegen_buf,label_line.c_str());
	do_stmt.fs.CurrentTerm = Term{ TokenMeta::NT_DORANGE, string(codegen_buf) };
}

void regen_do_while(FunctionInfo * finfo, ParseNode & do_stmt) {
	ParseNode & exp = do_stmt.get(0);
	ParseNode & suite = do_stmt.get(1);
    ParseNode & label = do_stmt.get(2);
	regen_exp(finfo, exp);
	regen_suite(finfo, suite, true);
    string label_line = "\n"+label.get_what()+(label.get_what().empty()?string(""):string(":"))+"nop();";
	sprintf(codegen_buf, "while(%s){\n%s}", exp.get_what().c_str(), tabber(suite.get_what()).c_str());
    sprintf(codegen_buf,"%s%s",codegen_buf,label_line.c_str());
	do_stmt.fs.CurrentTerm = Term{ TokenMeta::NT_WHILE, string(codegen_buf) };
}


std::vector<ParseNode *> get_nested_hiddendo_layers(ParseNode & hiddendo) {
	std::vector<ParseNode *> hiddendo_layer;
	ParseNode * pn = &hiddendo;
	while (pn->token_equals(TokenMeta::NT_HIDDENDO)) {
		hiddendo_layer.push_back(pn);
		if (pn->length() > 0 
			&& pn->get(0).length() > 0 // NT_ARGTABLE_PURE
			&& pn->get(0).get(0).length() > 0 // NT_HIDDENDO
			) {
			pn = &pn->get(0).get(0);
		}
		else {
			break;
		}
	} 
	return hiddendo_layer;
}

std::vector<ParseNode *> get_parent_hiddendo_layers(ParseNode & hiddendo) {
	std::vector<ParseNode *> hiddendo_layer;
	ParseNode * pn = &hiddendo;
	while (pn->token_equals(TokenMeta::NT_HIDDENDO)) {
		hiddendo_layer.push_back(pn);
		if (pn->father != nullptr // NT_ARGTABLE_PURE
			&& pn->father->father != nullptr // NT_HIDDENDO
			&& pn->father->father->token_equals(TokenMeta::NT_HIDDENDO) ) {
			pn = pn->father->father;
		}
		else {
			break;
		}
	}
	return hiddendo_layer;
}

SliceBoundInfo get_lbound_size_from_hiddendo(FunctionInfo * finfo, ParseNode & hiddendo, std::vector<ParseNode *> hiddendo_layer) {
	SliceBoundInfo lb_size = get_sliceinfo_base(hiddendo_layer.begin(), hiddendo_layer.end()
		, [&](ParseNode * x) {
			regen_exp(finfo, x->get(2));
			return x->get(2).get_what();
		} , [&](ParseNode * x) {
			regen_exp(finfo, x->get(3));
			return x->get(3).get_what() + "+1-" + x->get(2).get_what();
	});
	return lb_size;
}

SliceBoundInfo get_lbound_ubound_from_hiddendo(FunctionInfo * finfo, ParseNode & hiddendo, std::vector<ParseNode *> hiddendo_layer) {
	SliceBoundInfo lb_ub = get_sliceinfo_base(hiddendo_layer.begin(), hiddendo_layer.end()
		, [&](ParseNode * x) {
			regen_exp(finfo, x->get(2));
			return x->get(2).get_what();
		} , [&](ParseNode * x) {
			regen_exp(finfo, x->get(3));
			return x->get(3).get_what();
	});
	return lb_ub;
}

void regen_hiddendo_expr(FunctionInfo * finfo, ParseNode & hiddendo, std::function<void(ParseNode &)> regen_innermost_argtable) {
	/**************************************
	* this function flatten a n-layer nested hidden do into an lambda function,
	*	this function use a 1-dimension size-n array of `fsize_t`,
	*	which stands for index of every n-dimensions.
	* this function is used to deal with io-implied-do, ac-implied-do, data-implied-do.
	* Usage:
	* 1) io-implied-do
	* 2) ac-implied-do
	*	passed to `make_init_list` its 3rd argument
	*======================================
	*	A a0; B b0; C c0;
	*	auto result = [](const fsize_t * current) {
	*		return [](A a, B b, C c){ 
	*			// actual ac/data/io-implied-do-control codes
	*		}(current[0], current[1], ...);
	*	}
	*	({a0, b0, c0}) // call with args
	***************************************/
	if (get_context().parse_config.usefarray)
	{
		std::vector<ParseNode *> hiddendo_layers = get_nested_hiddendo_layers(hiddendo);

		/**************************************
		* generate indexer for each dimension(nested implied-do layers)
		*======================================
		*	e.g.
		*	```
		*	((F(I,J),J=1,2),I=3,4)
		*	```
		*	the indexer is
		*	`fsize_t i, fsize_t j`
		***************************************/
		string indexer_str = make_str_list(hiddendo_layers.begin(), hiddendo_layers.end(), [](auto x)->string {
			return "fsize_t " + (x)->get(1).get_what();
		});

		/**************************************
		* IMPORTANT
		*	refer rule `hidden_do` and found NT_HIDDENDO have EXACTLY **4** children
		*	the first child is argtable(because a hidden_do can contain several exps)
		* can't handle following code
		*======================================
		*	WRITE(2,*) (A(I),(B(I,J),J=1,i),I=2,10)
		***************************************/
		ParseNode & innermost_argtable = hiddendo_layers.back()->get(0);
		regen_innermost_argtable(innermost_argtable);
		string innermost_code = innermost_argtable.get_what();
		/**************************************
		* generate the following lambda
		*======================================
		*```
		*	return [](fsize_t a, fsize_t b, fsize_t c){
		*		// actual codes
		*	}(current[0], current[1], ...);
		*```
		*======================================
		* `current` is a `fsize_t *` array.
		* length of `current` = dimension
		* count of parameter of this lambda = dimension
		*======================================
		* usage of this lambda is to
		* feed current to each parameters:
		* current[0] -> a
		* current[1] -> b
		* current[2] -> c
		* so actual codes can directly use names, not a `fsize_t *` array
		*======================================
		* WHY NEED THIS WORKAROUND?
		* because we need a iterative function, not a deep for-loop, 
		* in order to make implied-do a expression rather than a structure,
		* to satisfy fortran's feature
		***************************************/
		sprintf(codegen_buf, "[&](%s){\n%s}", indexer_str.c_str(), tabber(innermost_code).c_str());
		string lambda_def = string(codegen_buf);
		int j = 0;
		string lambda_args = make_str_list(hiddendo_layers.begin(), hiddendo_layers.end(), [&](auto x) {
			sprintf(codegen_buf, "current[%d]", j++);
			return string(codegen_buf);
		}, ", ");

		// map array to parameter
		sprintf(codegen_buf, "return %s(%s);", lambda_def.c_str(), lambda_args.c_str());
		string lambda_call = string(codegen_buf);
		sprintf(codegen_buf, "[&](const fsize_t * current){\n%s\n}", tabber(lambda_call, false).c_str());
	}
	else {
		const ParseNode & exp = hiddendo.get(0);
		const ParseNode & index = hiddendo.get(1);
		const ParseNode & from = hiddendo.get(2);
		const ParseNode & to = hiddendo.get(3);
		sprintf(codegen_buf, "[&](int %s){return %s ;}", index.get_what().c_str(), exp.get_what().c_str());
		string str_lambda_body = string(codegen_buf);
		sprintf(codegen_buf, "f1a_init_hiddendo(%s, %s, %s)", from.get_what().c_str(), to.get_what().c_str(), str_lambda_body.c_str());
	}
	hiddendo.get_what() = string(codegen_buf);
}

void regen_hiddendo_expr(FunctionInfo * finfo, ParseNode & hiddendo) {
	regen_hiddendo_expr(finfo, hiddendo, [&](ParseNode & innermost_argtable) {
		regen_paramtable(finfo, innermost_argtable);
		// just change nothign and return
		sprintf(codegen_buf, "return %s;", innermost_argtable.get_what().c_str());
		innermost_argtable.get_what() = string(codegen_buf);
	});
}

void regen_hiddendo_exprex(FunctionInfo * finfo, ParseNode & hiddendo, std::function<void(ParseNode &)> regen_innermost) {
	ParseNode & argtable = hiddendo.get(0);
	ParseNode & index = hiddendo.get(1);
	ParseNode & from = hiddendo.get(2);
	ParseNode & to = hiddendo.get(3);

	int j = 0;
	std::string exprs = make_str_list(argtable.begin(), argtable.end(), [&](ParseNode * p) {
		ParseNode & pn = *p;
		regen_innermost(pn);
		j++;
		return pn.get_what();
	}, ", ");
	if (j > 1)
	{
		sprintf(codegen_buf, "return make_iostuff(make_tuple(%s));", exprs.c_str());
	}else{
		sprintf(codegen_buf, "return %s;", exprs.c_str());
	}
	std::string innermost_code = string(codegen_buf);

	//======BELOW THIS LINE JUST THE SIMILAR WITH REGEN_HIDDENDO_EXPR======//


	std::vector<ParseNode *> hiddendo_layers = get_parent_hiddendo_layers(hiddendo);
	string indexer_str = make_str_list(hiddendo_layers.rbegin(), hiddendo_layers.rend(), [](auto x)->string {
		return "fsize_t " + (x)->get(1).get_what();
	});

	sprintf(codegen_buf, "[&](%s){\n%s}", indexer_str.c_str(), tabber(innermost_code).c_str());
	string lambda_def = string(codegen_buf);
	j = 0;
	string lambda_args = make_str_list(hiddendo_layers.begin(), hiddendo_layers.end(), [&](auto x) {
		sprintf(codegen_buf, "current_%s[%d]", index.get_what().c_str(), j++);
		return string(codegen_buf);
	}, ", ");

	// map array to parameter
	sprintf(codegen_buf, "return %s(%s);", lambda_def.c_str(), lambda_args.c_str());
	string lambda_call = string(codegen_buf);
	sprintf(codegen_buf, "[&](const fsize_t * current_%s){\n%s\n}", index.get_what().c_str(), tabber(lambda_call, false).c_str());
	hiddendo.get_what() = string(codegen_buf);
}


void regen_hiddendo_exprex(FunctionInfo * finfo, ParseNode & hiddendo) {
	regen_hiddendo_exprex(finfo, hiddendo, [&](ParseNode & innermost_argtable) {
		regen_exp(finfo, innermost_argtable);
		innermost_argtable.get_what() += ";";
	});
}

ParseNode gen_hiddendo(const ParseNode & argtable, const ParseNode & index, const ParseNode & from, const ParseNode & to, TokenMeta_T return_token) {
	/**************************************
	* this function handles 1 layer at 1 time,
	*	it simply expands the hidden do into a `for` statement
	* though this function generates simpler code,
	*	it can not deal with io-implied-do, ac-implied-do, data-implied-do
	****************************************/
	ParseNode newnode = gen_token(Term{ TokenMeta::NT_HIDDENDO, "" });
	newnode.addlist(argtable, index, from, to);
	std::string stuff = "HIDDENDO GENERATED IN REGEN_HIDDENDO_XXX";
	sprintf(codegen_buf, "for(int %s = %s; %s <= %s; %s++){\n%s}", index.get_what().c_str(), from.get_what().c_str()
		, index.get_what().c_str(), to.get_what().c_str(), index.get_what().c_str(), tabber(stuff).c_str());
	newnode.fs.CurrentTerm = Term{ TokenMeta::NT_HIDDENDO, string(codegen_buf) };
	return newnode;
}
