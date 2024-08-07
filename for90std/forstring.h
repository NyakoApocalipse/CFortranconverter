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
#include "fordefs.h"
#include "farray.h"
#include <string>
#include <cctype>
#include <algorithm>



_NAMESPACE_FORTRAN_BEGIN

// add implementation of forstring struct
// this implementation is lack of pointer access to the string
// it has to be further implement in the future if neccessary
// can refer to fable/fem (a c++ library to simulate fortran)
// https://github.com/cctbx/cctbx_project/blob/master/fable/fem
struct forstring{
	public:
		std::string s;
		int length;

	forstring(){
		// rarely used
		length = 0;
		s = "";
	}
	forstring(std::string input){
		s = input;
		length = input.length();
	}
	forstring(std::string input, int leng)
	{
		// need to be specified
		length = leng;
		s = "";
		if(leng > input.size())
		{
			s = input;
			for(int i = 0; i < leng-input.size(); i++)
			{
				s += ' ';
			}
		}
		else if(leng < input.size())
		{
			s = input.substr(0,leng);
		}
		else s = input;

	}
	forstring(int leng)
	{
		length = leng;
		s = "";
		for(int i = 0; i < leng; i++)
		{
			s += ' ';
		}
	}
	forstring(const char* ss)
	{
		s = ss;
		length = s.size();
	}
	forstring(char cc)
	{
		s = cc;
		length = s.size();
	}

	forstring operator=(const forstring& fs)
	{
		if(this != &fs)
		{
			if(this->length >= fs.length)
			{
				this->s = fs.s;
				for(int i = 0; i < this->length-fs.length; i++)
				{
					this->s += ' ';
				}
			}
			else
			{
				this->s = fs.s.substr(0, this->length);
			}
		}
		return *this;
	}
	forstring operator=(std::string ss)
	{
		if(this->length >= ss.size())
		{
			this->s = ss;
			for(int i = 0; i < this->length-ss.size(); i++)
			{
				this->s += ' ';
			}
		}
		else{
			this->s = ss.substr(0, this->length);
		}
		return *this;
	}
	forstring operator+(const forstring& s2)
	{
		std::string inputs = this->s+s2.s;
		return forstring(inputs);
	}
	char& operator()(int i){
		// not available
		return s[i];
	}


};

// specialization `forslice` of std::string
// inline std::string forslice(std::string str, const slice_info<int> & tp) {
inline forstring forslice(forstring str, const slice_info<int> & tp) {

	// use `slice_info<int>` to avoid narrow casting
	if (tp.to >= (int)str.s.length()) {
		// shall we append it?
		size_t appendlen = tp.to + 1 - (int)str.s.size() + 1;
		str.s += std::string(appendlen, ' ');
	}
	if (tp.step == 1) {
		std::string newstr = str.s.substr(tp.fr, tp.to - tp.fr + 1);
		return forstring(newstr);
	}
	else {
		std::string newstr;
		for (size_t i = tp.fr; (int)i <= tp.to; i += tp.step)
		{
			newstr += str.s[i];
		}
		return forstring(newstr);
	}
}

inline forstring foradjustl(forstring ss) {
	// ltrim
	std::string s = ss.s;
	s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](int ch) {
		return !std::isspace(ch);
	}));
	return forstring(s);
}

inline forstring foradjustr(forstring ss) {
	std::string s = ss.s;
	s.erase(std::find_if(s.rbegin(), s.rend(), [](int ch) {
		return !std::isspace(ch);
	}).base(), s.end());
	return forstring(s);
}

inline int forlen(forstring s)
{
	return s.length;
}

inline int trim(forstring ss)
{
	std::string s = ss.s;
	s.erase(std::find_if(s.rbegin(), s.rend(), [](int ch) {
		return !std::isspace(ch);
	}).base(), s.end());
	return s.size();
}

inline int ichar(forstring c)
{
	if(c.length == 1)
	{
		const char* tempc = c.s.c_str();
		return int(tempc[0]);
	}
	else return -1; // meaning error
}

inline forstring for_to_string(int num)
{
	return forstring(char(num));
}

inline void assign_forslice(forstring& ss, std::string ins, const slice_info<int> & tp)
{

	if(tp.to < ss.s.length() && tp.fr >= 0)
	{
		int count = 0;
		for(int i = tp.fr; i <= tp.to; i+=tp.step)
		{
			ss.s[i] = ins[count];
			count++;
		}
	}
	else printf("Over bound from assign_forslice!!\n");
	
	// need another version of implement in farray
	// not tested yet
	return;
}

_NAMESPACE_FORTRAN_END
