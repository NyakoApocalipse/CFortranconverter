/* A Bison parser, made by GNU Bison 3.5.1.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2020 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

#ifndef YY_YY_SRC_GRAMMAR_FOR90_TAB_HPP_INCLUDED
# define YY_YY_SRC_GRAMMAR_FOR90_TAB_HPP_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YY_IGNORE_THIS = 258,
    YY_CRLF = 259,
    YY_GT = 260,
    YY_GE = 261,
    YY_EQ = 262,
    YY_LE = 263,
    YY_LT = 264,
    YY_NEQ = 265,
    YY_NEQV = 266,
    YY_EQV = 267,
    YY_ANDAND = 268,
    YY_OROR = 269,
    YY_NOT = 270,
    YY_POWER = 271,
    YY_DOUBLECOLON = 272,
    YY_NEG = 273,
    YY_POS = 274,
    YY_EXPONENT = 275,
    YY_PLET = 276,
    YY_PNULL = 277,
    YY_INTEGER = 278,
    YY_FLOAT = 279,
    YY_WORD = 280,
    YY_OPERATOR = 281,
    YY_STRING = 282,
    YY_ILLEGAL = 283,
    YY_COMPLEX = 284,
    YY_TRUE = 285,
    YY_FALSE = 286,
    YY_FORMAT_STMT = 287,
    YY_COMMENT = 288,
    YY_LABEL = 289,
    YY_END = 290,
    YY_IF = 291,
    YY_THEN = 292,
    YY_ELSE = 293,
    YY_ELSEIF = 294,
    YY_ENDIF = 295,
    YY_DO = 296,
    YY_ENDDO = 297,
    YY_CONTINUE = 298,
    YY_BREAK = 299,
    YY_EXIT = 300,
    YY_CYCLE = 301,
    YY_WHILE = 302,
    YY_ENDWHILE = 303,
    YY_WHERE = 304,
    YY_ENDWHERE = 305,
    YY_CASE = 306,
    YY_ENDCASE = 307,
    YY_SELECT = 308,
    YY_ENDSELECT = 309,
    YY_GOTO = 310,
    YY_DOWHILE = 311,
    YY_DEFAULT = 312,
    YY_TYPE = 313,
    YY_ENDTYPE = 314,
    YY_PROGRAM = 315,
    YY_ENDPROGRAM = 316,
    YY_FUNCTION = 317,
    YY_ENDFUNCTION = 318,
    YY_RECURSIVE = 319,
    YY_RESULT = 320,
    YY_SUBROUTINE = 321,
    YY_ENDSUBROUTINE = 322,
    YY_MODULE = 323,
    YY_ENDMODULE = 324,
    YY_BLOCK = 325,
    YY_ENDBLOCK = 326,
    YY_INTERFACE = 327,
    YY_ENDINTERFACE = 328,
    YY_COMMON = 329,
    YY_DATA = 330,
    YY_PROCEDURE = 331,
    YY_CONTAINS = 332,
    YY_IMPLICIT = 333,
    YY_NONE = 334,
    YY_USE = 335,
    YY_PARAMETER = 336,
    YY_ENTRY = 337,
    YY_DIMENSION = 338,
    YY_ARRAYBUILDER_START = 339,
    YY_ARRAYBUILDER_END = 340,
    YY_INTENT = 341,
    YY_IN = 342,
    YY_OUT = 343,
    YY_INOUT = 344,
    YY_OPTIONAL = 345,
    YY_LEN = 346,
    YY_KIND = 347,
    YY_SINGR = 348,
    YY_FULLR = 349,
    YY_SINGI = 350,
    YY_FULLI = 351,
    YY_SINGL = 352,
    YY__SINGR = 353,
    YY__FULLR = 354,
    YY__SINGI = 355,
    YY__FULLI = 356,
    YY__SINGL = 357,
    YY_SAVE = 358,
    YY_ALLOCATABLE = 359,
    YY_TARGET = 360,
    YY_POINTER = 361,
    YY_FMT = 362,
    YY_INTEGER_T = 363,
    YY_FLOAT_T = 364,
    YY_STRING_T = 365,
    YY_COMPLEX_T = 366,
    YY_BOOL_T = 367,
    YY_CHARACTER_T = 368,
    YY_DOUBLE_T = 369,
    YY_WRITE = 370,
    YY_READ = 371,
    YY_PRINT = 372,
    YY_CALL = 373,
    YY_STOP = 374,
    YY_PAUSE = 375,
    YY_RETURN = 376,
    YY_CONFIG_IMPLICIT = 377,
    YY_ALLOCATE = 378
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_SRC_GRAMMAR_FOR90_TAB_HPP_INCLUDED  */
