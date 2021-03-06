/* A Bison parser, made by GNU Bison 3.5.1.  */

/* Bison implementation for Yacc-like parsers in C

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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.5.1"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1


/* Substitute the variable and function names.  */
#define yyparse         ozyyparse
#define yylex           ozyylex
#define yyerror         ozyyerror
#define yydebug         ozyydebug
#define yynerrs         ozyynerrs
#define yylval          ozyylval
#define yychar          ozyychar

/* First part of user prologue.  */
#line 1 "poz.y"

/*
**	Grammar for Oz programs.
*/

#include	<stdio.h>
#include	<stdlib.h>
#include	<ctype.h>
#include	"oz.h"
#include	"missing.h"

extern	char	*ozyytext;

extern	void	ozyyerror(const char *s);

#line 93 "poz.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Use api.header.include to #include this header
   instead of duplicating it here.  */
#ifndef YY_OZYY_POZ_H_INCLUDED
# define YY_OZYY_POZ_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int ozyydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    INSTR_PUSH_STACK_FRAME = 258,
    INSTR_POP_STACK_FRAME = 259,
    INSTR_LOAD = 260,
    INSTR_STORE = 261,
    INSTR_LOAD_ADDRESS = 262,
    INSTR_LOAD_INDIRECT = 263,
    INSTR_STORE_INDIRECT = 264,
    INSTR_INT_CONST = 265,
    INSTR_REAL_CONST = 266,
    INSTR_STRING_CONST = 267,
    INSTR_ADD_INT = 268,
    INSTR_ADD_REAL = 269,
    INSTR_ADD_OFFSET = 270,
    INSTR_SUB_INT = 271,
    INSTR_SUB_REAL = 272,
    INSTR_SUB_OFFSET = 273,
    INSTR_MUL_INT = 274,
    INSTR_MUL_REAL = 275,
    INSTR_DIV_INT = 276,
    INSTR_DIV_REAL = 277,
    INSTR_NEG_INT = 278,
    INSTR_NEG_REAL = 279,
    INSTR_CMP_EQ_INT = 280,
    INSTR_CMP_NE_INT = 281,
    INSTR_CMP_GT_INT = 282,
    INSTR_CMP_GE_INT = 283,
    INSTR_CMP_LT_INT = 284,
    INSTR_CMP_LE_INT = 285,
    INSTR_CMP_EQ_REAL = 286,
    INSTR_CMP_NE_REAL = 287,
    INSTR_CMP_GT_REAL = 288,
    INSTR_CMP_GE_REAL = 289,
    INSTR_CMP_LT_REAL = 290,
    INSTR_CMP_LE_REAL = 291,
    INSTR_CMP_EQ_STRING = 292,
    INSTR_CMP_NE_STRING = 293,
    INSTR_CMP_GT_STRING = 294,
    INSTR_CMP_GE_STRING = 295,
    INSTR_CMP_LT_STRING = 296,
    INSTR_CMP_LE_STRING = 297,
    INSTR_AND = 298,
    INSTR_OR = 299,
    INSTR_NOT = 300,
    INSTR_BRANCH_UNCOND = 301,
    INSTR_BRANCH_ON_TRUE = 302,
    INSTR_BRANCH_ON_FALSE = 303,
    INSTR_CALL = 304,
    INSTR_CALL_BUILTIN = 305,
    INSTR_RETURN = 306,
    INSTR_INT_TO_REAL = 307,
    INSTR_MOVE = 308,
    INSTR_DEBUG_REG = 309,
    INSTR_DEBUG_SLOT = 310,
    INSTR_DEBUG_STACK = 311,
    INSTR_HALT = 312,
    FUNC_READ_INT = 313,
    FUNC_READ_REAL = 314,
    FUNC_READ_BOOL = 315,
    FUNC_READ_STRING = 316,
    FUNC_PRINT_INT = 317,
    FUNC_PRINT_REAL = 318,
    FUNC_PRINT_BOOL = 319,
    FUNC_PRINT_STRING = 320,
    FUNC_PRINT_NEWLINE = 321,
    FUNC_STRING_CONCAT = 322,
    FUNC_STRING_LENGTH = 323,
    FUNC_SUBSTRING = 324,
    FUNC_SQRT = 325,
    FUNC_TRUNC = 326,
    FUNC_ROUND = 327,
    COMMA = 328,
    COLON = 329,
    TOKEN_REG = 330,
    TOKEN_ID = 331,
    NAT_CONST = 332,
    INT_CONST = 333,
    REAL_CONST = 334,
    STRING_CONST = 335,
    GARBAGE = 336
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 20 "poz.y"

	char		*Ustr;
	int		Uint;
	bool		Ubool;
	float		Ureal;
	Function	Ufunc;

#line 235 "poz.c"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE ozyylval;

int ozyyparse (void);

#endif /* !YY_OZYY_POZ_H_INCLUDED  */



#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))

/* Stored state numbers (used for stacks). */
typedef yytype_int16 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && ! defined __ICC && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                            \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   275

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  82
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  7
/* YYNRULES -- Number of rules.  */
#define YYNRULES  77
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  281

#define YYUNDEFTOK  2
#define YYMAXUTOK   336


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   133,   133,   136,   137,   140,   145,   151,   157,   164,
     171,   178,   185,   193,   200,   207,   215,   223,   231,   239,
     247,   255,   263,   271,   279,   287,   295,   302,   310,   318,
     326,   334,   342,   350,   359,   367,   375,   383,   391,   399,
     409,   417,   425,   433,   441,   449,   458,   466,   474,   482,
     488,   495,   503,   509,   515,   521,   529,   537,   543,   549,
     554,   561,   562,   563,   564,   565,   566,   567,   568,   569,
     570,   571,   572,   573,   574,   575,   578,   590
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "INSTR_PUSH_STACK_FRAME",
  "INSTR_POP_STACK_FRAME", "INSTR_LOAD", "INSTR_STORE",
  "INSTR_LOAD_ADDRESS", "INSTR_LOAD_INDIRECT", "INSTR_STORE_INDIRECT",
  "INSTR_INT_CONST", "INSTR_REAL_CONST", "INSTR_STRING_CONST",
  "INSTR_ADD_INT", "INSTR_ADD_REAL", "INSTR_ADD_OFFSET", "INSTR_SUB_INT",
  "INSTR_SUB_REAL", "INSTR_SUB_OFFSET", "INSTR_MUL_INT", "INSTR_MUL_REAL",
  "INSTR_DIV_INT", "INSTR_DIV_REAL", "INSTR_NEG_INT", "INSTR_NEG_REAL",
  "INSTR_CMP_EQ_INT", "INSTR_CMP_NE_INT", "INSTR_CMP_GT_INT",
  "INSTR_CMP_GE_INT", "INSTR_CMP_LT_INT", "INSTR_CMP_LE_INT",
  "INSTR_CMP_EQ_REAL", "INSTR_CMP_NE_REAL", "INSTR_CMP_GT_REAL",
  "INSTR_CMP_GE_REAL", "INSTR_CMP_LT_REAL", "INSTR_CMP_LE_REAL",
  "INSTR_CMP_EQ_STRING", "INSTR_CMP_NE_STRING", "INSTR_CMP_GT_STRING",
  "INSTR_CMP_GE_STRING", "INSTR_CMP_LT_STRING", "INSTR_CMP_LE_STRING",
  "INSTR_AND", "INSTR_OR", "INSTR_NOT", "INSTR_BRANCH_UNCOND",
  "INSTR_BRANCH_ON_TRUE", "INSTR_BRANCH_ON_FALSE", "INSTR_CALL",
  "INSTR_CALL_BUILTIN", "INSTR_RETURN", "INSTR_INT_TO_REAL", "INSTR_MOVE",
  "INSTR_DEBUG_REG", "INSTR_DEBUG_SLOT", "INSTR_DEBUG_STACK", "INSTR_HALT",
  "FUNC_READ_INT", "FUNC_READ_REAL", "FUNC_READ_BOOL", "FUNC_READ_STRING",
  "FUNC_PRINT_INT", "FUNC_PRINT_REAL", "FUNC_PRINT_BOOL",
  "FUNC_PRINT_STRING", "FUNC_PRINT_NEWLINE", "FUNC_STRING_CONCAT",
  "FUNC_STRING_LENGTH", "FUNC_SUBSTRING", "FUNC_SQRT", "FUNC_TRUNC",
  "FUNC_ROUND", "COMMA", "COLON", "TOKEN_REG", "TOKEN_ID", "NAT_CONST",
  "INT_CONST", "REAL_CONST", "STRING_CONST", "GARBAGE", "$accept", "file",
  "instrs", "instr", "funcname", "reg", "int_const", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_int16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336
};
# endif

#define YYPACT_NINF (-36)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     -36,    39,    45,   -36,   -35,   -34,   -31,    26,   -31,   -31,
     -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,
     -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,
     -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,
     -31,   -31,   -31,   -31,   -31,   -31,   -31,    43,   -31,   -31,
      44,    46,   -36,   -31,   -31,   -31,    47,   -36,   -36,    48,
     -36,   -36,   -36,   -36,    54,    55,    56,    90,    91,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   -36,   130,   131,   -36,   -36,
     -36,   -36,   -36,   -36,   -36,   -36,   -36,   -36,   -36,   -36,
     -36,   -36,   -36,   -36,   -36,   132,   133,   -36,   -36,   -36,
     134,   -31,   135,   -31,   -31,   166,   165,   167,   -31,   -31,
     -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,
     -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,
     -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,
     -31,   169,   170,   -31,   -31,   -36,   -36,   -36,   -36,   -36,
     -36,   -36,   -36,   -36,   136,   137,   175,   176,   177,   178,
     179,   180,   181,   182,   -36,   -36,   183,   184,   185,   186,
     187,   188,   189,   190,   191,   192,   193,   194,   195,   196,
     197,   198,   199,   200,   201,   202,   -36,   -36,   -36,   -36,
     -36,   -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,
     -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,
     -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,   -31,
     -31,   -36,   -36,   -36,   -36,   -36,   -36,   -36,   -36,   -36,
     -36,   -36,   -36,   -36,   -36,   -36,   -36,   -36,   -36,   -36,
     -36,   -36,   -36,   -36,   -36,   -36,   -36,   -36,   -36,   -36,
     -36
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int8 yydefact[] =
{
       4,     0,     2,     1,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    54,     0,     0,     0,     0,    59,    60,     0,
       3,     6,     7,    76,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    49,     0,     0,    52,    61,
      62,    63,    64,    65,    66,    67,    68,    69,    70,    71,
      72,    73,    74,    75,    53,     0,     0,    57,    58,     5,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     8,     9,    10,    11,    12,
      77,    13,    14,    15,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    26,    27,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    48,    50,    51,    55,
      56,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
      47
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -36,   -36,   -36,   -36,   -36,    -8,   -36
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,    60,   124,    64,   181
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      66,    67,    68,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
      96,    97,    98,    99,   100,   101,   102,   103,   104,     3,
     106,   107,    61,    62,    63,   125,   126,   127,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    58,    65,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   105,
     108,    59,   129,   176,   128,   178,   179,   130,   131,   132,
     184,   185,   186,   187,   188,   189,   190,   191,   192,   193,
     194,   195,   196,   197,   198,   199,   200,   201,   202,   203,
     204,   205,   206,   207,   208,   209,   210,   211,   212,   213,
     214,   215,   216,   133,   134,   219,   220,   135,   136,   137,
     138,   139,   140,   141,   142,   143,   144,   145,   146,   147,
     148,   149,   150,   151,   152,   153,   154,   155,   156,   157,
     158,   159,   160,   161,   162,   163,   164,   165,   166,   167,
     168,   169,   170,   171,   172,   173,   174,     0,     0,   221,
     222,   175,   177,   251,   252,   253,   254,   255,   256,   257,
     258,   259,   260,   261,   262,   263,   264,   265,   266,   267,
     268,   269,   270,   271,   272,   273,   274,   275,   276,   277,
     278,   279,   280,   180,   182,   217,   218,   183,   223,   224,
     225,   226,   227,   228,   229,   230,   231,   232,   233,   234,
     235,   236,   237,   238,   239,   240,   241,   242,   243,   244,
     245,   246,   247,   248,   249,   250
};

static const yytype_int16 yycheck[] =
{
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    44,    45,    46,     0,
      48,    49,    77,    77,    75,    53,    54,    55,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    77,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    72,    76,
      76,    76,    74,   131,    77,   133,   134,    73,    73,    73,
     138,   139,   140,   141,   142,   143,   144,   145,   146,   147,
     148,   149,   150,   151,   152,   153,   154,   155,   156,   157,
     158,   159,   160,   161,   162,   163,   164,   165,   166,   167,
     168,   169,   170,    73,    73,   173,   174,    73,    73,    73,
      73,    73,    73,    73,    73,    73,    73,    73,    73,    73,
      73,    73,    73,    73,    73,    73,    73,    73,    73,    73,
      73,    73,    73,    73,    73,    73,    73,    73,    73,    73,
      73,    73,    73,    73,    73,    73,    73,    -1,    -1,    73,
      73,    77,    77,   221,   222,   223,   224,   225,   226,   227,
     228,   229,   230,   231,   232,   233,   234,   235,   236,   237,
     238,   239,   240,   241,   242,   243,   244,   245,   246,   247,
     248,   249,   250,    77,    79,    76,    76,    80,    73,    73,
      73,    73,    73,    73,    73,    73,    73,    73,    73,    73,
      73,    73,    73,    73,    73,    73,    73,    73,    73,    73,
      73,    73,    73,    73,    73,    73
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,    83,    84,     0,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    76,
      85,    77,    77,    75,    87,    77,    87,    87,    87,    87,
      87,    87,    87,    87,    87,    87,    87,    87,    87,    87,
      87,    87,    87,    87,    87,    87,    87,    87,    87,    87,
      87,    87,    87,    87,    87,    87,    87,    87,    87,    87,
      87,    87,    87,    87,    87,    76,    87,    87,    76,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    86,    87,    87,    87,    77,    74,
      73,    73,    73,    73,    73,    73,    73,    73,    73,    73,
      73,    73,    73,    73,    73,    73,    73,    73,    73,    73,
      73,    73,    73,    73,    73,    73,    73,    73,    73,    73,
      73,    73,    73,    73,    73,    73,    73,    73,    73,    73,
      73,    73,    73,    73,    73,    77,    87,    77,    87,    87,
      77,    88,    79,    80,    87,    87,    87,    87,    87,    87,
      87,    87,    87,    87,    87,    87,    87,    87,    87,    87,
      87,    87,    87,    87,    87,    87,    87,    87,    87,    87,
      87,    87,    87,    87,    87,    87,    87,    76,    76,    87,
      87,    73,    73,    73,    73,    73,    73,    73,    73,    73,
      73,    73,    73,    73,    73,    73,    73,    73,    73,    73,
      73,    73,    73,    73,    73,    73,    73,    73,    73,    73,
      73,    87,    87,    87,    87,    87,    87,    87,    87,    87,
      87,    87,    87,    87,    87,    87,    87,    87,    87,    87,
      87,    87,    87,    87,    87,    87,    87,    87,    87,    87,
      87
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int8 yyr1[] =
{
       0,    82,    83,    84,    84,    85,    85,    85,    85,    85,
      85,    85,    85,    85,    85,    85,    85,    85,    85,    85,
      85,    85,    85,    85,    85,    85,    85,    85,    85,    85,
      85,    85,    85,    85,    85,    85,    85,    85,    85,    85,
      85,    85,    85,    85,    85,    85,    85,    85,    85,    85,
      85,    85,    85,    85,    85,    85,    85,    85,    85,    85,
      85,    86,    86,    86,    86,    86,    86,    86,    86,    86,
      86,    86,    86,    86,    86,    86,    87,    88
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     1,     2,     0,     2,     2,     2,     4,     4,
       4,     4,     4,     4,     4,     4,     6,     6,     6,     6,
       6,     6,     6,     6,     6,     6,     4,     4,     6,     6,
       6,     6,     6,     6,     6,     6,     6,     6,     6,     6,
       6,     6,     6,     6,     6,     6,     6,     6,     4,     2,
       4,     4,     2,     2,     1,     4,     4,     2,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YYUSE (yyoutput);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyo, yytoknum[yytype], *yyvaluep);
# endif
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyo, yytype, yyvaluep);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[+yyssp[yyi + 1 - yynrhs]],
                       &yyvsp[(yyi + 1) - (yynrhs)]
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen(S) (YY_CAST (YYPTRDIFF_T, strlen (S)))
#  else
/* Return the length of YYSTR.  */
static YYPTRDIFF_T
yystrlen (const char *yystr)
{
  YYPTRDIFF_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYPTRDIFF_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYPTRDIFF_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            else
              goto append;

          append:
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (yyres)
    return yystpcpy (yyres, yystr) - yyres;
  else
    return yystrlen (yystr);
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYPTRDIFF_T *yymsg_alloc, char **yymsg,
                yy_state_t *yyssp, int yytoken)
{
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat: reported tokens (one for the "unexpected",
     one per "expected"). */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Actual size of YYARG. */
  int yycount = 0;
  /* Cumulated lengths of YYARG.  */
  YYPTRDIFF_T yysize = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[+*yyssp];
      YYPTRDIFF_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
      yysize = yysize0;
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYPTRDIFF_T yysize1
                    = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
                    yysize = yysize1;
                  else
                    return 2;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
    default: /* Avoid compiler warnings. */
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    /* Don't count the "%s"s in the final size, but reserve room for
       the terminator.  */
    YYPTRDIFF_T yysize1 = yysize + (yystrlen (yyformat) - 2 * yycount) + 1;
    if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
      yysize = yysize1;
    else
      return 2;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          ++yyp;
          ++yyformat;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss;
    yy_state_t *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYPTRDIFF_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYPTRDIFF_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    goto yyexhaustedlab;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
# undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 5:
#line 141 "poz.y"
                {
			record_label((yyvsp[-1].Ustr));
		}
#line 1596 "poz.c"
    break;

  case 6:
#line 146 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_PUSH_STACK_FRAME;
			instr->int_const = (yyvsp[0].Uint);
		}
#line 1606 "poz.c"
    break;

  case 7:
#line 152 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_POP_STACK_FRAME;
			instr->int_const = (yyvsp[0].Uint);
		}
#line 1616 "poz.c"
    break;

  case 8:
#line 158 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_LOAD;
			instr->rd = (yyvsp[-2].Uint);
			instr->int_const = (yyvsp[0].Uint);
		}
#line 1627 "poz.c"
    break;

  case 9:
#line 165 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_STORE;
			instr->int_const = (yyvsp[-2].Uint);
			instr->rs1 = (yyvsp[0].Uint);
		}
#line 1638 "poz.c"
    break;

  case 10:
#line 172 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_LOAD_ADDRESS;
			instr->rd = (yyvsp[-2].Uint);
			instr->int_const = (yyvsp[0].Uint);
		}
#line 1649 "poz.c"
    break;

  case 11:
#line 179 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_LOAD_INDIRECT;
			instr->rd = (yyvsp[-2].Uint);
			instr->rs1 = (yyvsp[0].Uint);
		}
#line 1660 "poz.c"
    break;

  case 12:
#line 186 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_STORE_INDIRECT;
			instr->rd = (yyvsp[-2].Uint);
			instr->rs1 = (yyvsp[0].Uint);
		}
#line 1671 "poz.c"
    break;

  case 13:
#line 194 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_INT_CONST;
			instr->rd = (yyvsp[-2].Uint);
			instr->int_const = (yyvsp[0].Uint);
		}
#line 1682 "poz.c"
    break;

  case 14:
#line 201 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_REAL_CONST;
			instr->rd = (yyvsp[-2].Uint);
			instr->real_const = (yyvsp[0].Ureal);
		}
#line 1693 "poz.c"
    break;

  case 15:
#line 208 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_STRING_CONST;
			instr->rd = (yyvsp[-2].Uint);
			instr->string_const = (yyvsp[0].Ustr);
		}
#line 1704 "poz.c"
    break;

  case 16:
#line 216 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_ADD_INT;
			instr->rd = (yyvsp[-4].Uint);
			instr->rs1 = (yyvsp[-2].Uint);
			instr->rs2 = (yyvsp[0].Uint);
		}
#line 1716 "poz.c"
    break;

  case 17:
#line 224 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_ADD_REAL;
			instr->rd = (yyvsp[-4].Uint);
			instr->rs1 = (yyvsp[-2].Uint);
			instr->rs2 = (yyvsp[0].Uint);
		}
#line 1728 "poz.c"
    break;

  case 18:
#line 232 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_ADD_OFFSET;
			instr->rd = (yyvsp[-4].Uint);
			instr->rs1 = (yyvsp[-2].Uint);
			instr->rs2 = (yyvsp[0].Uint);
		}
#line 1740 "poz.c"
    break;

  case 19:
#line 240 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_SUB_INT;
			instr->rd = (yyvsp[-4].Uint);
			instr->rs1 = (yyvsp[-2].Uint);
			instr->rs2 = (yyvsp[0].Uint);
		}
#line 1752 "poz.c"
    break;

  case 20:
#line 248 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_SUB_REAL;
			instr->rd = (yyvsp[-4].Uint);
			instr->rs1 = (yyvsp[-2].Uint);
			instr->rs2 = (yyvsp[0].Uint);
		}
#line 1764 "poz.c"
    break;

  case 21:
#line 256 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_SUB_OFFSET;
			instr->rd = (yyvsp[-4].Uint);
			instr->rs1 = (yyvsp[-2].Uint);
			instr->rs2 = (yyvsp[0].Uint);
		}
#line 1776 "poz.c"
    break;

  case 22:
#line 264 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_MUL_INT;
			instr->rd = (yyvsp[-4].Uint);
			instr->rs1 = (yyvsp[-2].Uint);
			instr->rs2 = (yyvsp[0].Uint);
		}
#line 1788 "poz.c"
    break;

  case 23:
#line 272 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_MUL_REAL;
			instr->rd = (yyvsp[-4].Uint);
			instr->rs1 = (yyvsp[-2].Uint);
			instr->rs2 = (yyvsp[0].Uint);
		}
#line 1800 "poz.c"
    break;

  case 24:
#line 280 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_DIV_INT;
			instr->rd = (yyvsp[-4].Uint);
			instr->rs1 = (yyvsp[-2].Uint);
			instr->rs2 = (yyvsp[0].Uint);
		}
#line 1812 "poz.c"
    break;

  case 25:
#line 288 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_DIV_REAL;
			instr->rd = (yyvsp[-4].Uint);
			instr->rs1 = (yyvsp[-2].Uint);
			instr->rs2 = (yyvsp[0].Uint);
		}
#line 1824 "poz.c"
    break;

  case 26:
#line 296 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_NEG_INT;
			instr->rd = (yyvsp[-2].Uint);
			instr->rs1 = (yyvsp[0].Uint);
		}
#line 1835 "poz.c"
    break;

  case 27:
#line 303 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_NEG_REAL;
			instr->rd = (yyvsp[-2].Uint);
			instr->rs1 = (yyvsp[0].Uint);
		}
#line 1846 "poz.c"
    break;

  case 28:
#line 311 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_EQ_INT;
			instr->rd = (yyvsp[-4].Uint);
			instr->rs1 = (yyvsp[-2].Uint);
			instr->rs2 = (yyvsp[0].Uint);
		}
#line 1858 "poz.c"
    break;

  case 29:
#line 319 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_NE_INT;
			instr->rd = (yyvsp[-4].Uint);
			instr->rs1 = (yyvsp[-2].Uint);
			instr->rs2 = (yyvsp[0].Uint);
		}
#line 1870 "poz.c"
    break;

  case 30:
#line 327 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_GT_INT;
			instr->rd = (yyvsp[-4].Uint);
			instr->rs1 = (yyvsp[-2].Uint);
			instr->rs2 = (yyvsp[0].Uint);
		}
#line 1882 "poz.c"
    break;

  case 31:
#line 335 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_GE_INT;
			instr->rd = (yyvsp[-4].Uint);
			instr->rs1 = (yyvsp[-2].Uint);
			instr->rs2 = (yyvsp[0].Uint);
		}
#line 1894 "poz.c"
    break;

  case 32:
#line 343 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_LT_INT;
			instr->rd = (yyvsp[-4].Uint);
			instr->rs1 = (yyvsp[-2].Uint);
			instr->rs2 = (yyvsp[0].Uint);
		}
#line 1906 "poz.c"
    break;

  case 33:
#line 351 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_LE_INT;
			instr->rd = (yyvsp[-4].Uint);
			instr->rs1 = (yyvsp[-2].Uint);
			instr->rs2 = (yyvsp[0].Uint);
		}
#line 1918 "poz.c"
    break;

  case 34:
#line 360 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_EQ_REAL;
			instr->rd = (yyvsp[-4].Uint);
			instr->rs1 = (yyvsp[-2].Uint);
			instr->rs2 = (yyvsp[0].Uint);
		}
#line 1930 "poz.c"
    break;

  case 35:
#line 368 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_NE_REAL;
			instr->rd = (yyvsp[-4].Uint);
			instr->rs1 = (yyvsp[-2].Uint);
			instr->rs2 = (yyvsp[0].Uint);
		}
#line 1942 "poz.c"
    break;

  case 36:
#line 376 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_GT_REAL;
			instr->rd = (yyvsp[-4].Uint);
			instr->rs1 = (yyvsp[-2].Uint);
			instr->rs2 = (yyvsp[0].Uint);
		}
#line 1954 "poz.c"
    break;

  case 37:
#line 384 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_GE_REAL;
			instr->rd = (yyvsp[-4].Uint);
			instr->rs1 = (yyvsp[-2].Uint);
			instr->rs2 = (yyvsp[0].Uint);
		}
#line 1966 "poz.c"
    break;

  case 38:
#line 392 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_LT_REAL;
			instr->rd = (yyvsp[-4].Uint);
			instr->rs1 = (yyvsp[-2].Uint);
			instr->rs2 = (yyvsp[0].Uint);
		}
#line 1978 "poz.c"
    break;

  case 39:
#line 400 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_LE_REAL;
			instr->rd = (yyvsp[-4].Uint);
			instr->rs1 = (yyvsp[-2].Uint);
			instr->rs2 = (yyvsp[0].Uint);
		}
#line 1990 "poz.c"
    break;

  case 40:
#line 410 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_EQ_STRING;
			instr->rd = (yyvsp[-4].Uint);
			instr->rs1 = (yyvsp[-2].Uint);
			instr->rs2 = (yyvsp[0].Uint);
		}
#line 2002 "poz.c"
    break;

  case 41:
#line 418 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_NE_STRING;
			instr->rd = (yyvsp[-4].Uint);
			instr->rs1 = (yyvsp[-2].Uint);
			instr->rs2 = (yyvsp[0].Uint);
		}
#line 2014 "poz.c"
    break;

  case 42:
#line 426 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_GT_STRING;
			instr->rd = (yyvsp[-4].Uint);
			instr->rs1 = (yyvsp[-2].Uint);
			instr->rs2 = (yyvsp[0].Uint);
		}
#line 2026 "poz.c"
    break;

  case 43:
#line 434 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_GE_STRING;
			instr->rd = (yyvsp[-4].Uint);
			instr->rs1 = (yyvsp[-2].Uint);
			instr->rs2 = (yyvsp[0].Uint);
		}
#line 2038 "poz.c"
    break;

  case 44:
#line 442 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_LT_STRING;
			instr->rd = (yyvsp[-4].Uint);
			instr->rs1 = (yyvsp[-2].Uint);
			instr->rs2 = (yyvsp[0].Uint);
		}
#line 2050 "poz.c"
    break;

  case 45:
#line 450 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_CMP_LE_STRING;
			instr->rd = (yyvsp[-4].Uint);
			instr->rs1 = (yyvsp[-2].Uint);
			instr->rs2 = (yyvsp[0].Uint);
		}
#line 2062 "poz.c"
    break;

  case 46:
#line 459 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_AND;
			instr->rd = (yyvsp[-4].Uint);
			instr->rs1 = (yyvsp[-2].Uint);
			instr->rs2 = (yyvsp[0].Uint);
		}
#line 2074 "poz.c"
    break;

  case 47:
#line 467 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_OR;
			instr->rd = (yyvsp[-4].Uint);
			instr->rs1 = (yyvsp[-2].Uint);
			instr->rs2 = (yyvsp[0].Uint);
		}
#line 2086 "poz.c"
    break;

  case 48:
#line 475 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_NOT;
			instr->rd = (yyvsp[-2].Uint);
			instr->rs1 = (yyvsp[0].Uint);
		}
#line 2097 "poz.c"
    break;

  case 49:
#line 483 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_BRANCH_UNCOND;
			instr->string_const = (yyvsp[0].Ustr);
		}
#line 2107 "poz.c"
    break;

  case 50:
#line 489 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_BRANCH_ON_TRUE;
			instr->rs1 = (yyvsp[-2].Uint);
			instr->string_const = (yyvsp[0].Ustr);
		}
#line 2118 "poz.c"
    break;

  case 51:
#line 496 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_BRANCH_ON_FALSE;
			instr->rs1 = (yyvsp[-2].Uint);
			instr->string_const = (yyvsp[0].Ustr);
		}
#line 2129 "poz.c"
    break;

  case 52:
#line 504 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_CALL;
			instr->string_const = (yyvsp[0].Ustr);
		}
#line 2139 "poz.c"
    break;

  case 53:
#line 510 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_CALL_BUILTIN;
			instr->func = (yyvsp[0].Ufunc);
		}
#line 2149 "poz.c"
    break;

  case 54:
#line 516 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_RETURN;
		}
#line 2158 "poz.c"
    break;

  case 55:
#line 522 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_INT_TO_REAL;
			instr->rd = (yyvsp[-2].Uint);
			instr->rs1 = (yyvsp[0].Uint);
		}
#line 2169 "poz.c"
    break;

  case 56:
#line 530 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_MOVE;
			instr->rd = (yyvsp[-2].Uint);
			instr->rs1 = (yyvsp[0].Uint);
		}
#line 2180 "poz.c"
    break;

  case 57:
#line 538 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_DEBUG_REG;
			instr->rs1 = (yyvsp[0].Uint);
		}
#line 2190 "poz.c"
    break;

  case 58:
#line 544 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_DEBUG_SLOT;
			instr->int_const = (yyvsp[0].Uint);
		}
#line 2200 "poz.c"
    break;

  case 59:
#line 550 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_DEBUG_STACK;
		}
#line 2209 "poz.c"
    break;

  case 60:
#line 555 "poz.y"
                {
			Instr *instr = record_instr();
			instr->opcode = OP_HALT;
		}
#line 2218 "poz.c"
    break;

  case 61:
#line 561 "poz.y"
                                        { (yyval.Ufunc) = FUNCOP_READ_INT; 	}
#line 2224 "poz.c"
    break;

  case 62:
#line 562 "poz.y"
                                        { (yyval.Ufunc) = FUNCOP_READ_REAL;	}
#line 2230 "poz.c"
    break;

  case 63:
#line 563 "poz.y"
                                        { (yyval.Ufunc) = FUNCOP_READ_BOOL;	}
#line 2236 "poz.c"
    break;

  case 64:
#line 564 "poz.y"
                                        { (yyval.Ufunc) = FUNCOP_READ_STRING;	}
#line 2242 "poz.c"
    break;

  case 65:
#line 565 "poz.y"
                                        { (yyval.Ufunc) = FUNCOP_PRINT_INT; 	}
#line 2248 "poz.c"
    break;

  case 66:
#line 566 "poz.y"
                                        { (yyval.Ufunc) = FUNCOP_PRINT_REAL;	}
#line 2254 "poz.c"
    break;

  case 67:
#line 567 "poz.y"
                                        { (yyval.Ufunc) = FUNCOP_PRINT_BOOL;	}
#line 2260 "poz.c"
    break;

  case 68:
#line 568 "poz.y"
                                        { (yyval.Ufunc) = FUNCOP_PRINT_STRING;	}
#line 2266 "poz.c"
    break;

  case 69:
#line 569 "poz.y"
                                        { (yyval.Ufunc) = FUNCOP_PRINT_NEWLINE;	}
#line 2272 "poz.c"
    break;

  case 70:
#line 570 "poz.y"
                                        { (yyval.Ufunc) = FUNCOP_STRING_CONCAT;	}
#line 2278 "poz.c"
    break;

  case 71:
#line 571 "poz.y"
                                        { (yyval.Ufunc) = FUNCOP_STRING_LENGTH;	}
#line 2284 "poz.c"
    break;

  case 72:
#line 572 "poz.y"
                                        { (yyval.Ufunc) = FUNCOP_SUBSTRING;	}
#line 2290 "poz.c"
    break;

  case 73:
#line 573 "poz.y"
                                        { (yyval.Ufunc) = FUNCOP_SQRT;		}
#line 2296 "poz.c"
    break;

  case 74:
#line 574 "poz.y"
                                        { (yyval.Ufunc) = FUNCOP_TRUNC;}
#line 2302 "poz.c"
    break;

  case 75:
#line 575 "poz.y"
                                        { (yyval.Ufunc) = FUNCOP_ROUND;}
#line 2308 "poz.c"
    break;

  case 76:
#line 579 "poz.y"
                {
			if ((yyvsp[0].Uint) < NUMBER_REGS) {
				(yyval.Uint) = (yyvsp[0].Uint);
			} else {
				printf("%s, %d: ", ozyyfile, ozyylinenum);
				printf("reference to r%d\n", (yyvsp[0].Uint));
				report_error_and_exit("");
			}
		}
#line 2322 "poz.c"
    break;

  case 77:
#line 591 "poz.y"
                {
			(yyval.Uint) = (yyvsp[0].Uint);
		}
#line 2330 "poz.c"
    break;


#line 2334 "poz.c"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = YY_CAST (char *, YYSTACK_ALLOC (YY_CAST (YYSIZE_T, yymsg_alloc)));
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;


#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif


/*-----------------------------------------------------.
| yyreturn -- parsing is finished, return the result.  |
`-----------------------------------------------------*/
yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[+*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 596 "poz.y"


void ozyyerror(const char *s)
{
	char		buf[80];

	if (ozyychar <= 0)
	{
		sprintf(buf, "premature EOF");
		ozyylinenum--;
	}
	else if (ozyytext[0] == '\n' || ozyytext[0] == '\f')
		sprintf(buf, "%s at end of line", s);
	else if (isprint((int) ozyytext[0]))
		sprintf(buf, "%s at symbol `%s'", s, ozyytext);
	else
		sprintf(buf, "%s at \\%o", s, ozyytext[0]);
	
	printf("%s, %d: %s\n", ozyyfile, ozyylinenum, buf);
}
