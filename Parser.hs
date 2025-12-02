{-# OPTIONS_GHC -w #-}
module Parser where

import Token
import AST
import qualified Lex as L
--import Semantico (verProg)
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23
	| HappyAbsSyn24 t24
	| HappyAbsSyn25 t25
	| HappyAbsSyn26 t26
	| HappyAbsSyn27 t27
	| HappyAbsSyn28 t28
	| HappyAbsSyn29 t29
	| HappyAbsSyn30 t30
	| HappyAbsSyn31 t31
	| HappyAbsSyn32 t32
	| HappyAbsSyn33 t33

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,391) ([0,0,512,480,0,0,0,15360,0,0,0,32776,7,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,36270,0,0,0,46528,17,0,0,0,0,0,0,0,16384,0,0,1024,55808,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,256,0,0,0,8192,0,0,0,16384,132,61440,0,0,128,0,0,0,4096,0,0,0,0,3,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,64,112,0,0,1088,0,240,0,34816,1,7680,0,0,17,49152,3,0,0,0,64,0,1536,8,0,0,0,3,0,0,0,0,0,0,0,8192,0,1408,0,16384,4,45056,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,2048,0,0,0,4096,16385,11264,0,0,34,32776,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,18432,0,0,0,0,0,0,0,0,0,0,0,0,4096,26624,35,0,0,0,0,0,0,0,32768,0,0,0,0,0,0,8192,768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6144,4032,0,0,0,34,32776,5,0,1088,0,176,0,0,6145,0,0,0,49,49152,3,0,0,0,0,0,34304,0,0,0,0,0,0,0,0,256,0,44,0,8192,0,1408,0,16384,4,45056,0,0,136,0,22,0,0,0,0,0,0,4,0,0,0,134,0,0,0,4096,0,0,0,0,6,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,32768,513,0,0,0,16384,0,0,0,32768,1,0,0,0,0,0,0,0,0,0,32,0,0,1,0,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,0,0,7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,34,32768,7,0,32768,0,0,0,0,16,0,0,0,512,0,0,0,192,0,0,0,6144,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,24,0,0,0,0,0,0,0,32768,0,0,0,8192,32770,22528,0,0,68,16,11,0,0,0,0,0,0,12290,0,0,0,63555,1,0,0,1088,0,176,0,34816,0,5632,0,0,17,49152,2,0,544,0,88,0,17408,0,2816,0,32768,8,24576,1,0,2048,0,0,0,0,0,0,0,0,0,64,0,0,0,46080,17,0,384,0,0,0,12288,0,0,0,0,6,0,0,0,192,0,0,0,6144,0,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,24576,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,55808,8,0,16384,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_calc","Programa","ListaFunc","Func","TipoRetorno","DeclParametros","Parametro","BlocoPrincipal","Declaracoes","Declaracao","Tipo","ListaId","Bloco","ListaCmd","Comando","Retorno","CmdIf","CmdWhile","CmdAtrib","CmdEscrita","CmdLeitura","ChamadaProc","ChamadaFunc","ListaP","ExprL","ExprL2","ExprL3","ExprR","ExprA","ExprA2","ExprA3","'+'","'-'","'*'","'/'","'='","'('","')'","','","'{'","'}'","';'","'<'","'>'","'<='","'>='","'=='","'/='","'&&'","'||'","'!'","'int'","'double'","'string'","'void'","'if'","'else'","'while'","'return'","'for'","'read'","'print'","Int","Double","String","Id","%eof"]
        bit_start = st Prelude.* 69
        bit_end = (st Prelude.+ 1) Prelude.* 69
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..68]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (42) = happyShift action_12
action_0 (54) = happyShift action_6
action_0 (55) = happyShift action_7
action_0 (56) = happyShift action_8
action_0 (57) = happyShift action_9
action_0 (4) = happyGoto action_10
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 (10) = happyGoto action_11
action_0 (13) = happyGoto action_5
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (54) = happyShift action_6
action_1 (55) = happyShift action_7
action_1 (56) = happyShift action_8
action_1 (57) = happyShift action_9
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (7) = happyGoto action_4
action_1 (13) = happyGoto action_5
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (42) = happyShift action_12
action_2 (54) = happyShift action_6
action_2 (55) = happyShift action_7
action_2 (56) = happyShift action_8
action_2 (57) = happyShift action_9
action_2 (6) = happyGoto action_33
action_2 (7) = happyGoto action_4
action_2 (10) = happyGoto action_34
action_2 (13) = happyGoto action_5
action_2 _ = happyFail (happyExpListPerState 2)

action_3 _ = happyReduce_4

action_4 (68) = happyShift action_32
action_4 _ = happyFail (happyExpListPerState 4)

action_5 _ = happyReduce_7

action_6 _ = happyReduce_17

action_7 _ = happyReduce_19

action_8 _ = happyReduce_18

action_9 _ = happyReduce_8

action_10 (69) = happyAccept
action_10 _ = happyFail (happyExpListPerState 10)

action_11 _ = happyReduce_2

action_12 (54) = happyShift action_6
action_12 (55) = happyShift action_7
action_12 (56) = happyShift action_8
action_12 (58) = happyShift action_26
action_12 (60) = happyShift action_27
action_12 (61) = happyShift action_28
action_12 (63) = happyShift action_29
action_12 (64) = happyShift action_30
action_12 (68) = happyShift action_31
action_12 (11) = happyGoto action_13
action_12 (12) = happyGoto action_14
action_12 (13) = happyGoto action_15
action_12 (16) = happyGoto action_16
action_12 (17) = happyGoto action_17
action_12 (18) = happyGoto action_18
action_12 (19) = happyGoto action_19
action_12 (20) = happyGoto action_20
action_12 (21) = happyGoto action_21
action_12 (22) = happyGoto action_22
action_12 (23) = happyGoto action_23
action_12 (24) = happyGoto action_24
action_12 (25) = happyGoto action_25
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (54) = happyShift action_6
action_13 (55) = happyShift action_7
action_13 (56) = happyShift action_8
action_13 (58) = happyShift action_26
action_13 (60) = happyShift action_27
action_13 (61) = happyShift action_28
action_13 (63) = happyShift action_29
action_13 (64) = happyShift action_30
action_13 (68) = happyShift action_31
action_13 (12) = happyGoto action_57
action_13 (13) = happyGoto action_15
action_13 (16) = happyGoto action_58
action_13 (17) = happyGoto action_17
action_13 (18) = happyGoto action_18
action_13 (19) = happyGoto action_19
action_13 (20) = happyGoto action_20
action_13 (21) = happyGoto action_21
action_13 (22) = happyGoto action_22
action_13 (23) = happyGoto action_23
action_13 (24) = happyGoto action_24
action_13 (25) = happyGoto action_25
action_13 _ = happyFail (happyExpListPerState 13)

action_14 _ = happyReduce_15

action_15 (68) = happyShift action_56
action_15 (14) = happyGoto action_55
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (43) = happyShift action_54
action_16 (58) = happyShift action_26
action_16 (60) = happyShift action_27
action_16 (61) = happyShift action_28
action_16 (63) = happyShift action_29
action_16 (64) = happyShift action_30
action_16 (68) = happyShift action_31
action_16 (17) = happyGoto action_53
action_16 (18) = happyGoto action_18
action_16 (19) = happyGoto action_19
action_16 (20) = happyGoto action_20
action_16 (21) = happyGoto action_21
action_16 (22) = happyGoto action_22
action_16 (23) = happyGoto action_23
action_16 (24) = happyGoto action_24
action_16 (25) = happyGoto action_25
action_16 _ = happyFail (happyExpListPerState 16)

action_17 _ = happyReduce_24

action_18 _ = happyReduce_31

action_19 _ = happyReduce_25

action_20 _ = happyReduce_26

action_21 _ = happyReduce_27

action_22 _ = happyReduce_28

action_23 _ = happyReduce_29

action_24 _ = happyReduce_30

action_25 (44) = happyShift action_52
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (39) = happyShift action_51
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (39) = happyShift action_50
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (35) = happyShift action_43
action_28 (39) = happyShift action_44
action_28 (44) = happyShift action_45
action_28 (65) = happyShift action_46
action_28 (66) = happyShift action_47
action_28 (67) = happyShift action_48
action_28 (68) = happyShift action_49
action_28 (31) = happyGoto action_40
action_28 (32) = happyGoto action_41
action_28 (33) = happyGoto action_42
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (39) = happyShift action_39
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (39) = happyShift action_38
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (38) = happyShift action_36
action_31 (39) = happyShift action_37
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (39) = happyShift action_35
action_32 _ = happyFail (happyExpListPerState 32)

action_33 _ = happyReduce_3

action_34 _ = happyReduce_1

action_35 (40) = happyShift action_91
action_35 (54) = happyShift action_6
action_35 (55) = happyShift action_7
action_35 (56) = happyShift action_8
action_35 (8) = happyGoto action_88
action_35 (9) = happyGoto action_89
action_35 (13) = happyGoto action_90
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (35) = happyShift action_43
action_36 (39) = happyShift action_44
action_36 (65) = happyShift action_46
action_36 (66) = happyShift action_47
action_36 (67) = happyShift action_87
action_36 (68) = happyShift action_49
action_36 (31) = happyGoto action_86
action_36 (32) = happyGoto action_41
action_36 (33) = happyGoto action_42
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (35) = happyShift action_43
action_37 (39) = happyShift action_44
action_37 (40) = happyShift action_84
action_37 (65) = happyShift action_46
action_37 (66) = happyShift action_47
action_37 (67) = happyShift action_85
action_37 (68) = happyShift action_49
action_37 (26) = happyGoto action_82
action_37 (31) = happyGoto action_83
action_37 (32) = happyGoto action_41
action_37 (33) = happyGoto action_42
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (35) = happyShift action_43
action_38 (39) = happyShift action_44
action_38 (65) = happyShift action_46
action_38 (66) = happyShift action_47
action_38 (67) = happyShift action_81
action_38 (68) = happyShift action_49
action_38 (31) = happyGoto action_80
action_38 (32) = happyGoto action_41
action_38 (33) = happyGoto action_42
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (68) = happyShift action_79
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (34) = happyShift action_76
action_40 (35) = happyShift action_77
action_40 (44) = happyShift action_78
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (36) = happyShift action_74
action_41 (37) = happyShift action_75
action_41 _ = happyReduce_65

action_42 _ = happyReduce_68

action_43 (39) = happyShift action_44
action_43 (65) = happyShift action_46
action_43 (66) = happyShift action_47
action_43 (68) = happyShift action_49
action_43 (33) = happyGoto action_73
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (35) = happyShift action_43
action_44 (39) = happyShift action_44
action_44 (65) = happyShift action_46
action_44 (66) = happyShift action_47
action_44 (68) = happyShift action_49
action_44 (31) = happyGoto action_72
action_44 (32) = happyGoto action_41
action_44 (33) = happyGoto action_42
action_44 _ = happyFail (happyExpListPerState 44)

action_45 _ = happyReduce_34

action_46 _ = happyReduce_72

action_47 _ = happyReduce_71

action_48 (44) = happyShift action_71
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (39) = happyShift action_70
action_49 _ = happyReduce_73

action_50 (35) = happyShift action_43
action_50 (39) = happyShift action_67
action_50 (53) = happyShift action_68
action_50 (65) = happyShift action_46
action_50 (66) = happyShift action_47
action_50 (68) = happyShift action_49
action_50 (27) = happyGoto action_69
action_50 (28) = happyGoto action_63
action_50 (29) = happyGoto action_64
action_50 (30) = happyGoto action_65
action_50 (31) = happyGoto action_66
action_50 (32) = happyGoto action_41
action_50 (33) = happyGoto action_42
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (35) = happyShift action_43
action_51 (39) = happyShift action_67
action_51 (53) = happyShift action_68
action_51 (65) = happyShift action_46
action_51 (66) = happyShift action_47
action_51 (68) = happyShift action_49
action_51 (27) = happyGoto action_62
action_51 (28) = happyGoto action_63
action_51 (29) = happyGoto action_64
action_51 (30) = happyGoto action_65
action_51 (31) = happyGoto action_66
action_51 (32) = happyGoto action_41
action_51 (33) = happyGoto action_42
action_51 _ = happyFail (happyExpListPerState 51)

action_52 _ = happyReduce_43

action_53 _ = happyReduce_23

action_54 _ = happyReduce_13

action_55 (41) = happyShift action_60
action_55 (44) = happyShift action_61
action_55 _ = happyFail (happyExpListPerState 55)

action_56 _ = happyReduce_21

action_57 _ = happyReduce_14

action_58 (43) = happyShift action_59
action_58 (58) = happyShift action_26
action_58 (60) = happyShift action_27
action_58 (61) = happyShift action_28
action_58 (63) = happyShift action_29
action_58 (64) = happyShift action_30
action_58 (68) = happyShift action_31
action_58 (17) = happyGoto action_53
action_58 (18) = happyGoto action_18
action_58 (19) = happyGoto action_19
action_58 (20) = happyGoto action_20
action_58 (21) = happyGoto action_21
action_58 (22) = happyGoto action_22
action_58 (23) = happyGoto action_23
action_58 (24) = happyGoto action_24
action_58 (25) = happyGoto action_25
action_58 _ = happyFail (happyExpListPerState 58)

action_59 _ = happyReduce_12

action_60 (68) = happyShift action_123
action_60 _ = happyFail (happyExpListPerState 60)

action_61 _ = happyReduce_16

action_62 (40) = happyShift action_122
action_62 (51) = happyShift action_111
action_62 (52) = happyShift action_112
action_62 _ = happyFail (happyExpListPerState 62)

action_63 _ = happyReduce_52

action_64 _ = happyReduce_54

action_65 _ = happyReduce_56

action_66 (34) = happyShift action_76
action_66 (35) = happyShift action_77
action_66 (45) = happyShift action_116
action_66 (46) = happyShift action_117
action_66 (47) = happyShift action_118
action_66 (48) = happyShift action_119
action_66 (49) = happyShift action_120
action_66 (50) = happyShift action_121
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (35) = happyShift action_43
action_67 (39) = happyShift action_67
action_67 (53) = happyShift action_68
action_67 (65) = happyShift action_46
action_67 (66) = happyShift action_47
action_67 (68) = happyShift action_49
action_67 (27) = happyGoto action_114
action_67 (28) = happyGoto action_63
action_67 (29) = happyGoto action_64
action_67 (30) = happyGoto action_65
action_67 (31) = happyGoto action_115
action_67 (32) = happyGoto action_41
action_67 (33) = happyGoto action_42
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (35) = happyShift action_43
action_68 (39) = happyShift action_67
action_68 (65) = happyShift action_46
action_68 (66) = happyShift action_47
action_68 (68) = happyShift action_49
action_68 (29) = happyGoto action_113
action_68 (30) = happyGoto action_65
action_68 (31) = happyGoto action_66
action_68 (32) = happyGoto action_41
action_68 (33) = happyGoto action_42
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (40) = happyShift action_110
action_69 (51) = happyShift action_111
action_69 (52) = happyShift action_112
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (35) = happyShift action_43
action_70 (39) = happyShift action_44
action_70 (40) = happyShift action_109
action_70 (65) = happyShift action_46
action_70 (66) = happyShift action_47
action_70 (67) = happyShift action_85
action_70 (68) = happyShift action_49
action_70 (26) = happyGoto action_108
action_70 (31) = happyGoto action_83
action_70 (32) = happyGoto action_41
action_70 (33) = happyGoto action_42
action_70 _ = happyFail (happyExpListPerState 70)

action_71 _ = happyReduce_33

action_72 (34) = happyShift action_76
action_72 (35) = happyShift action_77
action_72 (40) = happyShift action_107
action_72 _ = happyFail (happyExpListPerState 72)

action_73 _ = happyReduce_69

action_74 (39) = happyShift action_44
action_74 (65) = happyShift action_46
action_74 (66) = happyShift action_47
action_74 (68) = happyShift action_49
action_74 (33) = happyGoto action_106
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (39) = happyShift action_44
action_75 (65) = happyShift action_46
action_75 (66) = happyShift action_47
action_75 (68) = happyShift action_49
action_75 (33) = happyGoto action_105
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (35) = happyShift action_43
action_76 (39) = happyShift action_44
action_76 (65) = happyShift action_46
action_76 (66) = happyShift action_47
action_76 (68) = happyShift action_49
action_76 (32) = happyGoto action_104
action_76 (33) = happyGoto action_42
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (35) = happyShift action_43
action_77 (39) = happyShift action_44
action_77 (65) = happyShift action_46
action_77 (66) = happyShift action_47
action_77 (68) = happyShift action_49
action_77 (32) = happyGoto action_103
action_77 (33) = happyGoto action_42
action_77 _ = happyFail (happyExpListPerState 77)

action_78 _ = happyReduce_32

action_79 (40) = happyShift action_102
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (34) = happyShift action_76
action_80 (35) = happyShift action_77
action_80 (40) = happyShift action_101
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (40) = happyShift action_100
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (40) = happyShift action_98
action_82 (41) = happyShift action_99
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (34) = happyShift action_76
action_83 (35) = happyShift action_77
action_83 _ = happyReduce_48

action_84 _ = happyReduce_45

action_85 _ = happyReduce_49

action_86 (34) = happyShift action_76
action_86 (35) = happyShift action_77
action_86 (44) = happyShift action_97
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (44) = happyShift action_96
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (40) = happyShift action_94
action_88 (41) = happyShift action_95
action_88 _ = happyFail (happyExpListPerState 88)

action_89 _ = happyReduce_10

action_90 (68) = happyShift action_93
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (42) = happyShift action_12
action_91 (10) = happyGoto action_92
action_91 _ = happyFail (happyExpListPerState 91)

action_92 _ = happyReduce_6

action_93 _ = happyReduce_11

action_94 (42) = happyShift action_12
action_94 (10) = happyGoto action_143
action_94 _ = happyFail (happyExpListPerState 94)

action_95 (54) = happyShift action_6
action_95 (55) = happyShift action_7
action_95 (56) = happyShift action_8
action_95 (9) = happyGoto action_142
action_95 (13) = happyGoto action_90
action_95 _ = happyFail (happyExpListPerState 95)

action_96 _ = happyReduce_39

action_97 _ = happyReduce_38

action_98 _ = happyReduce_44

action_99 (35) = happyShift action_43
action_99 (39) = happyShift action_44
action_99 (65) = happyShift action_46
action_99 (66) = happyShift action_47
action_99 (67) = happyShift action_141
action_99 (68) = happyShift action_49
action_99 (31) = happyGoto action_140
action_99 (32) = happyGoto action_41
action_99 (33) = happyGoto action_42
action_99 _ = happyFail (happyExpListPerState 99)

action_100 (44) = happyShift action_139
action_100 _ = happyFail (happyExpListPerState 100)

action_101 (44) = happyShift action_138
action_101 _ = happyFail (happyExpListPerState 101)

action_102 (44) = happyShift action_137
action_102 _ = happyFail (happyExpListPerState 102)

action_103 (36) = happyShift action_74
action_103 (37) = happyShift action_75
action_103 _ = happyReduce_64

action_104 (36) = happyShift action_74
action_104 (37) = happyShift action_75
action_104 _ = happyReduce_63

action_105 _ = happyReduce_67

action_106 _ = happyReduce_66

action_107 _ = happyReduce_70

action_108 (40) = happyShift action_136
action_108 (41) = happyShift action_99
action_108 _ = happyFail (happyExpListPerState 108)

action_109 _ = happyReduce_74

action_110 (42) = happyShift action_125
action_110 (15) = happyGoto action_135
action_110 _ = happyFail (happyExpListPerState 110)

action_111 (35) = happyShift action_43
action_111 (39) = happyShift action_67
action_111 (53) = happyShift action_68
action_111 (65) = happyShift action_46
action_111 (66) = happyShift action_47
action_111 (68) = happyShift action_49
action_111 (28) = happyGoto action_134
action_111 (29) = happyGoto action_64
action_111 (30) = happyGoto action_65
action_111 (31) = happyGoto action_66
action_111 (32) = happyGoto action_41
action_111 (33) = happyGoto action_42
action_111 _ = happyFail (happyExpListPerState 111)

action_112 (35) = happyShift action_43
action_112 (39) = happyShift action_67
action_112 (53) = happyShift action_68
action_112 (65) = happyShift action_46
action_112 (66) = happyShift action_47
action_112 (68) = happyShift action_49
action_112 (28) = happyGoto action_133
action_112 (29) = happyGoto action_64
action_112 (30) = happyGoto action_65
action_112 (31) = happyGoto action_66
action_112 (32) = happyGoto action_41
action_112 (33) = happyGoto action_42
action_112 _ = happyFail (happyExpListPerState 112)

action_113 _ = happyReduce_53

action_114 (40) = happyShift action_132
action_114 (51) = happyShift action_111
action_114 (52) = happyShift action_112
action_114 _ = happyFail (happyExpListPerState 114)

action_115 (34) = happyShift action_76
action_115 (35) = happyShift action_77
action_115 (40) = happyShift action_107
action_115 (45) = happyShift action_116
action_115 (46) = happyShift action_117
action_115 (47) = happyShift action_118
action_115 (48) = happyShift action_119
action_115 (49) = happyShift action_120
action_115 (50) = happyShift action_121
action_115 _ = happyFail (happyExpListPerState 115)

action_116 (35) = happyShift action_43
action_116 (39) = happyShift action_44
action_116 (65) = happyShift action_46
action_116 (66) = happyShift action_47
action_116 (68) = happyShift action_49
action_116 (31) = happyGoto action_131
action_116 (32) = happyGoto action_41
action_116 (33) = happyGoto action_42
action_116 _ = happyFail (happyExpListPerState 116)

action_117 (35) = happyShift action_43
action_117 (39) = happyShift action_44
action_117 (65) = happyShift action_46
action_117 (66) = happyShift action_47
action_117 (68) = happyShift action_49
action_117 (31) = happyGoto action_130
action_117 (32) = happyGoto action_41
action_117 (33) = happyGoto action_42
action_117 _ = happyFail (happyExpListPerState 117)

action_118 (35) = happyShift action_43
action_118 (39) = happyShift action_44
action_118 (65) = happyShift action_46
action_118 (66) = happyShift action_47
action_118 (68) = happyShift action_49
action_118 (31) = happyGoto action_129
action_118 (32) = happyGoto action_41
action_118 (33) = happyGoto action_42
action_118 _ = happyFail (happyExpListPerState 118)

action_119 (35) = happyShift action_43
action_119 (39) = happyShift action_44
action_119 (65) = happyShift action_46
action_119 (66) = happyShift action_47
action_119 (68) = happyShift action_49
action_119 (31) = happyGoto action_128
action_119 (32) = happyGoto action_41
action_119 (33) = happyGoto action_42
action_119 _ = happyFail (happyExpListPerState 119)

action_120 (35) = happyShift action_43
action_120 (39) = happyShift action_44
action_120 (65) = happyShift action_46
action_120 (66) = happyShift action_47
action_120 (68) = happyShift action_49
action_120 (31) = happyGoto action_127
action_120 (32) = happyGoto action_41
action_120 (33) = happyGoto action_42
action_120 _ = happyFail (happyExpListPerState 120)

action_121 (35) = happyShift action_43
action_121 (39) = happyShift action_44
action_121 (65) = happyShift action_46
action_121 (66) = happyShift action_47
action_121 (68) = happyShift action_49
action_121 (31) = happyGoto action_126
action_121 (32) = happyGoto action_41
action_121 (33) = happyGoto action_42
action_121 _ = happyFail (happyExpListPerState 121)

action_122 (42) = happyShift action_125
action_122 (15) = happyGoto action_124
action_122 _ = happyFail (happyExpListPerState 122)

action_123 _ = happyReduce_20

action_124 (59) = happyShift action_145
action_124 _ = happyReduce_35

action_125 (58) = happyShift action_26
action_125 (60) = happyShift action_27
action_125 (61) = happyShift action_28
action_125 (63) = happyShift action_29
action_125 (64) = happyShift action_30
action_125 (68) = happyShift action_31
action_125 (16) = happyGoto action_144
action_125 (17) = happyGoto action_17
action_125 (18) = happyGoto action_18
action_125 (19) = happyGoto action_19
action_125 (20) = happyGoto action_20
action_125 (21) = happyGoto action_21
action_125 (22) = happyGoto action_22
action_125 (23) = happyGoto action_23
action_125 (24) = happyGoto action_24
action_125 (25) = happyGoto action_25
action_125 _ = happyFail (happyExpListPerState 125)

action_126 (34) = happyShift action_76
action_126 (35) = happyShift action_77
action_126 _ = happyReduce_58

action_127 (34) = happyShift action_76
action_127 (35) = happyShift action_77
action_127 _ = happyReduce_57

action_128 (34) = happyShift action_76
action_128 (35) = happyShift action_77
action_128 _ = happyReduce_61

action_129 (34) = happyShift action_76
action_129 (35) = happyShift action_77
action_129 _ = happyReduce_62

action_130 (34) = happyShift action_76
action_130 (35) = happyShift action_77
action_130 _ = happyReduce_59

action_131 (34) = happyShift action_76
action_131 (35) = happyShift action_77
action_131 _ = happyReduce_60

action_132 _ = happyReduce_55

action_133 _ = happyReduce_51

action_134 _ = happyReduce_50

action_135 _ = happyReduce_37

action_136 _ = happyReduce_75

action_137 _ = happyReduce_42

action_138 _ = happyReduce_40

action_139 _ = happyReduce_41

action_140 (34) = happyShift action_76
action_140 (35) = happyShift action_77
action_140 _ = happyReduce_46

action_141 _ = happyReduce_47

action_142 _ = happyReduce_9

action_143 _ = happyReduce_5

action_144 (43) = happyShift action_147
action_144 (58) = happyShift action_26
action_144 (60) = happyShift action_27
action_144 (61) = happyShift action_28
action_144 (63) = happyShift action_29
action_144 (64) = happyShift action_30
action_144 (68) = happyShift action_31
action_144 (17) = happyGoto action_53
action_144 (18) = happyGoto action_18
action_144 (19) = happyGoto action_19
action_144 (20) = happyGoto action_20
action_144 (21) = happyGoto action_21
action_144 (22) = happyGoto action_22
action_144 (23) = happyGoto action_23
action_144 (24) = happyGoto action_24
action_144 (25) = happyGoto action_25
action_144 _ = happyFail (happyExpListPerState 144)

action_145 (42) = happyShift action_125
action_145 (15) = happyGoto action_146
action_145 _ = happyFail (happyExpListPerState 145)

action_146 _ = happyReduce_36

action_147 _ = happyReduce_22

happyReduce_1 = happySpecReduce_2  4 happyReduction_1
happyReduction_1 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (Prog (map fst happy_var_1) (map (\((ident :->: (vars, tipo)), bloco) -> (ident, fst bloco, snd bloco)) happy_var_1) (fst happy_var_2) (snd happy_var_2)
	)
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn4
		 (Prog [] [] (fst happy_var_1) (snd happy_var_1)
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1++[happy_var_2]
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  5 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 ([happy_var_1]
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happyReduce 6 6 happyReduction_5
happyReduction_5 ((HappyAbsSyn10  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (ID happy_var_2)) `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 ((happy_var_2 :->: (happy_var_4, happy_var_1)), (happy_var_6)
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 5 6 happyReduction_6
happyReduction_6 ((HappyAbsSyn10  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (ID happy_var_2)) `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 ((happy_var_2 :->: ([], happy_var_1)), (happy_var_5)
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_1  7 happyReduction_7
happyReduction_7 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  7 happyReduction_8
happyReduction_8 _
	 =  HappyAbsSyn7
		 (TVoid
	)

happyReduce_9 = happySpecReduce_3  8 happyReduction_9
happyReduction_9 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1++[happy_var_3]
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  8 happyReduction_10
happyReduction_10 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 ([happy_var_1]
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  9 happyReduction_11
happyReduction_11 (HappyTerminal (ID happy_var_2))
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_2 :#: (happy_var_1,0)
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happyReduce 4 10 happyReduction_12
happyReduction_12 (_ `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (happy_var_2,happy_var_3
	) `HappyStk` happyRest

happyReduce_13 = happySpecReduce_3  10 happyReduction_13
happyReduction_13 _
	(HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn10
		 ([],happy_var_2
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2  11 happyReduction_14
happyReduction_14 (HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1++happy_var_2
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  11 happyReduction_15
happyReduction_15 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  12 happyReduction_16
happyReduction_16 _
	(HappyAbsSyn14  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 (map (\ident -> ident :#: (happy_var_1,0)) happy_var_2
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  13 happyReduction_17
happyReduction_17 _
	 =  HappyAbsSyn13
		 (TInt
	)

happyReduce_18 = happySpecReduce_1  13 happyReduction_18
happyReduction_18 _
	 =  HappyAbsSyn13
		 (TString
	)

happyReduce_19 = happySpecReduce_1  13 happyReduction_19
happyReduction_19 _
	 =  HappyAbsSyn13
		 (TDouble
	)

happyReduce_20 = happySpecReduce_3  14 happyReduction_20
happyReduction_20 (HappyTerminal (ID happy_var_3))
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1++[happy_var_3]
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  14 happyReduction_21
happyReduction_21 (HappyTerminal (ID happy_var_1))
	 =  HappyAbsSyn14
		 ([happy_var_1]
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  15 happyReduction_22
happyReduction_22 _
	(HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (happy_var_2
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_2  16 happyReduction_23
happyReduction_23 (HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1++[happy_var_2]
	)
happyReduction_23 _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  16 happyReduction_24
happyReduction_24 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 ([happy_var_1]
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  17 happyReduction_25
happyReduction_25 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  17 happyReduction_26
happyReduction_26 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  17 happyReduction_27
happyReduction_27 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  17 happyReduction_28
happyReduction_28 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  17 happyReduction_29
happyReduction_29 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  17 happyReduction_30
happyReduction_30 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  17 happyReduction_31
happyReduction_31 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  18 happyReduction_32
happyReduction_32 _
	(HappyAbsSyn31  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (Ret (Just happy_var_2)
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  18 happyReduction_33
happyReduction_33 _
	(HappyTerminal (LSTRING happy_var_2))
	_
	 =  HappyAbsSyn18
		 (Ret (Just (Lit happy_var_2))
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_2  18 happyReduction_34
happyReduction_34 _
	_
	 =  HappyAbsSyn18
		 (Ret (Nothing)
	)

happyReduce_35 = happyReduce 5 19 happyReduction_35
happyReduction_35 ((HappyAbsSyn15  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (If happy_var_3 happy_var_5 []
	) `HappyStk` happyRest

happyReduce_36 = happyReduce 7 19 happyReduction_36
happyReduction_36 ((HappyAbsSyn15  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (If happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_37 = happyReduce 5 20 happyReduction_37
happyReduction_37 ((HappyAbsSyn15  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (While happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_38 = happyReduce 4 21 happyReduction_38
happyReduction_38 (_ `HappyStk`
	(HappyAbsSyn31  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (ID happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (Atrib happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_39 = happyReduce 4 21 happyReduction_39
happyReduction_39 (_ `HappyStk`
	(HappyTerminal (LSTRING happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (ID happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (Atrib happy_var_1 (Lit happy_var_3)
	) `HappyStk` happyRest

happyReduce_40 = happyReduce 5 22 happyReduction_40
happyReduction_40 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn31  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (Imp happy_var_3
	) `HappyStk` happyRest

happyReduce_41 = happyReduce 5 22 happyReduction_41
happyReduction_41 (_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (LSTRING happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (Imp (Lit happy_var_3)
	) `HappyStk` happyRest

happyReduce_42 = happyReduce 5 23 happyReduction_42
happyReduction_42 (_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (ID happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 (Leitura happy_var_3
	) `HappyStk` happyRest

happyReduce_43 = happySpecReduce_2  24 happyReduction_43
happyReduction_43 _
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_43 _ _  = notHappyAtAll 

happyReduce_44 = happyReduce 4 25 happyReduction_44
happyReduction_44 (_ `HappyStk`
	(HappyAbsSyn26  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (ID happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (Proc happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_45 = happySpecReduce_3  25 happyReduction_45
happyReduction_45 _
	_
	(HappyTerminal (ID happy_var_1))
	 =  HappyAbsSyn25
		 (Proc happy_var_1 []
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  26 happyReduction_46
happyReduction_46 (HappyAbsSyn31  happy_var_3)
	_
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1++[happy_var_3]
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  26 happyReduction_47
happyReduction_47 (HappyTerminal (LSTRING happy_var_3))
	_
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn26
		 (happy_var_1++[Lit happy_var_3]
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  26 happyReduction_48
happyReduction_48 (HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn26
		 ([happy_var_1]
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  26 happyReduction_49
happyReduction_49 (HappyTerminal (LSTRING happy_var_1))
	 =  HappyAbsSyn26
		 ([Lit happy_var_1]
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_3  27 happyReduction_50
happyReduction_50 (HappyAbsSyn28  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (And happy_var_1 happy_var_3
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  27 happyReduction_51
happyReduction_51 (HappyAbsSyn28  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (Or happy_var_1 happy_var_3
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  27 happyReduction_52
happyReduction_52 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_2  28 happyReduction_53
happyReduction_53 (HappyAbsSyn29  happy_var_2)
	_
	 =  HappyAbsSyn28
		 (Not happy_var_2
	)
happyReduction_53 _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_1  28 happyReduction_54
happyReduction_54 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn28
		 (happy_var_1
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_3  29 happyReduction_55
happyReduction_55 _
	(HappyAbsSyn27  happy_var_2)
	_
	 =  HappyAbsSyn29
		 (happy_var_2
	)
happyReduction_55 _ _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  29 happyReduction_56
happyReduction_56 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn29
		 (Rel happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_3  30 happyReduction_57
happyReduction_57 (HappyAbsSyn31  happy_var_3)
	_
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn30
		 (Req happy_var_1 happy_var_3
	)
happyReduction_57 _ _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_3  30 happyReduction_58
happyReduction_58 (HappyAbsSyn31  happy_var_3)
	_
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn30
		 (Rdif happy_var_1 happy_var_3
	)
happyReduction_58 _ _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_3  30 happyReduction_59
happyReduction_59 (HappyAbsSyn31  happy_var_3)
	_
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn30
		 (Rgt happy_var_1 happy_var_3
	)
happyReduction_59 _ _ _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_3  30 happyReduction_60
happyReduction_60 (HappyAbsSyn31  happy_var_3)
	_
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn30
		 (Rlt happy_var_1 happy_var_3
	)
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_3  30 happyReduction_61
happyReduction_61 (HappyAbsSyn31  happy_var_3)
	_
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn30
		 (Rge happy_var_1 happy_var_3
	)
happyReduction_61 _ _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_3  30 happyReduction_62
happyReduction_62 (HappyAbsSyn31  happy_var_3)
	_
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn30
		 (Rle happy_var_1 happy_var_3
	)
happyReduction_62 _ _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_3  31 happyReduction_63
happyReduction_63 (HappyAbsSyn32  happy_var_3)
	_
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 (Add happy_var_1 happy_var_3
	)
happyReduction_63 _ _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_3  31 happyReduction_64
happyReduction_64 (HappyAbsSyn32  happy_var_3)
	_
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 (Sub happy_var_1 happy_var_3
	)
happyReduction_64 _ _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_1  31 happyReduction_65
happyReduction_65 (HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn31
		 (happy_var_1
	)
happyReduction_65 _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_3  32 happyReduction_66
happyReduction_66 (HappyAbsSyn33  happy_var_3)
	_
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn32
		 (Mul happy_var_1 happy_var_3
	)
happyReduction_66 _ _ _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_3  32 happyReduction_67
happyReduction_67 (HappyAbsSyn33  happy_var_3)
	_
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn32
		 (Div happy_var_1 happy_var_3
	)
happyReduction_67 _ _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_1  32 happyReduction_68
happyReduction_68 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn32
		 (happy_var_1
	)
happyReduction_68 _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_2  32 happyReduction_69
happyReduction_69 (HappyAbsSyn33  happy_var_2)
	_
	 =  HappyAbsSyn32
		 (Neg happy_var_2
	)
happyReduction_69 _ _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_3  33 happyReduction_70
happyReduction_70 _
	(HappyAbsSyn31  happy_var_2)
	_
	 =  HappyAbsSyn33
		 (happy_var_2
	)
happyReduction_70 _ _ _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_1  33 happyReduction_71
happyReduction_71 (HappyTerminal (CDOUBLE happy_var_1))
	 =  HappyAbsSyn33
		 (Const (CDouble happy_var_1)
	)
happyReduction_71 _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_1  33 happyReduction_72
happyReduction_72 (HappyTerminal (CINT happy_var_1))
	 =  HappyAbsSyn33
		 (Const (CInt happy_var_1)
	)
happyReduction_72 _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_1  33 happyReduction_73
happyReduction_73 (HappyTerminal (ID happy_var_1))
	 =  HappyAbsSyn33
		 (IdVar happy_var_1
	)
happyReduction_73 _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_3  33 happyReduction_74
happyReduction_74 _
	_
	(HappyTerminal (ID happy_var_1))
	 =  HappyAbsSyn33
		 (Chamada happy_var_1 []
	)
happyReduction_74 _ _ _  = notHappyAtAll 

happyReduce_75 = happyReduce 4 33 happyReduction_75
happyReduction_75 (_ `HappyStk`
	(HappyAbsSyn26  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (ID happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn33
		 (Chamada happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 69 69 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	ADD -> cont 34;
	SUB -> cont 35;
	MUL -> cont 36;
	DIV -> cont 37;
	ATT -> cont 38;
	LPAR -> cont 39;
	RPAR -> cont 40;
	COMMA -> cont 41;
	LBRACKET -> cont 42;
	RBRACKET -> cont 43;
	SEMICOLON -> cont 44;
	MEQ -> cont 45;
	MAQ -> cont 46;
	LE -> cont 47;
	GE -> cont 48;
	IG -> cont 49;
	DIFF -> cont 50;
	AND -> cont 51;
	OR -> cont 52;
	NOT -> cont 53;
	TINT -> cont 54;
	TDOUBLE -> cont 55;
	TSTRING -> cont 56;
	TVOID -> cont 57;
	IF -> cont 58;
	ELSE -> cont 59;
	WHILE -> cont 60;
	RETURN -> cont 61;
	FOR -> cont 62;
	READ -> cont 63;
	PRINT -> cont 64;
	CINT happy_dollar_dollar -> cont 65;
	CDOUBLE happy_dollar_dollar -> cont 66;
	LSTRING happy_dollar_dollar -> cont 67;
	ID happy_dollar_dollar -> cont 68;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 69 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
calc tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError s = error ("Parse error:" ++ show s)

main :: IO ()
main = do 
    -- 1. Lê o arquivo inteiro (não apenas uma linha)
    s <- readFile "teste.j--" 
    
    -- 2. Gera os tokens
    let tokens = L.alexScanTokens s
    
    -- 3. Gera a AST (O Parser vai funcionar pois o arquivo é um Programa válido)
    let ast = calc tokens
    
    -- 4. Imprime a AST bruta (para você conferir se os nós Add, Sub, etc estão lá)
    print ast
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
