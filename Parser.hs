{-# OPTIONS_GHC -w #-}
module Parser where
import CastData
import Data.Char
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.11

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,193) ([15360,2308,149,2168,10770,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,14336,0,8,3840,0,57344,18465,1192,448,0,32768,8327,4769,0,0,0,56320,15,0,0,0,0,0,61440,9232,596,8672,43080,49156,36931,2384,34688,41248,18,16655,9538,7680,33922,74,1084,38153,30720,4616,298,4336,21540,2,64960,0,0,0,2,0,256,0,0,2,0,1024,0,8120,2,49152,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1536,0,0,0,64,32768,7,0,3840,0,0,30,0,0,4096,0,0,0,0,0,0,0,0,15360,2308,149,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,57344,126,57345,18465,1192,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,256,0,33024,0,8672,43080,4,2,0,0,17399,0,16655,9538,0,0,0,0,0,0,8192,0,0,0,57344,18465,1192,0,120,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_calc","Expr","Consts","Expr1","ExprArith","ExprBool","Type","Type1","int","float","bool","var","\"Int\"","\"Float\"","\"Bool\"","\"Dyn\"","'\\\\'","\"==\"","\">=\"","\"<=\"","\"->\"","'+'","'-'","'*'","'/'","'<'","'>'","'.'","':'","','","'('","')'","'['","']'","if","then","else","none","%eof"]
        bit_start = st * 41
        bit_end = (st + 1) * 41
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..40]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (11) = happyShift action_4
action_0 (12) = happyShift action_5
action_0 (13) = happyShift action_6
action_0 (14) = happyShift action_7
action_0 (19) = happyShift action_8
action_0 (25) = happyShift action_9
action_0 (28) = happyShift action_10
action_0 (33) = happyShift action_11
action_0 (35) = happyShift action_12
action_0 (37) = happyShift action_13
action_0 (40) = happyShift action_14
action_0 (4) = happyGoto action_15
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 (7) = happyGoto action_16
action_0 (8) = happyGoto action_17
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (11) = happyShift action_4
action_1 (12) = happyShift action_5
action_1 (13) = happyShift action_6
action_1 (14) = happyShift action_7
action_1 (19) = happyShift action_8
action_1 (25) = happyShift action_9
action_1 (28) = happyShift action_10
action_1 (33) = happyShift action_11
action_1 (35) = happyShift action_12
action_1 (37) = happyShift action_13
action_1 (40) = happyShift action_14
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_9

action_3 _ = happyReduce_1

action_4 _ = happyReduce_4

action_5 _ = happyReduce_6

action_6 _ = happyReduce_8

action_7 _ = happyReduce_12

action_8 (14) = happyShift action_40
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (11) = happyShift action_4
action_9 (12) = happyShift action_5
action_9 (13) = happyShift action_6
action_9 (35) = happyShift action_39
action_9 (5) = happyGoto action_38
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (15) = happyShift action_34
action_10 (16) = happyShift action_35
action_10 (17) = happyShift action_36
action_10 (18) = happyShift action_37
action_10 (9) = happyGoto action_33
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (11) = happyShift action_4
action_11 (12) = happyShift action_5
action_11 (13) = happyShift action_6
action_11 (14) = happyShift action_7
action_11 (19) = happyShift action_8
action_11 (25) = happyShift action_9
action_11 (28) = happyShift action_10
action_11 (33) = happyShift action_11
action_11 (35) = happyShift action_12
action_11 (37) = happyShift action_13
action_11 (40) = happyShift action_14
action_11 (4) = happyGoto action_32
action_11 (5) = happyGoto action_2
action_11 (6) = happyGoto action_3
action_11 (7) = happyGoto action_16
action_11 (8) = happyGoto action_17
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (11) = happyShift action_29
action_12 (12) = happyShift action_30
action_12 (13) = happyShift action_31
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (11) = happyShift action_4
action_13 (12) = happyShift action_5
action_13 (13) = happyShift action_6
action_13 (14) = happyShift action_7
action_13 (19) = happyShift action_8
action_13 (25) = happyShift action_9
action_13 (28) = happyShift action_10
action_13 (33) = happyShift action_11
action_13 (35) = happyShift action_12
action_13 (37) = happyShift action_13
action_13 (40) = happyShift action_14
action_13 (4) = happyGoto action_27
action_13 (5) = happyGoto action_2
action_13 (6) = happyGoto action_3
action_13 (7) = happyGoto action_16
action_13 (8) = happyGoto action_28
action_13 _ = happyFail (happyExpListPerState 13)

action_14 _ = happyReduce_18

action_15 (20) = happyShift action_18
action_15 (21) = happyShift action_19
action_15 (22) = happyShift action_20
action_15 (24) = happyShift action_21
action_15 (25) = happyShift action_22
action_15 (26) = happyShift action_23
action_15 (27) = happyShift action_24
action_15 (28) = happyShift action_25
action_15 (29) = happyShift action_26
action_15 (41) = happyAccept
action_15 _ = happyFail (happyExpListPerState 15)

action_16 _ = happyReduce_2

action_17 _ = happyReduce_3

action_18 (11) = happyShift action_4
action_18 (12) = happyShift action_5
action_18 (13) = happyShift action_6
action_18 (14) = happyShift action_7
action_18 (19) = happyShift action_8
action_18 (25) = happyShift action_9
action_18 (28) = happyShift action_10
action_18 (33) = happyShift action_11
action_18 (35) = happyShift action_12
action_18 (37) = happyShift action_13
action_18 (40) = happyShift action_14
action_18 (5) = happyGoto action_2
action_18 (6) = happyGoto action_57
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (11) = happyShift action_4
action_19 (12) = happyShift action_5
action_19 (13) = happyShift action_6
action_19 (14) = happyShift action_7
action_19 (19) = happyShift action_8
action_19 (25) = happyShift action_9
action_19 (28) = happyShift action_10
action_19 (33) = happyShift action_11
action_19 (35) = happyShift action_12
action_19 (37) = happyShift action_13
action_19 (40) = happyShift action_14
action_19 (5) = happyGoto action_2
action_19 (6) = happyGoto action_56
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (11) = happyShift action_4
action_20 (12) = happyShift action_5
action_20 (13) = happyShift action_6
action_20 (14) = happyShift action_7
action_20 (19) = happyShift action_8
action_20 (25) = happyShift action_9
action_20 (28) = happyShift action_10
action_20 (33) = happyShift action_11
action_20 (35) = happyShift action_12
action_20 (37) = happyShift action_13
action_20 (40) = happyShift action_14
action_20 (5) = happyGoto action_2
action_20 (6) = happyGoto action_55
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (11) = happyShift action_4
action_21 (12) = happyShift action_5
action_21 (13) = happyShift action_6
action_21 (14) = happyShift action_7
action_21 (19) = happyShift action_8
action_21 (25) = happyShift action_9
action_21 (28) = happyShift action_10
action_21 (33) = happyShift action_11
action_21 (35) = happyShift action_12
action_21 (37) = happyShift action_13
action_21 (40) = happyShift action_14
action_21 (5) = happyGoto action_2
action_21 (6) = happyGoto action_54
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (11) = happyShift action_4
action_22 (12) = happyShift action_5
action_22 (13) = happyShift action_6
action_22 (14) = happyShift action_7
action_22 (19) = happyShift action_8
action_22 (25) = happyShift action_9
action_22 (28) = happyShift action_10
action_22 (33) = happyShift action_11
action_22 (35) = happyShift action_12
action_22 (37) = happyShift action_13
action_22 (40) = happyShift action_14
action_22 (5) = happyGoto action_2
action_22 (6) = happyGoto action_53
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (11) = happyShift action_4
action_23 (12) = happyShift action_5
action_23 (13) = happyShift action_6
action_23 (14) = happyShift action_7
action_23 (19) = happyShift action_8
action_23 (25) = happyShift action_9
action_23 (28) = happyShift action_10
action_23 (33) = happyShift action_11
action_23 (35) = happyShift action_12
action_23 (37) = happyShift action_13
action_23 (40) = happyShift action_14
action_23 (5) = happyGoto action_2
action_23 (6) = happyGoto action_52
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (11) = happyShift action_4
action_24 (12) = happyShift action_5
action_24 (13) = happyShift action_6
action_24 (14) = happyShift action_7
action_24 (19) = happyShift action_8
action_24 (25) = happyShift action_9
action_24 (28) = happyShift action_10
action_24 (33) = happyShift action_11
action_24 (35) = happyShift action_12
action_24 (37) = happyShift action_13
action_24 (40) = happyShift action_14
action_24 (5) = happyGoto action_2
action_24 (6) = happyGoto action_51
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (11) = happyShift action_4
action_25 (12) = happyShift action_5
action_25 (13) = happyShift action_6
action_25 (14) = happyShift action_7
action_25 (19) = happyShift action_8
action_25 (25) = happyShift action_9
action_25 (28) = happyShift action_10
action_25 (33) = happyShift action_11
action_25 (35) = happyShift action_12
action_25 (37) = happyShift action_13
action_25 (40) = happyShift action_14
action_25 (5) = happyGoto action_2
action_25 (6) = happyGoto action_50
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (11) = happyShift action_4
action_26 (12) = happyShift action_5
action_26 (13) = happyShift action_6
action_26 (14) = happyShift action_7
action_26 (19) = happyShift action_8
action_26 (25) = happyShift action_9
action_26 (28) = happyShift action_10
action_26 (33) = happyShift action_11
action_26 (35) = happyShift action_12
action_26 (37) = happyShift action_13
action_26 (40) = happyShift action_14
action_26 (5) = happyGoto action_2
action_26 (6) = happyGoto action_49
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (20) = happyShift action_18
action_27 (21) = happyShift action_19
action_27 (22) = happyShift action_20
action_27 (24) = happyShift action_21
action_27 (25) = happyShift action_22
action_27 (26) = happyShift action_23
action_27 (27) = happyShift action_24
action_27 (28) = happyShift action_25
action_27 (29) = happyShift action_26
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (38) = happyShift action_48
action_28 _ = happyReduce_3

action_29 (36) = happyShift action_47
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (36) = happyShift action_46
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (36) = happyShift action_45
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (20) = happyShift action_18
action_32 (21) = happyShift action_19
action_32 (22) = happyShift action_20
action_32 (24) = happyShift action_21
action_32 (25) = happyShift action_22
action_32 (26) = happyShift action_23
action_32 (27) = happyShift action_24
action_32 (28) = happyShift action_25
action_32 (29) = happyShift action_26
action_32 (34) = happyShift action_44
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (22) = happyShift action_42
action_33 (23) = happyShift action_43
action_33 _ = happyFail (happyExpListPerState 33)

action_34 _ = happyReduce_28

action_35 _ = happyReduce_29

action_36 _ = happyReduce_30

action_37 _ = happyReduce_31

action_38 _ = happyReduce_10

action_39 (11) = happyShift action_29
action_39 (12) = happyShift action_30
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (31) = happyShift action_41
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (15) = happyShift action_34
action_41 (16) = happyShift action_35
action_41 (17) = happyShift action_36
action_41 (18) = happyShift action_37
action_41 (9) = happyGoto action_66
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (15) = happyShift action_34
action_42 (16) = happyShift action_35
action_42 (17) = happyShift action_36
action_42 (18) = happyShift action_37
action_42 (9) = happyGoto action_65
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (15) = happyShift action_61
action_43 (16) = happyShift action_62
action_43 (17) = happyShift action_63
action_43 (18) = happyShift action_64
action_43 (10) = happyGoto action_60
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (33) = happyShift action_59
action_44 _ = happyReduce_13

action_45 _ = happyReduce_11

action_46 _ = happyReduce_7

action_47 _ = happyReduce_5

action_48 (11) = happyShift action_4
action_48 (12) = happyShift action_5
action_48 (13) = happyShift action_6
action_48 (14) = happyShift action_7
action_48 (19) = happyShift action_8
action_48 (25) = happyShift action_9
action_48 (28) = happyShift action_10
action_48 (33) = happyShift action_11
action_48 (35) = happyShift action_12
action_48 (37) = happyShift action_13
action_48 (40) = happyShift action_14
action_48 (4) = happyGoto action_58
action_48 (5) = happyGoto action_2
action_48 (6) = happyGoto action_3
action_48 (7) = happyGoto action_16
action_48 (8) = happyGoto action_17
action_48 _ = happyFail (happyExpListPerState 48)

action_49 _ = happyReduce_26

action_50 _ = happyReduce_25

action_51 _ = happyReduce_22

action_52 _ = happyReduce_21

action_53 _ = happyReduce_20

action_54 _ = happyReduce_19

action_55 _ = happyReduce_23

action_56 _ = happyReduce_24

action_57 _ = happyReduce_27

action_58 (20) = happyShift action_18
action_58 (21) = happyShift action_19
action_58 (22) = happyShift action_20
action_58 (24) = happyShift action_21
action_58 (25) = happyShift action_22
action_58 (26) = happyShift action_23
action_58 (27) = happyShift action_24
action_58 (28) = happyShift action_25
action_58 (29) = happyShift action_26
action_58 (39) = happyShift action_70
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (11) = happyShift action_4
action_59 (12) = happyShift action_5
action_59 (13) = happyShift action_6
action_59 (14) = happyShift action_7
action_59 (19) = happyShift action_8
action_59 (25) = happyShift action_9
action_59 (28) = happyShift action_10
action_59 (33) = happyShift action_11
action_59 (35) = happyShift action_12
action_59 (37) = happyShift action_13
action_59 (40) = happyShift action_14
action_59 (4) = happyGoto action_69
action_59 (5) = happyGoto action_2
action_59 (6) = happyGoto action_3
action_59 (7) = happyGoto action_16
action_59 (8) = happyGoto action_17
action_59 _ = happyFail (happyExpListPerState 59)

action_60 _ = happyReduce_32

action_61 _ = happyReduce_33

action_62 _ = happyReduce_34

action_63 _ = happyReduce_35

action_64 _ = happyReduce_36

action_65 (23) = happyShift action_43
action_65 (32) = happyShift action_68
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (23) = happyShift action_43
action_66 (30) = happyShift action_67
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (11) = happyShift action_4
action_67 (12) = happyShift action_5
action_67 (13) = happyShift action_6
action_67 (14) = happyShift action_7
action_67 (19) = happyShift action_8
action_67 (25) = happyShift action_9
action_67 (28) = happyShift action_10
action_67 (33) = happyShift action_11
action_67 (35) = happyShift action_12
action_67 (37) = happyShift action_13
action_67 (40) = happyShift action_14
action_67 (4) = happyGoto action_74
action_67 (5) = happyGoto action_2
action_67 (6) = happyGoto action_3
action_67 (7) = happyGoto action_16
action_67 (8) = happyGoto action_17
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (14) = happyShift action_73
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (20) = happyShift action_18
action_69 (21) = happyShift action_19
action_69 (22) = happyShift action_20
action_69 (24) = happyShift action_21
action_69 (25) = happyShift action_22
action_69 (26) = happyShift action_23
action_69 (27) = happyShift action_24
action_69 (28) = happyShift action_25
action_69 (29) = happyShift action_26
action_69 (34) = happyShift action_72
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (11) = happyShift action_4
action_70 (12) = happyShift action_5
action_70 (13) = happyShift action_6
action_70 (14) = happyShift action_7
action_70 (19) = happyShift action_8
action_70 (25) = happyShift action_9
action_70 (28) = happyShift action_10
action_70 (33) = happyShift action_11
action_70 (35) = happyShift action_12
action_70 (37) = happyShift action_13
action_70 (40) = happyShift action_14
action_70 (4) = happyGoto action_71
action_70 (5) = happyGoto action_2
action_70 (6) = happyGoto action_3
action_70 (7) = happyGoto action_16
action_70 (8) = happyGoto action_17
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (20) = happyShift action_18
action_71 (21) = happyShift action_19
action_71 (22) = happyShift action_20
action_71 (24) = happyShift action_21
action_71 (25) = happyShift action_22
action_71 (26) = happyShift action_23
action_71 (27) = happyShift action_24
action_71 (28) = happyShift action_25
action_71 (29) = happyShift action_26
action_71 _ = happyReduce_15

action_72 _ = happyReduce_14

action_73 (29) = happyShift action_75
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (20) = happyShift action_18
action_74 (21) = happyShift action_19
action_74 (22) = happyShift action_20
action_74 (24) = happyShift action_21
action_74 (25) = happyShift action_22
action_74 (26) = happyShift action_23
action_74 (27) = happyShift action_24
action_74 (28) = happyShift action_25
action_74 (29) = happyShift action_26
action_74 _ = happyReduce_16

action_75 (11) = happyShift action_4
action_75 (12) = happyShift action_5
action_75 (13) = happyShift action_6
action_75 (14) = happyShift action_7
action_75 (19) = happyShift action_8
action_75 (25) = happyShift action_9
action_75 (28) = happyShift action_10
action_75 (33) = happyShift action_11
action_75 (35) = happyShift action_12
action_75 (37) = happyShift action_13
action_75 (40) = happyShift action_14
action_75 (4) = happyGoto action_76
action_75 (5) = happyGoto action_2
action_75 (6) = happyGoto action_3
action_75 (7) = happyGoto action_16
action_75 (8) = happyGoto action_17
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (20) = happyShift action_18
action_76 (21) = happyFail []
action_76 (22) = happyFail []
action_76 (24) = happyShift action_21
action_76 (25) = happyShift action_22
action_76 (26) = happyShift action_23
action_76 (27) = happyShift action_24
action_76 (28) = happyFail []
action_76 (29) = happyFail []
action_76 _ = happyReduce_17

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  4 happyReduction_3
happyReduction_3 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  5 happyReduction_4
happyReduction_4 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn5
		 (ConstI happy_var_1 TInt
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  5 happyReduction_5
happyReduction_5 _
	(HappyTerminal (TokenInt happy_var_2))
	_
	 =  HappyAbsSyn5
		 (ConstI happy_var_2 Dyn
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  5 happyReduction_6
happyReduction_6 (HappyTerminal (TokenFloat happy_var_1))
	 =  HappyAbsSyn5
		 (ConstF happy_var_1 TFloat
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  5 happyReduction_7
happyReduction_7 _
	(HappyTerminal (TokenFloat happy_var_2))
	_
	 =  HappyAbsSyn5
		 (ConstF happy_var_2 Dyn
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  5 happyReduction_8
happyReduction_8 (HappyTerminal (TokenBool happy_var_1))
	 =  HappyAbsSyn5
		 (ConstB happy_var_1 TBool
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  6 happyReduction_9
happyReduction_9 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_2  6 happyReduction_10
happyReduction_10 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (Minus happy_var_2
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  6 happyReduction_11
happyReduction_11 _
	(HappyTerminal (TokenBool happy_var_2))
	_
	 =  HappyAbsSyn6
		 (ConstB happy_var_2 Dyn
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  6 happyReduction_12
happyReduction_12 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn6
		 (VarE happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  6 happyReduction_13
happyReduction_13 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (happy_var_2
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happyReduce 6 6 happyReduction_14
happyReduction_14 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (AppE happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_15 = happyReduce 6 6 happyReduction_15
happyReduction_15 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (If happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_16 = happyReduce 6 6 happyReduction_16
happyReduction_16 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (FuncE happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_17 = happyReduce 8 6 happyReduction_17
happyReduction_17 ((HappyAbsSyn4  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_6)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (ExprC happy_var_8 happy_var_4 happy_var_2 happy_var_6
	) `HappyStk` happyRest

happyReduce_18 = happySpecReduce_1  6 happyReduction_18
happyReduction_18 _
	 =  HappyAbsSyn6
		 (None
	)

happyReduce_19 = happySpecReduce_3  7 happyReduction_19
happyReduction_19 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn7
		 (Add happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  7 happyReduction_20
happyReduction_20 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn7
		 (Sub happy_var_1 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  7 happyReduction_21
happyReduction_21 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn7
		 (Mul happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  7 happyReduction_22
happyReduction_22 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn7
		 (Div happy_var_1 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  8 happyReduction_23
happyReduction_23 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn8
		 (LessEq happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  8 happyReduction_24
happyReduction_24 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn8
		 (BiggerEq happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  8 happyReduction_25
happyReduction_25 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn8
		 (Less happy_var_1 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  8 happyReduction_26
happyReduction_26 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn8
		 (Bigger happy_var_1 happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  8 happyReduction_27
happyReduction_27 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn8
		 (Eq happy_var_1 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  9 happyReduction_28
happyReduction_28 _
	 =  HappyAbsSyn9
		 (TInt
	)

happyReduce_29 = happySpecReduce_1  9 happyReduction_29
happyReduction_29 _
	 =  HappyAbsSyn9
		 (TFloat
	)

happyReduce_30 = happySpecReduce_1  9 happyReduction_30
happyReduction_30 _
	 =  HappyAbsSyn9
		 (TBool
	)

happyReduce_31 = happySpecReduce_1  9 happyReduction_31
happyReduction_31 _
	 =  HappyAbsSyn9
		 (Dyn
	)

happyReduce_32 = happySpecReduce_3  9 happyReduction_32
happyReduction_32 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (FuncT happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  10 happyReduction_33
happyReduction_33 _
	 =  HappyAbsSyn10
		 (TInt
	)

happyReduce_34 = happySpecReduce_1  10 happyReduction_34
happyReduction_34 _
	 =  HappyAbsSyn10
		 (TFloat
	)

happyReduce_35 = happySpecReduce_1  10 happyReduction_35
happyReduction_35 _
	 =  HappyAbsSyn10
		 (TBool
	)

happyReduce_36 = happySpecReduce_1  10 happyReduction_36
happyReduction_36 _
	 =  HappyAbsSyn10
		 (Dyn
	)

happyNewToken action sts stk [] =
	action 41 41 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenInt happy_dollar_dollar -> cont 11;
	TokenFloat happy_dollar_dollar -> cont 12;
	TokenBool happy_dollar_dollar -> cont 13;
	TokenVar happy_dollar_dollar -> cont 14;
	TokenStringInt -> cont 15;
	TokenStringFloat -> cont 16;
	TokenStringBool -> cont 17;
	TokenStringDyn -> cont 18;
	TokenLambda -> cont 19;
	TokenEq -> cont 20;
	TokenBiggerEq -> cont 21;
	TokenLessEq -> cont 22;
	TokenArrow -> cont 23;
	TokenAdd -> cont 24;
	TokenSub -> cont 25;
	TokenMul -> cont 26;
	TokenDiv -> cont 27;
	TokenLess -> cont 28;
	TokenBigger -> cont 29;
	TokenDot -> cont 30;
	TokenColon -> cont 31;
	TokenComma -> cont 32;
	TokenOBrack -> cont 33;
	TokenCBrack -> cont 34;
	TokenOSquare -> cont 35;
	TokenCSquare -> cont 36;
	TokenIf -> cont 37;
	TokenThen -> cont 38;
	TokenElse -> cont 39;
	TokenNone -> cont 40;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 41 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
calc tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token
    = TokenInt Int 
    | TokenFloat Float
    | TokenBool Bool 
    | TokenVar String
    | TokenEq
    | TokenBiggerEq
    | TokenLessEq
    | TokenLess
    | TokenBigger
    | TokenLambda
    | TokenArrow
    | TokenStringInt 
    | TokenStringFloat 
    | TokenStringDyn 
    | TokenStringBool
    | TokenAdd
    | TokenSub
    | TokenMul
    | TokenDiv
    | TokenDot
    | TokenColon
    | TokenComma
    | TokenOBrack
    | TokenCBrack
    | TokenOSquare
    | TokenCSquare
    | TokenIf
    | TokenThen
    | TokenElse
    | TokenNone
    deriving (Show, Eq)

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
      | isSpace c = lexer cs
      | isAlpha c = lexVar (c:cs)
      | isDigit c = lexNum (c:cs)
lexer ('=':'=':cs) = TokenEq : lexer cs
lexer ('<':'=':cs) = TokenLessEq : lexer cs
lexer ('>':'=':cs) = TokenBiggerEq : lexer cs
lexer ('<':cs) = TokenLess : lexer cs
lexer ('>':cs) = TokenBigger : lexer cs
lexer ('-':'>':cs) = TokenArrow : lexer cs
lexer ('+':cs) = TokenAdd : lexer cs
lexer ('-':cs) = TokenSub : lexer cs
lexer ('*':cs) = TokenMul : lexer cs
lexer ('/':cs) = TokenDiv : lexer cs
lexer ('\\':cs) = TokenLambda : lexer cs
lexer ('(':cs) = TokenOBrack : lexer cs
lexer (')':cs) = TokenCBrack : lexer cs
lexer ('[':cs) = TokenOSquare : lexer cs
lexer (']':cs) = TokenCSquare : lexer cs
lexer (':':cs) = TokenColon : lexer cs
lexer ('.':cs) = TokenDot : lexer cs
lexer (',':cs) = TokenComma : lexer cs
  
lexNum :: String -> [Token]
lexNum cs = let (num, rest) = span isDigit cs in if rest == [] then [TokenInt (read num)] else if (head rest) == '.' then let (first, second) = span isDigit (tail rest) in (TokenFloat (read (num ++ "." ++ first)) : lexer second) else TokenInt (read num) : lexer rest

lexVar :: String -> [Token]
lexVar cs =
    case span isAlpha cs of
    ("if",rest) -> TokenIf : lexer rest
    ("then",rest) -> TokenThen : lexer rest
    ("else",rest) -> TokenElse : lexer rest
    ("Int",rest) -> TokenStringInt : lexer rest
    ("Bool",rest) -> TokenStringBool : lexer rest
    ("Float",rest) -> TokenStringFloat : lexer rest
    ("Dyn",rest) -> TokenStringDyn : lexer rest
    ("none",rest) -> TokenNone : lexer rest
    (var,rest) -> TokenVar var : lexer rest

parse :: String -> Expr
parse s = calc(lexer s)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}







# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4











































{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}















{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc7199_0/ghc_2.h" #-}
































































































































































































{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}

{-# LINE 84 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 137 "templates/GenericTemplate.hs" #-}

{-# LINE 147 "templates/GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
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
     = case happyDrop (k - ((1) :: Int)) sts of
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





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 267 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 333 "templates/GenericTemplate.hs" #-}
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
