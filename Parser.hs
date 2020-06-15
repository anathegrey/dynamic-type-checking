{-# OPTIONS_GHC -w #-}
module Parser where
import CastData
import Data.Char
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.11

data HappyAbsSyn t4 t5 t6 t7 t8 t9
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,178) ([7680,33922,74,33310,19076,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,1536,0,0,480,0,7680,33922,74,32782,0,7680,33922,74,0,0,0,4060,0,0,0,0,0,0,33310,19076,7680,33922,74,33310,19076,7680,33922,74,33310,19076,7680,33922,74,33310,19076,7680,33922,74,33310,19076,0,4060,0,0,4096,0,0,4,0,1024,0,0,4,6,0,0,4060,1,12288,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,480,0,57344,1,0,480,0,0,32768,0,0,1024,0,0,4,0,0,0,0,0,0,0,7680,33922,74,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4060,32,0,0,0,0,0,33310,19076,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,64,0,4128,0,33310,19076,4096,0,0,56320,271,7680,33922,74,0,0,0,0,0,0,8,0,0,0,33310,19076,0,960,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_calc","Expr","Expr1","ExprArith","ExprBool","Type","Type1","int","float","bool","var","\"Int\"","\"Float\"","\"Bool\"","\"Dyn\"","'\\\\'","\"==\"","\">=\"","\"<=\"","\"->\"","'+'","'-'","'*'","'/'","'<'","'>'","'.'","':'","','","'('","')'","'['","']'","if","then","else","none","%eof"]
        bit_start = st * 40
        bit_end = (st + 1) * 40
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..39]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (10) = happyShift action_3
action_0 (11) = happyShift action_4
action_0 (12) = happyShift action_5
action_0 (13) = happyShift action_6
action_0 (18) = happyShift action_7
action_0 (24) = happyShift action_8
action_0 (27) = happyShift action_9
action_0 (32) = happyShift action_10
action_0 (34) = happyShift action_11
action_0 (36) = happyShift action_12
action_0 (39) = happyShift action_13
action_0 (4) = happyGoto action_14
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_15
action_0 (7) = happyGoto action_16
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (10) = happyShift action_3
action_1 (11) = happyShift action_4
action_1 (12) = happyShift action_5
action_1 (13) = happyShift action_6
action_1 (18) = happyShift action_7
action_1 (24) = happyShift action_8
action_1 (27) = happyShift action_9
action_1 (32) = happyShift action_10
action_1 (34) = happyShift action_11
action_1 (36) = happyShift action_12
action_1 (39) = happyShift action_13
action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 _ = happyReduce_4

action_4 _ = happyReduce_8

action_5 _ = happyReduce_12

action_6 _ = happyReduce_14

action_7 (13) = happyShift action_40
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (10) = happyShift action_38
action_8 (11) = happyShift action_39
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (14) = happyShift action_34
action_9 (15) = happyShift action_35
action_9 (16) = happyShift action_36
action_9 (17) = happyShift action_37
action_9 (8) = happyGoto action_33
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (10) = happyShift action_3
action_10 (11) = happyShift action_4
action_10 (12) = happyShift action_5
action_10 (13) = happyShift action_6
action_10 (18) = happyShift action_7
action_10 (24) = happyShift action_8
action_10 (27) = happyShift action_9
action_10 (32) = happyShift action_10
action_10 (34) = happyShift action_11
action_10 (36) = happyShift action_12
action_10 (39) = happyShift action_13
action_10 (4) = happyGoto action_32
action_10 (5) = happyGoto action_2
action_10 (6) = happyGoto action_15
action_10 (7) = happyGoto action_16
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (10) = happyShift action_28
action_11 (11) = happyShift action_29
action_11 (12) = happyShift action_30
action_11 (24) = happyShift action_31
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (10) = happyShift action_3
action_12 (11) = happyShift action_4
action_12 (12) = happyShift action_5
action_12 (13) = happyShift action_6
action_12 (18) = happyShift action_7
action_12 (24) = happyShift action_8
action_12 (27) = happyShift action_9
action_12 (32) = happyShift action_10
action_12 (34) = happyShift action_11
action_12 (36) = happyShift action_12
action_12 (39) = happyShift action_13
action_12 (4) = happyGoto action_26
action_12 (5) = happyGoto action_2
action_12 (6) = happyGoto action_15
action_12 (7) = happyGoto action_27
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_20

action_14 (19) = happyShift action_17
action_14 (20) = happyShift action_18
action_14 (21) = happyShift action_19
action_14 (23) = happyShift action_20
action_14 (24) = happyShift action_21
action_14 (25) = happyShift action_22
action_14 (26) = happyShift action_23
action_14 (27) = happyShift action_24
action_14 (28) = happyShift action_25
action_14 (40) = happyAccept
action_14 _ = happyFail (happyExpListPerState 14)

action_15 _ = happyReduce_2

action_16 _ = happyReduce_3

action_17 (10) = happyShift action_3
action_17 (11) = happyShift action_4
action_17 (12) = happyShift action_5
action_17 (13) = happyShift action_6
action_17 (18) = happyShift action_7
action_17 (24) = happyShift action_8
action_17 (27) = happyShift action_9
action_17 (32) = happyShift action_10
action_17 (34) = happyShift action_11
action_17 (36) = happyShift action_12
action_17 (39) = happyShift action_13
action_17 (5) = happyGoto action_59
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (10) = happyShift action_3
action_18 (11) = happyShift action_4
action_18 (12) = happyShift action_5
action_18 (13) = happyShift action_6
action_18 (18) = happyShift action_7
action_18 (24) = happyShift action_8
action_18 (27) = happyShift action_9
action_18 (32) = happyShift action_10
action_18 (34) = happyShift action_11
action_18 (36) = happyShift action_12
action_18 (39) = happyShift action_13
action_18 (5) = happyGoto action_58
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (10) = happyShift action_3
action_19 (11) = happyShift action_4
action_19 (12) = happyShift action_5
action_19 (13) = happyShift action_6
action_19 (18) = happyShift action_7
action_19 (24) = happyShift action_8
action_19 (27) = happyShift action_9
action_19 (32) = happyShift action_10
action_19 (34) = happyShift action_11
action_19 (36) = happyShift action_12
action_19 (39) = happyShift action_13
action_19 (5) = happyGoto action_57
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (10) = happyShift action_3
action_20 (11) = happyShift action_4
action_20 (12) = happyShift action_5
action_20 (13) = happyShift action_6
action_20 (18) = happyShift action_7
action_20 (24) = happyShift action_8
action_20 (27) = happyShift action_9
action_20 (32) = happyShift action_10
action_20 (34) = happyShift action_11
action_20 (36) = happyShift action_12
action_20 (39) = happyShift action_13
action_20 (5) = happyGoto action_56
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (10) = happyShift action_3
action_21 (11) = happyShift action_4
action_21 (12) = happyShift action_5
action_21 (13) = happyShift action_6
action_21 (18) = happyShift action_7
action_21 (24) = happyShift action_8
action_21 (27) = happyShift action_9
action_21 (32) = happyShift action_10
action_21 (34) = happyShift action_11
action_21 (36) = happyShift action_12
action_21 (39) = happyShift action_13
action_21 (5) = happyGoto action_55
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (10) = happyShift action_3
action_22 (11) = happyShift action_4
action_22 (12) = happyShift action_5
action_22 (13) = happyShift action_6
action_22 (18) = happyShift action_7
action_22 (24) = happyShift action_8
action_22 (27) = happyShift action_9
action_22 (32) = happyShift action_10
action_22 (34) = happyShift action_11
action_22 (36) = happyShift action_12
action_22 (39) = happyShift action_13
action_22 (5) = happyGoto action_54
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (10) = happyShift action_3
action_23 (11) = happyShift action_4
action_23 (12) = happyShift action_5
action_23 (13) = happyShift action_6
action_23 (18) = happyShift action_7
action_23 (24) = happyShift action_8
action_23 (27) = happyShift action_9
action_23 (32) = happyShift action_10
action_23 (34) = happyShift action_11
action_23 (36) = happyShift action_12
action_23 (39) = happyShift action_13
action_23 (5) = happyGoto action_53
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (10) = happyShift action_3
action_24 (11) = happyShift action_4
action_24 (12) = happyShift action_5
action_24 (13) = happyShift action_6
action_24 (18) = happyShift action_7
action_24 (24) = happyShift action_8
action_24 (27) = happyShift action_9
action_24 (32) = happyShift action_10
action_24 (34) = happyShift action_11
action_24 (36) = happyShift action_12
action_24 (39) = happyShift action_13
action_24 (5) = happyGoto action_52
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (10) = happyShift action_3
action_25 (11) = happyShift action_4
action_25 (12) = happyShift action_5
action_25 (13) = happyShift action_6
action_25 (18) = happyShift action_7
action_25 (24) = happyShift action_8
action_25 (27) = happyShift action_9
action_25 (32) = happyShift action_10
action_25 (34) = happyShift action_11
action_25 (36) = happyShift action_12
action_25 (39) = happyShift action_13
action_25 (5) = happyGoto action_51
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (19) = happyShift action_17
action_26 (20) = happyShift action_18
action_26 (21) = happyShift action_19
action_26 (23) = happyShift action_20
action_26 (24) = happyShift action_21
action_26 (25) = happyShift action_22
action_26 (26) = happyShift action_23
action_26 (27) = happyShift action_24
action_26 (28) = happyShift action_25
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (37) = happyShift action_50
action_27 _ = happyReduce_3

action_28 (35) = happyShift action_49
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (35) = happyShift action_48
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (35) = happyShift action_47
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (10) = happyShift action_45
action_31 (11) = happyShift action_46
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (19) = happyShift action_17
action_32 (20) = happyShift action_18
action_32 (21) = happyShift action_19
action_32 (23) = happyShift action_20
action_32 (24) = happyShift action_21
action_32 (25) = happyShift action_22
action_32 (26) = happyShift action_23
action_32 (27) = happyShift action_24
action_32 (28) = happyShift action_25
action_32 (33) = happyShift action_44
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (21) = happyShift action_42
action_33 (22) = happyShift action_43
action_33 _ = happyFail (happyExpListPerState 33)

action_34 _ = happyReduce_30

action_35 _ = happyReduce_31

action_36 _ = happyReduce_32

action_37 _ = happyReduce_33

action_38 _ = happyReduce_5

action_39 _ = happyReduce_9

action_40 (30) = happyShift action_41
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (14) = happyShift action_34
action_41 (15) = happyShift action_35
action_41 (16) = happyShift action_36
action_41 (17) = happyShift action_37
action_41 (8) = happyGoto action_70
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (14) = happyShift action_34
action_42 (15) = happyShift action_35
action_42 (16) = happyShift action_36
action_42 (17) = happyShift action_37
action_42 (8) = happyGoto action_69
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (14) = happyShift action_65
action_43 (15) = happyShift action_66
action_43 (16) = happyShift action_67
action_43 (17) = happyShift action_68
action_43 (9) = happyGoto action_64
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (32) = happyShift action_63
action_44 _ = happyReduce_15

action_45 (35) = happyShift action_62
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (35) = happyShift action_61
action_46 _ = happyFail (happyExpListPerState 46)

action_47 _ = happyReduce_13

action_48 _ = happyReduce_10

action_49 _ = happyReduce_6

action_50 (10) = happyShift action_3
action_50 (11) = happyShift action_4
action_50 (12) = happyShift action_5
action_50 (13) = happyShift action_6
action_50 (18) = happyShift action_7
action_50 (24) = happyShift action_8
action_50 (27) = happyShift action_9
action_50 (32) = happyShift action_10
action_50 (34) = happyShift action_11
action_50 (36) = happyShift action_12
action_50 (39) = happyShift action_13
action_50 (4) = happyGoto action_60
action_50 (5) = happyGoto action_2
action_50 (6) = happyGoto action_15
action_50 (7) = happyGoto action_16
action_50 _ = happyFail (happyExpListPerState 50)

action_51 _ = happyReduce_28

action_52 _ = happyReduce_27

action_53 _ = happyReduce_24

action_54 _ = happyReduce_23

action_55 _ = happyReduce_22

action_56 _ = happyReduce_21

action_57 _ = happyReduce_25

action_58 _ = happyReduce_26

action_59 _ = happyReduce_29

action_60 (19) = happyShift action_17
action_60 (20) = happyShift action_18
action_60 (21) = happyShift action_19
action_60 (23) = happyShift action_20
action_60 (24) = happyShift action_21
action_60 (25) = happyShift action_22
action_60 (26) = happyShift action_23
action_60 (27) = happyShift action_24
action_60 (28) = happyShift action_25
action_60 (38) = happyShift action_74
action_60 _ = happyFail (happyExpListPerState 60)

action_61 _ = happyReduce_11

action_62 _ = happyReduce_7

action_63 (10) = happyShift action_3
action_63 (11) = happyShift action_4
action_63 (12) = happyShift action_5
action_63 (13) = happyShift action_6
action_63 (18) = happyShift action_7
action_63 (24) = happyShift action_8
action_63 (27) = happyShift action_9
action_63 (32) = happyShift action_10
action_63 (34) = happyShift action_11
action_63 (36) = happyShift action_12
action_63 (39) = happyShift action_13
action_63 (4) = happyGoto action_73
action_63 (5) = happyGoto action_2
action_63 (6) = happyGoto action_15
action_63 (7) = happyGoto action_16
action_63 _ = happyFail (happyExpListPerState 63)

action_64 _ = happyReduce_34

action_65 _ = happyReduce_35

action_66 _ = happyReduce_36

action_67 _ = happyReduce_37

action_68 _ = happyReduce_38

action_69 (22) = happyShift action_43
action_69 (31) = happyShift action_72
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (22) = happyShift action_43
action_70 (29) = happyShift action_71
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (10) = happyShift action_3
action_71 (11) = happyShift action_4
action_71 (12) = happyShift action_5
action_71 (13) = happyShift action_6
action_71 (18) = happyShift action_7
action_71 (24) = happyShift action_8
action_71 (27) = happyShift action_9
action_71 (32) = happyShift action_10
action_71 (34) = happyShift action_11
action_71 (36) = happyShift action_12
action_71 (39) = happyShift action_13
action_71 (4) = happyGoto action_78
action_71 (5) = happyGoto action_2
action_71 (6) = happyGoto action_15
action_71 (7) = happyGoto action_16
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (13) = happyShift action_77
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (19) = happyShift action_17
action_73 (20) = happyShift action_18
action_73 (21) = happyShift action_19
action_73 (23) = happyShift action_20
action_73 (24) = happyShift action_21
action_73 (25) = happyShift action_22
action_73 (26) = happyShift action_23
action_73 (27) = happyShift action_24
action_73 (28) = happyShift action_25
action_73 (33) = happyShift action_76
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (10) = happyShift action_3
action_74 (11) = happyShift action_4
action_74 (12) = happyShift action_5
action_74 (13) = happyShift action_6
action_74 (18) = happyShift action_7
action_74 (24) = happyShift action_8
action_74 (27) = happyShift action_9
action_74 (32) = happyShift action_10
action_74 (34) = happyShift action_11
action_74 (36) = happyShift action_12
action_74 (39) = happyShift action_13
action_74 (4) = happyGoto action_75
action_74 (5) = happyGoto action_2
action_74 (6) = happyGoto action_15
action_74 (7) = happyGoto action_16
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (19) = happyShift action_17
action_75 (20) = happyShift action_18
action_75 (21) = happyShift action_19
action_75 (23) = happyShift action_20
action_75 (24) = happyShift action_21
action_75 (25) = happyShift action_22
action_75 (26) = happyShift action_23
action_75 (27) = happyShift action_24
action_75 (28) = happyShift action_25
action_75 _ = happyReduce_17

action_76 _ = happyReduce_16

action_77 (28) = happyShift action_79
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (19) = happyShift action_17
action_78 (20) = happyShift action_18
action_78 (21) = happyShift action_19
action_78 (23) = happyShift action_20
action_78 (24) = happyShift action_21
action_78 (25) = happyShift action_22
action_78 (26) = happyShift action_23
action_78 (27) = happyShift action_24
action_78 (28) = happyShift action_25
action_78 _ = happyReduce_18

action_79 (10) = happyShift action_3
action_79 (11) = happyShift action_4
action_79 (12) = happyShift action_5
action_79 (13) = happyShift action_6
action_79 (18) = happyShift action_7
action_79 (24) = happyShift action_8
action_79 (27) = happyShift action_9
action_79 (32) = happyShift action_10
action_79 (34) = happyShift action_11
action_79 (36) = happyShift action_12
action_79 (39) = happyShift action_13
action_79 (4) = happyGoto action_80
action_79 (5) = happyGoto action_2
action_79 (6) = happyGoto action_15
action_79 (7) = happyGoto action_16
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (19) = happyShift action_17
action_80 (20) = happyFail []
action_80 (21) = happyFail []
action_80 (23) = happyShift action_20
action_80 (24) = happyShift action_21
action_80 (25) = happyShift action_22
action_80 (26) = happyShift action_23
action_80 (27) = happyFail []
action_80 (28) = happyFail []
action_80 _ = happyReduce_19

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  4 happyReduction_3
happyReduction_3 (HappyAbsSyn7  happy_var_1)
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

happyReduce_5 = happySpecReduce_2  5 happyReduction_5
happyReduction_5 (HappyTerminal (TokenInt happy_var_2))
	_
	 =  HappyAbsSyn5
		 (Minus (ConstI happy_var_2 TInt)
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  5 happyReduction_6
happyReduction_6 _
	(HappyTerminal (TokenInt happy_var_2))
	_
	 =  HappyAbsSyn5
		 (ConstI happy_var_2 Dyn
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happyReduce 4 5 happyReduction_7
happyReduction_7 (_ `HappyStk`
	(HappyTerminal (TokenInt happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Minus (ConstI happy_var_3 Dyn)
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_1  5 happyReduction_8
happyReduction_8 (HappyTerminal (TokenFloat happy_var_1))
	 =  HappyAbsSyn5
		 (ConstF happy_var_1 TFloat
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  5 happyReduction_9
happyReduction_9 (HappyTerminal (TokenFloat happy_var_2))
	_
	 =  HappyAbsSyn5
		 (Minus (ConstF happy_var_2 TFloat)
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  5 happyReduction_10
happyReduction_10 _
	(HappyTerminal (TokenFloat happy_var_2))
	_
	 =  HappyAbsSyn5
		 (ConstF happy_var_2 Dyn
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happyReduce 4 5 happyReduction_11
happyReduction_11 (_ `HappyStk`
	(HappyTerminal (TokenFloat happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Minus (ConstF happy_var_3 Dyn)
	) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_1  5 happyReduction_12
happyReduction_12 (HappyTerminal (TokenBool happy_var_1))
	 =  HappyAbsSyn5
		 (ConstB happy_var_1 TBool
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  5 happyReduction_13
happyReduction_13 _
	(HappyTerminal (TokenBool happy_var_2))
	_
	 =  HappyAbsSyn5
		 (ConstB happy_var_2 Dyn
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  5 happyReduction_14
happyReduction_14 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn5
		 (VarE happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  5 happyReduction_15
happyReduction_15 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happyReduce 6 5 happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (AppE happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_17 = happyReduce 6 5 happyReduction_17
happyReduction_17 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (If happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_18 = happyReduce 6 5 happyReduction_18
happyReduction_18 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (FuncE happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_19 = happyReduce 8 5 happyReduction_19
happyReduction_19 ((HappyAbsSyn4  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_6)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (ExprC happy_var_8 happy_var_4 happy_var_2 happy_var_6
	) `HappyStk` happyRest

happyReduce_20 = happySpecReduce_1  5 happyReduction_20
happyReduction_20 _
	 =  HappyAbsSyn5
		 (None
	)

happyReduce_21 = happySpecReduce_3  6 happyReduction_21
happyReduction_21 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn6
		 (Add happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  6 happyReduction_22
happyReduction_22 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn6
		 (Sub happy_var_1 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  6 happyReduction_23
happyReduction_23 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn6
		 (Mul happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  6 happyReduction_24
happyReduction_24 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn6
		 (Div happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  7 happyReduction_25
happyReduction_25 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn7
		 (LessEq happy_var_1 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  7 happyReduction_26
happyReduction_26 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn7
		 (BiggerEq happy_var_1 happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  7 happyReduction_27
happyReduction_27 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn7
		 (Less happy_var_1 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  7 happyReduction_28
happyReduction_28 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn7
		 (Bigger happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  7 happyReduction_29
happyReduction_29 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn7
		 (Eq happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  8 happyReduction_30
happyReduction_30 _
	 =  HappyAbsSyn8
		 (TInt
	)

happyReduce_31 = happySpecReduce_1  8 happyReduction_31
happyReduction_31 _
	 =  HappyAbsSyn8
		 (TFloat
	)

happyReduce_32 = happySpecReduce_1  8 happyReduction_32
happyReduction_32 _
	 =  HappyAbsSyn8
		 (TBool
	)

happyReduce_33 = happySpecReduce_1  8 happyReduction_33
happyReduction_33 _
	 =  HappyAbsSyn8
		 (Dyn
	)

happyReduce_34 = happySpecReduce_3  8 happyReduction_34
happyReduction_34 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (FuncT happy_var_1 happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  9 happyReduction_35
happyReduction_35 _
	 =  HappyAbsSyn9
		 (TInt
	)

happyReduce_36 = happySpecReduce_1  9 happyReduction_36
happyReduction_36 _
	 =  HappyAbsSyn9
		 (TFloat
	)

happyReduce_37 = happySpecReduce_1  9 happyReduction_37
happyReduction_37 _
	 =  HappyAbsSyn9
		 (TBool
	)

happyReduce_38 = happySpecReduce_1  9 happyReduction_38
happyReduction_38 _
	 =  HappyAbsSyn9
		 (Dyn
	)

happyNewToken action sts stk [] =
	action 40 40 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenInt happy_dollar_dollar -> cont 10;
	TokenFloat happy_dollar_dollar -> cont 11;
	TokenBool happy_dollar_dollar -> cont 12;
	TokenVar happy_dollar_dollar -> cont 13;
	TokenStringInt -> cont 14;
	TokenStringFloat -> cont 15;
	TokenStringBool -> cont 16;
	TokenStringDyn -> cont 17;
	TokenLambda -> cont 18;
	TokenEq -> cont 19;
	TokenBiggerEq -> cont 20;
	TokenLessEq -> cont 21;
	TokenArrow -> cont 22;
	TokenAdd -> cont 23;
	TokenSub -> cont 24;
	TokenMul -> cont 25;
	TokenDiv -> cont 26;
	TokenLess -> cont 27;
	TokenBigger -> cont 28;
	TokenDot -> cont 29;
	TokenColon -> cont 30;
	TokenComma -> cont 31;
	TokenOBrack -> cont 32;
	TokenCBrack -> cont 33;
	TokenOSquare -> cont 34;
	TokenCSquare -> cont 35;
	TokenIf -> cont 36;
	TokenThen -> cont 37;
	TokenElse -> cont 38;
	TokenNone -> cont 39;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 40 tk tks = happyError' (tks, explist)
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
