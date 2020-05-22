{-# OPTIONS_GHC -w #-}
module Main where
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
happyExpList = Happy_Data_Array.listArray (0,159) ([7680,2052,149,2108,10768,1,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,960,0,15360,4104,298,56,0,61440,16416,1192,0,0,0,63232,3,0,0,0,0,0,7680,2052,149,2108,10768,30721,8208,596,8432,43072,57348,32833,2384,33728,41216,32786,263,9538,3840,33794,74,1054,38152,0,16240,0,0,32768,0,0,64,0,32768,0,0,256,0,34798,0,12288,0,0,0,0,0,0,0,0,0,0,0,0,0,2,30720,0,0,240,0,57344,1,0,0,256,0,0,0,0,0,0,0,0,16864,20608,9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,3840,33794,74,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,16,0,4112,0,527,19076,8192,0,0,28672,1087,30720,8208,596,0,0,0,0,0,0,512,0,0,0,3840,33794,74,32768,7,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_calc","Expr","Expr1","ExprArith","ExprBool","Type","Type1","int","float","bool","var","label","\"Int\"","\"Float\"","\"Bool\"","\"Dyn\"","'\\\\'","\"==\"","\">=\"","\"<=\"","\"->\"","'+'","'-'","'*'","'/'","'<'","'>'","'.'","':'","','","'('","')'","'['","']'","\"if\"","\"then\"","\"else\"","\"none\"","%eof"]
        bit_start = st * 41
        bit_end = (st + 1) * 41
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..40]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (10) = happyShift action_3
action_0 (11) = happyShift action_4
action_0 (12) = happyShift action_5
action_0 (13) = happyShift action_6
action_0 (19) = happyShift action_7
action_0 (28) = happyShift action_8
action_0 (33) = happyShift action_9
action_0 (35) = happyShift action_10
action_0 (37) = happyShift action_11
action_0 (40) = happyShift action_12
action_0 (4) = happyGoto action_13
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_14
action_0 (7) = happyGoto action_15
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (10) = happyShift action_3
action_1 (11) = happyShift action_4
action_1 (12) = happyShift action_5
action_1 (13) = happyShift action_6
action_1 (19) = happyShift action_7
action_1 (28) = happyShift action_8
action_1 (33) = happyShift action_9
action_1 (35) = happyShift action_10
action_1 (37) = happyShift action_11
action_1 (40) = happyShift action_12
action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 _ = happyReduce_4

action_4 _ = happyReduce_6

action_5 _ = happyReduce_8

action_6 _ = happyReduce_10

action_7 (13) = happyShift action_36
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (15) = happyShift action_32
action_8 (16) = happyShift action_33
action_8 (17) = happyShift action_34
action_8 (18) = happyShift action_35
action_8 (8) = happyGoto action_31
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (10) = happyShift action_3
action_9 (11) = happyShift action_4
action_9 (12) = happyShift action_5
action_9 (13) = happyShift action_6
action_9 (19) = happyShift action_7
action_9 (28) = happyShift action_8
action_9 (33) = happyShift action_9
action_9 (35) = happyShift action_10
action_9 (37) = happyShift action_11
action_9 (40) = happyShift action_12
action_9 (4) = happyGoto action_30
action_9 (5) = happyGoto action_2
action_9 (6) = happyGoto action_14
action_9 (7) = happyGoto action_15
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (10) = happyShift action_27
action_10 (11) = happyShift action_28
action_10 (12) = happyShift action_29
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (10) = happyShift action_3
action_11 (11) = happyShift action_4
action_11 (12) = happyShift action_5
action_11 (13) = happyShift action_6
action_11 (19) = happyShift action_7
action_11 (28) = happyShift action_8
action_11 (33) = happyShift action_9
action_11 (35) = happyShift action_10
action_11 (37) = happyShift action_11
action_11 (40) = happyShift action_12
action_11 (4) = happyGoto action_25
action_11 (5) = happyGoto action_2
action_11 (6) = happyGoto action_14
action_11 (7) = happyGoto action_26
action_11 _ = happyFail (happyExpListPerState 11)

action_12 _ = happyReduce_16

action_13 (20) = happyShift action_16
action_13 (21) = happyShift action_17
action_13 (22) = happyShift action_18
action_13 (24) = happyShift action_19
action_13 (25) = happyShift action_20
action_13 (26) = happyShift action_21
action_13 (27) = happyShift action_22
action_13 (28) = happyShift action_23
action_13 (29) = happyShift action_24
action_13 (41) = happyAccept
action_13 _ = happyFail (happyExpListPerState 13)

action_14 _ = happyReduce_2

action_15 _ = happyReduce_3

action_16 (10) = happyShift action_3
action_16 (11) = happyShift action_4
action_16 (12) = happyShift action_5
action_16 (13) = happyShift action_6
action_16 (19) = happyShift action_7
action_16 (28) = happyShift action_8
action_16 (33) = happyShift action_9
action_16 (35) = happyShift action_10
action_16 (37) = happyShift action_11
action_16 (40) = happyShift action_12
action_16 (5) = happyGoto action_53
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (10) = happyShift action_3
action_17 (11) = happyShift action_4
action_17 (12) = happyShift action_5
action_17 (13) = happyShift action_6
action_17 (19) = happyShift action_7
action_17 (28) = happyShift action_8
action_17 (33) = happyShift action_9
action_17 (35) = happyShift action_10
action_17 (37) = happyShift action_11
action_17 (40) = happyShift action_12
action_17 (5) = happyGoto action_52
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (10) = happyShift action_3
action_18 (11) = happyShift action_4
action_18 (12) = happyShift action_5
action_18 (13) = happyShift action_6
action_18 (19) = happyShift action_7
action_18 (28) = happyShift action_8
action_18 (33) = happyShift action_9
action_18 (35) = happyShift action_10
action_18 (37) = happyShift action_11
action_18 (40) = happyShift action_12
action_18 (5) = happyGoto action_51
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (10) = happyShift action_3
action_19 (11) = happyShift action_4
action_19 (12) = happyShift action_5
action_19 (13) = happyShift action_6
action_19 (19) = happyShift action_7
action_19 (28) = happyShift action_8
action_19 (33) = happyShift action_9
action_19 (35) = happyShift action_10
action_19 (37) = happyShift action_11
action_19 (40) = happyShift action_12
action_19 (5) = happyGoto action_50
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (10) = happyShift action_3
action_20 (11) = happyShift action_4
action_20 (12) = happyShift action_5
action_20 (13) = happyShift action_6
action_20 (19) = happyShift action_7
action_20 (28) = happyShift action_8
action_20 (33) = happyShift action_9
action_20 (35) = happyShift action_10
action_20 (37) = happyShift action_11
action_20 (40) = happyShift action_12
action_20 (5) = happyGoto action_49
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (10) = happyShift action_3
action_21 (11) = happyShift action_4
action_21 (12) = happyShift action_5
action_21 (13) = happyShift action_6
action_21 (19) = happyShift action_7
action_21 (28) = happyShift action_8
action_21 (33) = happyShift action_9
action_21 (35) = happyShift action_10
action_21 (37) = happyShift action_11
action_21 (40) = happyShift action_12
action_21 (5) = happyGoto action_48
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (10) = happyShift action_3
action_22 (11) = happyShift action_4
action_22 (12) = happyShift action_5
action_22 (13) = happyShift action_6
action_22 (19) = happyShift action_7
action_22 (28) = happyShift action_8
action_22 (33) = happyShift action_9
action_22 (35) = happyShift action_10
action_22 (37) = happyShift action_11
action_22 (40) = happyShift action_12
action_22 (5) = happyGoto action_47
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (10) = happyShift action_3
action_23 (11) = happyShift action_4
action_23 (12) = happyShift action_5
action_23 (13) = happyShift action_6
action_23 (19) = happyShift action_7
action_23 (28) = happyShift action_8
action_23 (33) = happyShift action_9
action_23 (35) = happyShift action_10
action_23 (37) = happyShift action_11
action_23 (40) = happyShift action_12
action_23 (5) = happyGoto action_46
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (10) = happyShift action_3
action_24 (11) = happyShift action_4
action_24 (12) = happyShift action_5
action_24 (13) = happyShift action_6
action_24 (19) = happyShift action_7
action_24 (28) = happyShift action_8
action_24 (33) = happyShift action_9
action_24 (35) = happyShift action_10
action_24 (37) = happyShift action_11
action_24 (40) = happyShift action_12
action_24 (5) = happyGoto action_45
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (20) = happyShift action_16
action_25 (21) = happyShift action_17
action_25 (22) = happyShift action_18
action_25 (24) = happyShift action_19
action_25 (25) = happyShift action_20
action_25 (26) = happyShift action_21
action_25 (27) = happyShift action_22
action_25 (28) = happyShift action_23
action_25 (29) = happyShift action_24
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (38) = happyShift action_44
action_26 _ = happyReduce_3

action_27 (36) = happyShift action_43
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (36) = happyShift action_42
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (36) = happyShift action_41
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (20) = happyShift action_16
action_30 (21) = happyShift action_17
action_30 (22) = happyShift action_18
action_30 (24) = happyShift action_19
action_30 (25) = happyShift action_20
action_30 (26) = happyShift action_21
action_30 (27) = happyShift action_22
action_30 (28) = happyShift action_23
action_30 (29) = happyShift action_24
action_30 (34) = happyShift action_40
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (22) = happyShift action_38
action_31 (23) = happyShift action_39
action_31 _ = happyFail (happyExpListPerState 31)

action_32 _ = happyReduce_26

action_33 _ = happyReduce_27

action_34 _ = happyReduce_28

action_35 _ = happyReduce_29

action_36 (30) = happyShift action_37
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (15) = happyShift action_32
action_37 (16) = happyShift action_33
action_37 (17) = happyShift action_34
action_37 (18) = happyShift action_35
action_37 (8) = happyGoto action_62
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (15) = happyShift action_32
action_38 (16) = happyShift action_33
action_38 (17) = happyShift action_34
action_38 (18) = happyShift action_35
action_38 (8) = happyGoto action_61
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (15) = happyShift action_57
action_39 (16) = happyShift action_58
action_39 (17) = happyShift action_59
action_39 (18) = happyShift action_60
action_39 (9) = happyGoto action_56
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (33) = happyShift action_55
action_40 _ = happyReduce_11

action_41 _ = happyReduce_9

action_42 _ = happyReduce_7

action_43 _ = happyReduce_5

action_44 (10) = happyShift action_3
action_44 (11) = happyShift action_4
action_44 (12) = happyShift action_5
action_44 (13) = happyShift action_6
action_44 (19) = happyShift action_7
action_44 (28) = happyShift action_8
action_44 (33) = happyShift action_9
action_44 (35) = happyShift action_10
action_44 (37) = happyShift action_11
action_44 (40) = happyShift action_12
action_44 (5) = happyGoto action_54
action_44 _ = happyFail (happyExpListPerState 44)

action_45 _ = happyReduce_24

action_46 _ = happyReduce_23

action_47 _ = happyReduce_20

action_48 _ = happyReduce_19

action_49 _ = happyReduce_18

action_50 _ = happyReduce_17

action_51 _ = happyReduce_21

action_52 _ = happyReduce_22

action_53 _ = happyReduce_25

action_54 (39) = happyShift action_66
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (10) = happyShift action_3
action_55 (11) = happyShift action_4
action_55 (12) = happyShift action_5
action_55 (13) = happyShift action_6
action_55 (19) = happyShift action_7
action_55 (28) = happyShift action_8
action_55 (33) = happyShift action_9
action_55 (35) = happyShift action_10
action_55 (37) = happyShift action_11
action_55 (40) = happyShift action_12
action_55 (4) = happyGoto action_65
action_55 (5) = happyGoto action_2
action_55 (6) = happyGoto action_14
action_55 (7) = happyGoto action_15
action_55 _ = happyFail (happyExpListPerState 55)

action_56 _ = happyReduce_30

action_57 _ = happyReduce_31

action_58 _ = happyReduce_32

action_59 _ = happyReduce_33

action_60 _ = happyReduce_34

action_61 (23) = happyShift action_39
action_61 (32) = happyShift action_64
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (23) = happyShift action_39
action_62 (31) = happyShift action_63
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (10) = happyShift action_3
action_63 (11) = happyShift action_4
action_63 (12) = happyShift action_5
action_63 (13) = happyShift action_6
action_63 (19) = happyShift action_7
action_63 (28) = happyShift action_8
action_63 (33) = happyShift action_9
action_63 (35) = happyShift action_10
action_63 (37) = happyShift action_11
action_63 (40) = happyShift action_12
action_63 (4) = happyGoto action_70
action_63 (5) = happyGoto action_2
action_63 (6) = happyGoto action_14
action_63 (7) = happyGoto action_15
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (14) = happyShift action_69
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (20) = happyShift action_16
action_65 (21) = happyShift action_17
action_65 (22) = happyShift action_18
action_65 (24) = happyShift action_19
action_65 (25) = happyShift action_20
action_65 (26) = happyShift action_21
action_65 (27) = happyShift action_22
action_65 (28) = happyShift action_23
action_65 (29) = happyShift action_24
action_65 (34) = happyShift action_68
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (10) = happyShift action_3
action_66 (11) = happyShift action_4
action_66 (12) = happyShift action_5
action_66 (13) = happyShift action_6
action_66 (19) = happyShift action_7
action_66 (28) = happyShift action_8
action_66 (33) = happyShift action_9
action_66 (35) = happyShift action_10
action_66 (37) = happyShift action_11
action_66 (40) = happyShift action_12
action_66 (5) = happyGoto action_67
action_66 _ = happyFail (happyExpListPerState 66)

action_67 _ = happyReduce_13

action_68 _ = happyReduce_12

action_69 (29) = happyShift action_71
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (20) = happyShift action_16
action_70 (21) = happyShift action_17
action_70 (22) = happyShift action_18
action_70 (24) = happyShift action_19
action_70 (25) = happyShift action_20
action_70 (26) = happyShift action_21
action_70 (27) = happyShift action_22
action_70 (28) = happyShift action_23
action_70 (29) = happyShift action_24
action_70 _ = happyReduce_14

action_71 (10) = happyShift action_3
action_71 (11) = happyShift action_4
action_71 (12) = happyShift action_5
action_71 (13) = happyShift action_6
action_71 (19) = happyShift action_7
action_71 (28) = happyShift action_8
action_71 (33) = happyShift action_9
action_71 (35) = happyShift action_10
action_71 (37) = happyShift action_11
action_71 (40) = happyShift action_12
action_71 (4) = happyGoto action_72
action_71 (5) = happyGoto action_2
action_71 (6) = happyGoto action_14
action_71 (7) = happyGoto action_15
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (20) = happyShift action_16
action_72 (21) = happyFail []
action_72 (22) = happyFail []
action_72 (24) = happyShift action_19
action_72 (25) = happyShift action_20
action_72 (26) = happyShift action_21
action_72 (27) = happyShift action_22
action_72 (28) = happyFail []
action_72 (29) = happyFail []
action_72 _ = happyReduce_15

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
		 (ConstI happy_var_1 Int
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
		 (ConstF happy_var_1 Float
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
		 (ConstB happy_var_1 Bool
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  5 happyReduction_9
happyReduction_9 _
	(HappyTerminal (TokenBool happy_var_2))
	_
	 =  HappyAbsSyn5
		 (ConstB happy_var_2 Dyn
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  5 happyReduction_10
happyReduction_10 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn5
		 (VarE happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  5 happyReduction_11
happyReduction_11 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happyReduce 6 5 happyReduction_12
happyReduction_12 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (AppE happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_13 = happyReduce 6 5 happyReduction_13
happyReduction_13 ((HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (If happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 6 5 happyReduction_14
happyReduction_14 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (FuncE happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_15 = happyReduce 8 5 happyReduction_15
happyReduction_15 ((HappyAbsSyn4  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenLabel happy_var_6)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (ExprC happy_var_8 happy_var_4 happy_var_2 happy_var_6
	) `HappyStk` happyRest

happyReduce_16 = happySpecReduce_1  5 happyReduction_16
happyReduction_16 _
	 =  HappyAbsSyn5
		 (None
	)

happyReduce_17 = happySpecReduce_3  6 happyReduction_17
happyReduction_17 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn6
		 (Add happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  6 happyReduction_18
happyReduction_18 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn6
		 (Sub happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  6 happyReduction_19
happyReduction_19 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn6
		 (Mul happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  6 happyReduction_20
happyReduction_20 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn6
		 (Div happy_var_1 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  7 happyReduction_21
happyReduction_21 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn7
		 (LessEq happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  7 happyReduction_22
happyReduction_22 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn7
		 (BiggerEq happy_var_1 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  7 happyReduction_23
happyReduction_23 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn7
		 (Less happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  7 happyReduction_24
happyReduction_24 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn7
		 (Bigger happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  7 happyReduction_25
happyReduction_25 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn7
		 (Eq happy_var_1 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  8 happyReduction_26
happyReduction_26 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  8 happyReduction_27
happyReduction_27 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  8 happyReduction_28
happyReduction_28 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  8 happyReduction_29
happyReduction_29 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  8 happyReduction_30
happyReduction_30 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (FuncT happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  9 happyReduction_31
happyReduction_31 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  9 happyReduction_32
happyReduction_32 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  9 happyReduction_33
happyReduction_33 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  9 happyReduction_34
happyReduction_34 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 41 41 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenInt happy_dollar_dollar -> cont 10;
	TokenFloat happy_dollar_dollar -> cont 11;
	TokenBool happy_dollar_dollar -> cont 12;
	TokenVar happy_dollar_dollar -> cont 13;
	TokenLabel happy_dollar_dollar -> cont 14;
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
    | TokenLabel String
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
lexer ("==":cs) = TokenEq : lexer cs
lexer ("<=":cs) = TokenLessEq : lexer cs
lexer (">=":cs) = TokenBiggerEq : lexer cs
lexer ('<':cs) = TokenLess : lexer cs
lexer ('>':cs) = TokenBigger : lexer cs
lexer ("->":cs) = TokenArrow : lexer cs
lexer ('+':cs) = TokenAdd : lexer cs
lexer ('-':cs) = TokenSub : lexer cs
lexer ('*':cs) = TokenMul : lexer cs
lexer ('/':cs) = TokenDiv : lexer cs
lexer (':':cs) = TokenColon : lexer cs
lexer ('.':cs) = TokenDot : lexer cs
lexer (',':cs) = TokenComma : lexer cs
lexer ('\\':cs) = TokenLambda : lexer cs
lexer ('(':cs) = TokenOBrack : lexer cs
lexer (')':cs) = TokenCBrack : lexer cs
lexer ('[':cs) = TokenOSquare : lexer cs
lexer (']':cs) = TokenCSquare : lexer cs

lexNum cs = TokenInt (read num) : lexer rest
      where (num,rest) = span isDigit cs
  
lexVar cs =
    case span isAlpha cs of
    ("if",rest) -> TokenIf : lexer rest
    ("then",rest)  -> TokenThen : lexer rest
    ("else",rest) -> TokenElse : lexer rest
    (var,rest)   -> TokenVar var : lexer rest
    (label,rest) -> TokenLabel label : lexer rest
    ("Int",rest) -> TokenStringInt : lexer rest
    ("Bool",rest) -> TokenStringBool : lexer rest
    ("Float",rest) -> TokenStringFloat : lexer rest
    ("Dyn",rest) -> TokenStringDyn : lexer rest
    ("none",rest) -> TokenNone : lexer rest									     										     
    
main = getContents >>= print . calc . lexer
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
