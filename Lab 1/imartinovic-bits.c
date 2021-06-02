/* 
 * CS:APP Data Lab 
 * 
 * <Ivan Martinovic - imartinovic>
 * 
 * bits.c - Source file with your solutions to the Lab.
 *          This is the file you will hand in to your instructor.
 *
 * WARNING: Do not include the <stdio.h> header; it confuses the dlc
 * compiler. You can still use printf for debugging without including
 * <stdio.h>, although you might get a compiler warning. In general,
 * it's not good practice to ignore compiler warnings, but in this
 * case it's OK.  
 */

#if 0
/*
 * Instructions to Students:
 *
 * STEP 1: Read the following instructions carefully.
 */

You will provide your solution to the Data Lab by
editing the collection of functions in this source file.

INTEGER CODING RULES:
 
  Replace the "return" statement in each function with one
  or more lines of C code that implements the function. Your code 
  must conform to the following style:
 
  int Funct(arg1, arg2, ...) {
      /* brief description of how your implementation works */
      int var1 = Expr1;
      ...
      int varM = ExprM;

      varJ = ExprJ;
      ...
      varN = ExprN;
      return ExprR;
  }

  Each "Expr" is an expression using ONLY the following:
  1. Integer constants 0 through 255 (0xFF), inclusive. You are
      not allowed to use big constants such as 0xffffffff.
  2. Function arguments and local variables (no global variables).
  3. Unary integer operations ! ~
  4. Binary integer operations & ^ | + << >>
    
  Some of the problems restrict the set of allowed operators even further.
  Each "Expr" may consist of multiple operators. You are not restricted to
  one operator per line.

  You are expressly forbidden to:
  1. Use any control constructs such as if, do, while, for, switch, etc.
  2. Define or use any macros.
  3. Define any additional functions in this file.
  4. Call any functions.
  5. Use any other operations, such as &&, ||, -, or ?:
  6. Use any form of casting.
  7. Use any data type other than int.  This implies that you
     cannot use arrays, structs, or unions.

 
  You may assume that your machine:
  1. Uses 2s complement, 32-bit representations of integers.
  2. Performs right shifts arithmetically.
  3. Has unpredictable behavior when shifting an integer by more
     than the word size.

EXAMPLES OF ACCEPTABLE CODING STYLE:
  /*
   * pow2plus1 - returns 2^x + 1, where 0 <= x <= 31
   */
  int pow2plus1(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     return (1 << x) + 1;
  }

  /*
   * pow2plus4 - returns 2^x + 4, where 0 <= x <= 31
   */
  int pow2plus4(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     int result = (1 << x);
     result += 4;
     return result;
  }

FLOATING POINT CODING RULES

For the problems that require you to implent floating-point operations,
the coding rules are less strict.  You are allowed to use looping and
conditional control.  You are allowed to use both ints and unsigneds.
You can use arbitrary integer and unsigned constants.

You are expressly forbidden to:
  1. Define or use any macros.
  2. Define any additional functions in this file.
  3. Call any functions.
  4. Use any form of casting.
  5. Use any data type other than int or unsigned.  This means that you
     cannot use arrays, structs, or unions.
  6. Use any floating point data types, operations, or constants.


NOTES:
  1. Use the dlc (data lab checker) compiler (described in the handout) to 
     check the legality of your solutions.
  2. Each function has a maximum number of operators (! ~ & ^ | + << >>)
     that you are allowed to use for your implementation of the function. 
     The max operator count is checked by dlc. Note that '=' is not 
     counted; you may use as many of these as you want without penalty.
  3. Use the btest test harness to check your functions for correctness.
  4. Use the BDD checker to formally verify your functions
  5. The maximum number of ops for each function is given in the
     header comment for each function. If there are any inconsistencies 
     between the maximum ops in the writeup and in this file, consider
     this file the authoritative source.

/*
 * STEP 2: Modify the following functions according the coding rules.
 * 
 *   IMPORTANT. TO AVOID GRADING SURPRISES:
 *   1. Use the dlc compiler to check that your solutions conform
 *      to the coding rules.
 *   2. Use the BDD checker to formally verify that your solutions produce 
 *      the correct answers.
 */


#endif
/* Copyright (C) 1991-2018 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */
/* This header is separate from features.h so that the compiler can
   include it implicitly at the start of every compilation.  It must
   not itself include <features.h> or any other header that includes
   <features.h> because the implicit include comes before any feature
   test macros that may be defined in a source file before it first
   explicitly includes a system header.  GCC knows the name of this
   header in order to preinclude it.  */
/* glibc's intent is to support the IEC 559 math functionality, real
   and complex.  If the GCC (4.9 and later) predefined macros
   specifying compiler intent are available, use them to determine
   whether the overall intent is to support these features; otherwise,
   presume an older compiler has intent to support these features and
   define these macros by default.  */
/* wchar_t uses Unicode 10.0.0.  Version 10.0 of the Unicode Standard is
   synchronized with ISO/IEC 10646:2017, fifth edition, plus
   the following additions from Amendment 1 to the fifth edition:
   - 56 emoji characters
   - 285 hentaigana
   - 3 additional Zanabazar Square characters */
/* We do not support C11 <threads.h>.  */
/* 
 * oddBits - return word with all odd-numbered bits set to 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 8
 *   Rating: 2
 */
int oddBits(void) {
	int ob = 0xaa; //bit equivalent to 1010 1010
	//simply shift this constant to the left by 24 16 and 8 spots and or the results together 
	return (ob<<24)|(ob<<16)|(ob<<8)|(ob); 
}
/*
 * isTmin - returns 1 if x is the minimum, two's complement number,
 *     and 0 otherwise 
 *   Legal ops: ! ~ & ^ | +
 *   Max ops: 10
 *   Rating: 1
 */
int isTmin(int x) {
	//only 0 and the largest minimum two's complement number will overflow to 0 if added to itself
	//the second part of the and  makes sure it is not 0
	return (!(x+x))&(!!x);
}
/* 
 * bitXor - x^y using only ~ and & 
 *   Example: bitXor(4, 5) = 1
 *   Legal ops: ~ &
 *   Max ops: 14
 *   Rating: 1
 */
int bitXor(int x, int y) {
  	//consider following truth table
	// x | y | ~(x&(~y)) | ~((~x)&y) | ~((~x(x&(~y))&(~((~x)&y)))
	// 0 | 0 |     1     |     1     |             0
	// 0 | 1 |     1     |     0     |             1
	// 1 | 0 |     0     |     1     |             1
	// 1 | 1 |     1     |     1     |             0
	//
	// Which is what we need for a bitwise Xor
	return ~((~(x&(~y)))&(~((~x)&y)));
}
/* 
 * conditional - same as x ? y : z 
 *   Example: conditional(2,4,5) = 4
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 16
 *   Rating: 3 
 */
int conditional(int x, int y, int z) {
  int notX = !x;   //logical negation of x: if x is 0 then notX is 1, otherwise notX is 0
  int negOne = ~0; //0xffffffff mask or -1
  
  //if x is true then notX is 0, which added to negOne results in the mask 0xffffffff
  //if we and this mask to y we get y
  //
  //if x is false then not x is 1, which added to negOne results in 0 which anded to y returns all 0's
  
  //if x is true then !notX is 1, which added to negOne results in 0 which anded to z returns all 0's
  //
  //if x is false then !notX is 0, which added to negOne results in the mask 0xfffffff 
  //if we and this mask to z we get z
  //
  //
  //in conclusion we either get 0|z or y|0, which returns y if x is true and z if x is false
  return ((notX+negOne)&y)|((!notX+negOne)&z);
}
/* 
 * greatestBitPos - return a mask that marks the position of the
 *               most significant 1 bit. If x == 0, return 0
 *   Example: greatestBitPos(96) = 0x40
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 70
 *   Rating: 4 
 */
int greatestBitPos(int x) {
	int greatestBitMask = 0; //our return value we will fix later
	int imask31 = ~(0x80<<24); //mask 01111111 11111111 11111111 11111111

	//maskX returns a mask of all 1's up to x's most significant bit
        //eg if x is  00001001 10010101 01110100 01101101
        //then maskX  00001111 11111111 11111111 11111111 
	int maskX = x | x>>1;
	maskX = maskX | maskX>>2;
	maskX = maskX | maskX>>4;
	maskX = maskX | maskX>>8;
	maskX = maskX | maskX>>16;
	  
 
 	//if we shift maskX to the left by one (and and with imask31 for edge case when greates bit
	// is at position 31) and add 1 to it we get the mask we need
	

	greatestBitMask = ((maskX>>1) & imask31) + 1;
	
	//remember edge case when x is 0 so just and x to the mask
	return greatestBitMask & x;
}
	
/* 
 * divpwr2 - Compute x/(2^n), for 0 <= n <= 30
 *  Round toward zero
 *   Examples: divpwr2(15,1) = 7, divpwr2(-33,4) = -2
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 15
 *   Rating: 2
 */
	
int divpwr2(int x, int n) {
	//the usual case includes simply shifting x to the left by n bits
	//this works well when x is positive and it does proper rounding

	//However rounding changes depending if x is  negative
	int isNegative = !!(0x80<<24 & x); //returns a 1 if x is negative

	//The following determines whether you are supposed to round x
	int firstNbitsNotZero = !!((~(~0<<n))&x); //returns 1 if the n least significant bits in x are not 0

	//if both of them are true then add one to our result 
	return ((x>>n)+(isNegative &  firstNbitsNotZero));
}
/* 
 * isNonNegative - return 1 if x >= 0, return 0 otherwise 
 *   Example: isNonNegative(-1) = 0.  isNonNegative(0) = 1.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 6
 *   Rating: 3
 */
int isNonNegative(int x) {
	//anding x with 0x80000000 returns us its sign
	//whether x is nonNegative (positive) is just the logical opposite (!) of this
	return !(x&(0x80<<24));  	
}
/*
 * satMul2 - multiplies by 2, saturating to Tmin or Tmax if overflow
 *   Examples: satMul2(0x30000000) = 0x60000000
 *             satMul2(0x40000000) = 0x7FFFFFFF (saturate to TMax)
 *             satMul2(0x60000000) = 0x80000000 (saturate to TMin)
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3
 */
int satMul2(int x) {
        //first we need to determine whether x will overflow or not
	//x overflows if it's first two bits are different from each other i.e. if they are 10 or 01
	//the following mask returns 0xffffffff if x overflows or 0 if it does not
	int overflowMask = (x^(x<<1))>>31;

	//next if x does not overflow we simply return x<<1
	//hence resultNoOverflow returns x<<1 if x overflows or 0 if it does not
	int resultNoOverflow = (x<<1)&(~overflowMask);

	//if x does overflow we need to return either 0x7fffffff if x is positive or 0x80000000 if it is not
	//the following return the appropriate result if x overflows
	int iMask31 = ~(0x80<<24); //0x7fffffff is what we need to return if x is positive and overflows
	// simply adding 1 to iMask31 if x is negative and overflows return the appropriate result
	int signOfX = x&(0x80<<24); //retuns 0x8000000000 if x is positive or 0 if x is negative
	
	//resultWithOverflow returns the appropriate result if x overflows or 0 if it does not
	int resultWithOverflow = (iMask31 + (!!signOfX)) & overflowMask;
	
	return (resultNoOverflow) | (resultWithOverflow); 

}

/* 
 * isLess - if x < y  then return 1, else return 0 
 *   Example: isLess(4,5) = 1.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 24
 *   Rating: 3
 */
int isLess(int x, int y) {
  	//
	return 	1&				//format result to 0x01 or 0x00 
		((((x&(~y)))>>31) | (		//case when x negative and y positive (return 0xffffffff)
		(((x|(~y)))>>31) & 		//case when x positive and y negative (return 0) 
		((!(y^(0x80<<24)))+~0) & 	//edge case when y = 0x80000000 always return 0
		((x+(~y+1))>>31))); 		//default case, subtract y from x
						//returns 0xffffffff if x<y and 0 otherwise
}

/* 
 * isAsciiDigit - return 1 if 0x30 <= x <= 0x39 (ASCII codes for characters '0' to '9')
 *   Example: isAsciiDigit(0x35) = 1.
 *            isAsciiDigit(0x3a) = 0.
 *            isAsciiDigit(0x05) = 0.
 *   Legal ops: !  & ^ | + << >>
 *   Max ops: 15
 *   Rating: 3
 */
int isAsciiDigit(int x) {
//all ASCII digits are of the format 0x30 to 0x39
//this corresponds to range from 000000000 00000000 00000000 00110000
//                            to 000000000 00000000 00000000 00111001
//
	//the following mask determines whether x is in the range 0x30 to 0x3f
	int mask4To5 =(x&(~0x0f))^0x30;	//gives a 0 only if bits at indices 4 and 5 are both 1
					//and bits at indices greater than 5 are all 0
					//negation of this returns 1 if x is in range 0x30 to 0x3f
					//negation returns 0 if x is not in this range
				    	
	//the following mask determines whether the last 4 bits in x are in the range form 0 to 9
	int mask0To3 =(((x&0x04)<<1)|((x&0x02)<<2))&(x&0x08);//gives a 0 only if bits at position 3 and 2 are 0
						      	    //or if the bit a position 4 is 0
							    //returns 0x80 otherwise
							    //negation returns 1 if last 4 bits in x are 0-9
							    //negation returns 0 if otherwise
	//if both masks return a 0 then x must be in range 0x30 to 0x39
	return !(mask4To5 | mask0To3);

}
/*
 * trueThreeFourths - multiplies by 3/4 rounding toward 0,
 *   avoiding errors due to overflow
 *   Examples: trueThreeFourths(11) = 8
 *             trueThreeFourths(-9) = -6
 *             trueThreeFourths(1073741824) = 805306368 (no overflow)
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 4
 */
int trueThreeFourths(int x)
{
  	 
   	//start by subtracting floor(x/4) from x
 	//this also properly round x towards 0 for negative numbers 
       	int approxThreeQuarters = x + (~(x>>2)+1);

  	//Values of x which give a remainder of 1 2 or 3 when dividing a 4 are off by +1 if x is positive 
	//Therefore we need to subtract 1 i.e. add 0xffffffff
	
 	int signMask = ~(x>>31); //returns 0xffffffff if x is positive and  0 if x is negative
	int givesRemainder = !((x&0x01)|(x&0x02)) + ~0; //if x givea a remainder returns 0xffffffff
							//if x doesn't give  a remainder returns 0
	
	//Hence signMask & givesRemainder returns -1 if x is positive and gives a remainder and 0 otherwise
	return approxThreeQuarters + (signMask & givesRemainder);
  
} 
/*
 * ilog2 - return floor(log base 2 of x), where x > 0
 *   Example: ilog2(16) = 4
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 90
 *   Rating: 4
 */
int ilog2(int x) {
  //principle: count every other zero starting from bit position 31 up to the most significant bit
  // we count every other bit to save on number of operatiors
  
  int negOne = ~0; //negative one or the mask 0xffffffff
  int mask = 0; //mask we will use later
  //score counts every other zero starting at bit position 31 up to the most significant bit  
  int score = (	(!x)+!(x>>2)+!(x>>4)+!(x>>6)+
         	!(x>>8)+!(x>>10)+!(x>>12)+!(x>>14)+
         	!(x>>16)+!(x>>18)+!(x>>20)+!(x>>22)+
        	!(x>>24)+!(x>>26)+!(x>>28)+!(x>>30) );
  //by subtracting twice the score from 31 we get the approximate position of the most significant bit
  //we say approximate since we may be off by one
  int approx = 32 + ~(score+score);
  
  //the following mask tests if our approximation is correct or if we are off by one
  mask = !(x>>approx) + negOne;  //returns 0xffffffff if approx is correct and 0 if it is not
  //if our approximation is correct return the approximation, otherwise subtract one from it
  return (mask & approx) | (~mask & (approx + negOne));
}
/* 
 * float_neg - Return bit-level equivalent of expression -f for
 *   floating point argument f.
 *   Both the argument and result are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representations of
 *   single-precision floating point values.
 *   When argument is NaN, return argument.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 10
 *   Rating: 2
 */
unsigned float_neg(unsigned uf) {
	//We only need consider the case when uf is NaN
	int isNaN = !(((uf>>23)&0xff)+(~0xff)+1); //returns 1 if uf is NaN by checking if bits 
						  //in range 23 to 30 are all 1's
						  //returns 0 otherwise
	//also make sure to check that uf is not infinity (uf<<9 returns 0 if it is)
	if (isNaN && (uf<<9)) {
		return uf;
	}
	//in the default case simply flip the first bit
	return (uf^(0x80<<24));
}
/* 
 * float_i2f - Return bit-level equivalent of expression (float) x
 *   Result is returned as unsigned int, but
 *   it is to be interpreted as the bit-level representation of a
 *   single-precision floating point values.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
unsigned float_i2f(int x) {
 int E = 0; //exponent
 int e = 0; //exponent - bias
 int f = 0; //binary decimal remaninder
 int s = 0; //sign
 int correction = 0; //correction bit for the fractional component when abs(x) has more than 24 bits

 //some masks
 int mask31 = 0x80<<24; //0x8000000000
 
 //special case if x = 0 
 if (!x) {return 0;} 
 //special case if x = 0x80000000
 if (!(x+ (~mask31)+1)) {return (0xcf<<24);}

 //we may figure out the sign immediately
 s = x&(mask31);

 //we need absolute value of x
 if (s) { x = (~x) +1;}
  
 //figure out E by figuring out the index of the most significant bit in x 
 while (x>>(E+1))  {E = E+1;}
 
 //figure out e as e=E+bias
 //and then we shift it to the left by 23 bits
 e = (E + 127)<<23;

 //figure out f
 //f is composed of the bits in x up to E, which means that we need to put only the bits up to E
 //in the correct location
 //to isolate only these bits, first shift them (31 - E) places to the left
 //then undo the leading bit in position 31 by anding with ~(0x80<<24)
 //finally move this result to the right by 8 (k) bits to the right to get proper placement
 
 f = ((x<<(32+~E))&(~mask31));

 //before shifting right determine whether the number needs to be rounded
 //case 1) bits at postitions 7 and 8 are both 1 (1.1) we round up
 //case 2) bit at positions  7 is 0 (x.0) we round down 
 //case 3) bit at position 8 is 0, bit at position 8 is 1 and any of the bits in positions 0 to 6 are 1 
 //	(0.1...1...) round up

//The rounding is as follows

 if ((f&0x80) && ((f&(0x80<<1)) || (f&0x7f))) {correction = 1;}
 
 f = (f>>8) + correction;
 
 //put it all together
 return s+e+f;

}
/* 
 * float_twice - Return bit-level equivalent of expression 2*f for
 *   floating point argument f.
 *   Both the argument and result are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representation of
 *   single-precision floating point values.
 *   When argument is NaN, return argument
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
unsigned float_twice(unsigned uf) {
  	int s = 0;  //sign
	int e = 0;  //exponent bits
	int f = 0;  //binary decimal bits

	//masks	
	int mask23 = (1<<23);
	int mask23to30 = (0xff<<23); //mask 0x7f800000 for isolating e, also equal to infinity
	int mask31 = 0x80<<24; //mask 0x80000000 for isolating s
	int maskc23 = ~(mask31 | mask23to30);
	
	//we may figure out s immediately
	s = uf & mask31;

	//initially e is
	e = uf & mask23to30;

	//initially f is
	f = uf & maskc23;

	//if e is all 1's then uf is NaN
	if (!(e + ~mask23to30 +1)) {return uf;}

	//if e is all 0's then uf is denormalized
	if(!(e+~0 +1)) {
		//then the only thing we need to do is shift bits in f to the left by 1
		f = f << 1;
		return s+e+f;
	}

	//otherwise uf is normalized
	//we only need consider the case if uf overflows and then return infinity 
	if (!(e +(~ mask23to30) + mask23  + 1)) {
		return s + mask23to30;
	}
	
	//otherwise just increment the bits in e by 1
	e = e + mask23;
	
	//put it all together
	return s+e+f;
}
