module mkGCD (I_GCD);
	Reg#(int) x <- mkRegU;
	Reg#(int) y <- mkReg(0);

	rule swap ((x > y) && (y != 0));
		x <= y;
		y <= x;
	endrule

	rule subtract ((x <= y) && (y != 0));
		y <= y – x;
	endrule

	method Action start(int a, int b) if(y==0);
		x <= a;
		y <= b;
	endmethod

	method int result() if (y==0);
		return x;
	endmethod
endmodule
