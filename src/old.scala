package main

import chisel3._
import chisel3.util._
import org.scalacheck.Prop.True
import scala.collection.immutable.ListMap


import gaa.ListBundle

object Introspection {

    def isInterface(meth: java.lang.reflect.Method) = {
        (meth.getModifiers & java.lang.reflect.Modifier.ABSTRACT) != 0 &&
            (meth.getModifiers & java.lang.reflect.Modifier.PUBLIC) != 0
    }

    def getPublicFields(cc: Class[_], rootClass: Class[_]): Seq[java.lang.reflect.Method] = {
        // Suggest names to nodes using runtime reflection
        def getMethods(c: Class[_]): List[java.lang.reflect.Method] = {
            if (c == rootClass) List()
            else getMethods(c.getSuperclass) ++ c.getDeclaredMethods
        }
        val methods : List[java.lang.reflect.Method] = getMethods(cc)
        val interfaces = methods.filter(isInterface)
        val interface_names = interfaces.map(_.getName).toSet

        methods.foreach( (meth) => {
            val name = meth.getName
            val keep = !isInterface(meth) && interface_names.contains(name)
            if (keep) {

                val has_value = meth.getReturnType.toString != "void"
                val has_args = meth.getParameterCount > 0

                println(meth.getName)
                println(meth)
                println(meth.getModifiers)
                println(meth.getReturnType)
                println(meth.getParameterTypes)
                println(meth.getAnnotations)
                println(s"has_value: $has_value")
                println(s"has_args: $has_args")
                println()
            }
        })
        //    println(_.getName)
        //}

        List()
        //        def isPublicVal(m: java.lang.reflect.Method) =
        //            m.getParameterTypes.isEmpty && methodNames.contains(m.getName) && !m.getDeclaringClass.isAssignableFrom(rootClass)
        //        //cc.getMethods.filter(isPublicVal).sortWith(_.getName < _.getName)
        //        for meth <-
        //        println(methodNames)
        //        cc.getDeclaredMethods
    }
}


/*
 Out Interface class needs to extend Module because we want the implementations to directly
 derive from it.
 */
class Interface extends Module {
    lazy val io = {

        println("io called!")
        //println(Introspection.getPublicFields(this.getClass, classOf[Interface]).map(_.getName))
        Introspection.getPublicFields(this.getClass, classOf[Interface])
        IO(ListBundle(ListMap(
            "clock" -> Input(Clock())
        )))
    }
}

object guard {
    // single parameter list in order to support overloading on the block return type
    def apply (cond: => Bool, block: => Unit): Unit = {
        when(cond)(block)
    }

    def apply[T <: Data](cond: => Bool, block: => T): T = {
        var typ : Option[T] = None
        lazy val result = Wire(typ.get)
        when(cond){
            val tmp = block
            typ = Some(tmp.cloneType)
            result := tmp
        }
        result
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


abstract class IGcd extends Interface {
    // method Action
    def start(a: UInt, b: UInt): Unit

    // method ValueAction
    def result(): UInt
}


class Gcd extends IGcd {
    val x = RegInit(0.U(32.W))
    val y = RegInit(0.U(32.W))


    override def start(a: UInt, b: UInt): Unit =
        guard(y === 0.U, { x := a; y := b })

    override def result(): UInt =
        guard(y === 0.U, { x })

    // TODO: make a Module(...) like wrapper that evaluates `io`
    io
}


object Old {
    def old_main(args: Array[String]): Unit = {



        val b = new Bundle() {
            val data = Input(UInt(32.W))
        }

        println(s"${b}")


        val ir = chisel3.Driver.elaborate(() => new Gcd)
        val firrtl = chisel3.Driver.emit(ir)
        println(firrtl)


    }
}

/* src: https://raw.githubusercontent.com/rsnikhil/Bluespec_BSV_Tutorial/master/Example_Programs/Eg03a_Bubblesort/src_BSV/Bubblesort.bsv

// Copyright (c) 2013-2016 Bluespec, Inc.  All Rights Reserved.

package Bubblesort;

// ================================================================
// A serial bubble sorter

// This example is a warm-up exercise that sorts just 5 'Int#(32)' values.
// The goal here is to implement the classical sequential software
// bubble sort in hardware.

// ================================================================
// Project imports

import Utils :: *;

// ================================================================
// Interface definition for the sorter.
// Accepts a stream of 5 unsorted inputs via the put method.
// Returns a stream of 5 sorted outputs via the get method.

interface Sort_IFC;
   method Action  put (Int #(32) x);
   method ActionValue #(Int #(32))  get;
endinterface

// ================================================================
// Module defintion for the serial bubble sorter.

(* synthesize *)
module mkBubblesort (Sort_IFC);

   // ``State'' of the sorting FSM (the sequential sorting algorithm)
   // We use `pc' by analogy with ``Program Counter''
   Reg #(Bit #(3)) rg_pc <- mkReg (0);

   // Count incoming and outgoing values (up to 5)
   Reg #(UInt #(3))  rg_j <- mkReg (0);

   // True if there is a swap during current pass
   Reg #(Bool)  rg_swapped  <- mkRegU;

   // Five registers to hold the values to be sorted.
   // These registers are uninitialized.
   Reg #(Int #(32)) x0 <- mkRegU;
   Reg #(Int #(32)) x1 <- mkRegU;
   Reg #(Int #(32)) x2 <- mkRegU;
   Reg #(Int #(32)) x3 <- mkRegU;
   Reg #(Int #(32)) x4 <- mkRegU;

   // ----------------
   // RULES

   // The following four 'swap' rules are almost identical
   rule rl_swap_0_1 (rg_pc == 1);
      if (x0 > x1) begin
	 x0 <= x1; x1 <= x0; rg_swapped <= True;
      end
      rg_pc <= 2;
   endrule

   rule rl_swap_1_2 (rg_pc == 2);
      if (x1 > x2) begin
	 x1 <= x2; x2 <= x1; rg_swapped <= True;
      end
      rg_pc <= 3;
   endrule

   rule rl_swap_2_3 (rg_pc == 3);
      if (x2 > x3) begin
	 x2 <= x3; x3 <= x2; rg_swapped <= True;
      end
      rg_pc <= 4;
   endrule

   rule rl_swap_3_4 (rg_pc == 4);
      if (x3 > x4) begin
	 x3 <= x4; x4 <= x3; rg_swapped <= True;
      end
      rg_pc <= 5;
   endrule

   rule rl_loop_or_exit (rg_pc == 5);
      if (rg_swapped) begin
	 rg_swapped <= False;
	 rg_pc <= 1;
      end
      else
	 rg_pc <= 6;
   endrule

   // ----------------
   // INTERFACE

   // Help function used in both interface methods

   function Action shift (Int #(32) y);
      action
         x0 <= x1; x1 <= x2; x2 <= x3; x3 <= x4; x4 <= y;
      endaction
   endfunction

   // Inputs: feed input values into x4
   method Action put (Int#(32) x) if (rg_pc == 0);
      shift (x);
      rg_j <= rg_j + 1;
      if (rg_j == 4) begin // start the FSM
	 rg_pc <= 1;
	 rg_swapped <= False;
      end
   endmethod

   // Outputs: drain by shifting them out of x0
   method ActionValue#(Int#(32)) get () if ((rg_j != 0) && (rg_pc == 6));
      shift (?);
      rg_j <= rg_j - 1;
      if (rg_j == 1)
	 rg_pc <= 0;
      return x0;
   endmethod
endmodule: mkBubblesort

// ================================================================

endpackage: Bubblesort

 */