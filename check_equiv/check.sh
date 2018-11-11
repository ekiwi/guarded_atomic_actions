#!/usr/bin/env bash
cp ../ReferenceGcd.v ref.v

DUT=ReferenceGcd

yosys -p "
  read_verilog ref.v
  rename $DUT ref
  hierarchy -check; proc; opt; check -assert
  write_smt2 -bv -tpl test.tpl test.smt2
"

exit

yosys -p "
      read_verilog ref.v
      rename $DUT ref
      proc
      memory
      flatten ref
      hierarchy -top ref
      read_verilog .v
      rename $NEW new
      proc
      memory
      flatten new
      equiv_make ref new equiv
      hierarchy -top equiv
      clean -purge
      equiv_simple -v
      sat
    "
