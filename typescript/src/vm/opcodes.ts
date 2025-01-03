import { WorldState } from "../types/State";
import { AccruedSubstate, Input, MachineState } from "../types/Vm";
import { theta } from "./evm";

export enum Opcode {
  STOP = 0x00,
  ADD = 0x01,
  CALLDATALOAD = 0x35,
  GAS = 0x5a,
  CALL = 0xf1,
  PUSH1 = 0x60,
  PUSH2 = 0x61,
  PUSH3 = 0x62,
  PUSH4 = 0x63,
  PUSH5 = 0x64,
  PUSH6 = 0x65,
  PUSH7 = 0x66,
  PUSH8 = 0x67,
  PUSH9 = 0x68,
  PUSH10 = 0x69,
  PUSH11 = 0x6a,
  PUSH12 = 0x6b,
  PUSH13 = 0x6c,
  PUSH14 = 0x6d,
  PUSH15 = 0x6e,
  PUSH16 = 0x6f,
  PUSH17 = 0x70,
  PUSH18 = 0x71,
  PUSH19 = 0x72,
  PUSH20 = 0x73,
  PUSH21 = 0x74,
  PUSH22 = 0x75,
  PUSH23 = 0x76,
  PUSH24 = 0x77,
  PUSH25 = 0x78,
  PUSH26 = 0x79,
  PUSH27 = 0x7a,
  PUSH28 = 0x7b,
  PUSH29 = 0x7c,
  PUSH30 = 0x7d,
  PUSH31 = 0x7e,
  PUSH32 = 0x7f,
}

type Instruction = {
  name: string;
  getExecutionResult: (
    worldState: WorldState,
    machineState: MachineState,
    accruedSubstate: AccruedSubstate,
    input: Input
  ) => {
    worldState: WorldState;
    machineState: MachineState;
    accruedSubstate: AccruedSubstate;
  };
};

export let Instructions: Map<Opcode, Instruction> = new Map();

/*
#######################################
||                                   ||
|| 0s: Stop and Arithmetic Operations||
||                                   ||
#######################################
*/
Instructions.set(Opcode.STOP, {
  name: "STOP",
  getExecutionResult: (
    worldState: WorldState,
    machineState: MachineState,
    accruedSubstate: AccruedSubstate,
    input: Input
  ) => {
    return { worldState, machineState, accruedSubstate };
  },
});

Instructions.set(Opcode.ADD, {
  name: "ADD",
  getExecutionResult: (
    worldState: WorldState,
    machineState: MachineState,
    accruedSubstate: AccruedSubstate,
    input: Input
  ) => {
    let a = machineState.stack.pop().toBigInt();
    let b = machineState.stack.pop().toBigInt();
    machineState.stack.push(a + b);
    return { worldState, machineState, accruedSubstate };
  },
});

Instructions.set(Opcode.CALLDATALOAD, {
  name: "CALLDATALOAD",
  getExecutionResult: (
    worldState: WorldState,
    machineState: MachineState,
    accruedSubstate: AccruedSubstate,
    input: Input
  ) => {
    let offset = machineState.stack.pop().toBigInt();
    let calldata = input.data
      .subarray(Number(offset))
      .toString("hex")
      .padEnd(64, "0");
    machineState.stack.push(calldata);
    return { worldState, machineState, accruedSubstate };
  },
});

/*
###################################################
||                                               ||
||50s: Stack, Memory, Storage and Flow Operations||
||                                               ||
###################################################
*/

Instructions.set(Opcode.GAS, {
  name: "GAS",
  getExecutionResult: (
    worldState: WorldState,
    machineState: MachineState,
    accruedSubstate: AccruedSubstate,
    input: Input
  ) => {
    machineState.stack.push(machineState.gasAvailable);
    return { worldState, machineState, accruedSubstate };
  },
});

/*
################################
||                            ||
||   f0s: System operations   ||
||                            ||
################################
*/

Instructions.set(Opcode.CALL, {
  name: "CALL",
  getExecutionResult: (
    worldState: WorldState,
    machineState: MachineState,
    accruedSubstate: AccruedSubstate,
    input: Input
  ) => {
    let gas = machineState.stack.pop();
    let to = machineState.stack.pop();
    let value = machineState.stack.pop();
    let inOffset = machineState.stack.pop();
    let inSize = machineState.stack.pop();
    let outOffset = machineState.stack.pop();
    let outSize = machineState.stack.pop();

    return { worldState, machineState, accruedSubstate };
  },
});

// PUSH
for (let i = 0; i < 32; i++) {
  Instructions.set(0x60 + i, {
    name: `PUSH${i + 1}`,
    getExecutionResult: (
      worldState: WorldState,
      machineState: MachineState,
      accruedSubstate: AccruedSubstate,
      input: Input
    ) => {
      if (i > 4) {
        throw new Error("PUSH instruction too large");
      }

      let start = machineState.pc + 1;
      let end = start + i + 1;

      const value = input.code.subarray(start, end).toString("hex");
      console.log("PUSH" + (i + 1) + " " + value, input);

      machineState.stack.push(value);

      return { worldState, machineState, accruedSubstate };
    },
  });
}
