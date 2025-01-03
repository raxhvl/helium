import { MachineState } from "../types/Vm";

type Instruction = {
  name: string;
  getExecutionResult: (machineState: MachineState) => MachineState;
};

export let Instructions: Map<number, Instruction> = new Map();

/*
#######################################
||                                   ||
|| 0s: Stop and Arithmetic Operations||
||                                   ||
#######################################
*/

Instructions.set(0x00, {
  name: "STOP",
  getExecutionResult: (machineState: MachineState): MachineState => {
    return machineState;
  },
});

Instructions.set(0x01, {
  name: "ADD",
  getExecutionResult: (machineState: MachineState): MachineState => {
    let a = machineState.stack.pop();
    let b = machineState.stack.pop();
    machineState.stack.push(a + b);
    return machineState;
  },
});

Instructions.set(0x35, {
  name: "CALLDATALOAD",
  getExecutionResult: (machineState: MachineState): MachineState => {
    let offset = machineState.stack.pop();

    // Read the offset and pad right with 0s
    let calldata = machineState.tx.data
      .subarray(Number(offset))
      .toHex()
      .padEnd(64, "0");

    machineState.stack.push(calldata);
    return machineState;
  },
});

/*
###################################################
||                                               ||
||50s: Stack, Memory, Storage and Flow Operations||
||                                               ||
###################################################
*/

Instructions.set(0x5a, {
  name: "GAS",
  getExecutionResult: (machineState: MachineState): MachineState => {
    machineState.stack.push(machineState.gasRemaining);
    return machineState;
  },
});

/*
################################
||                            ||
||   f0s: System operations   ||
||                            ||
################################
*/

Instructions.set(0xf1, {
  name: "CALL",
  getExecutionResult: (machineState: MachineState): MachineState => {
    let gas = machineState.stack.pop();
    let to = machineState.stack.pop();
    let value = machineState.stack.pop();
    let inOffset = machineState.stack.pop();
    let inSize = machineState.stack.pop();
    let outOffset = machineState.stack.pop();
    let outSize = machineState.stack.pop();

    return machineState;
  },
});

// PUSH
for (let i = 0; i < 32; i++) {
  Instructions.set(0x60 + i, {
    name: `PUSH${i}`,
    getExecutionResult: (machineState: MachineState) => {
      if (i > 4) {
        throw new Error("PUSH instruction too large");
      }

      let start = machineState.pc + 1;
      let end = start + i + 1;

      const value = machineState.code.subarray(start, end).toHex();

      machineState.stack.push(value);
      machineState.pc += i + 1;

      return machineState;
    },
  });
}
