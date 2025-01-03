import { intToHex } from "../types/Data";
import { AccruedSubstate, Input, MachineState } from "../types/Vm";
import { WorldState, Transaction } from "../types/State";
import { Instructions, Opcode } from "./opcodes";
import { DEBUG } from "../lib/env";

/**
 * Ipsilon is the Ethereum state transition function. It takes the current state and a
 * transaction and returns the new state. A transaction thus represents a valid arc between
 * two states.
 *
 */
export const ipsilon = (
  tx: Transaction,
  worldState: WorldState
): WorldState => {
  const intrinsicGas = 0n; // TODO: Calculate intrinsic gas
  const gasRemaining = tx.gasLimit.toBigInt() - intrinsicGas; // (81)

  const sender = Buffer.alloc(20); // TODO: Calculate sender

  const input = new Input(
    tx.to,
    sender,
    sender,
    worldState[tx.to.toHex()].code,
    tx.data
  );

  const accruedSubstate = new AccruedSubstate();

  ({ worldState } = theta(worldState, gasRemaining, accruedSubstate, input));

  return worldState;
};

/**
 * Theta is the Ethereum execution function. It takes the current state, the remaining gas,
 * the accrued substate, and the input data and returns the new state, the remaining gas,
 * the accrued substate, and the output data.
 *
 */
export const theta = (
  worldState: WorldState,
  gasRemaining: bigint,
  accruedSubstate: AccruedSubstate,
  input: Input
): {
  worldState: WorldState;
  gasRemaining: bigint;
  accruedSubstate: AccruedSubstate;
  output: Buffer;
} => {
  let output = Buffer.alloc(0);

  let machineState = new MachineState(gasRemaining);

  while (machineState.pc < input.code.length) {
    const currentInstruction =
      machineState.pc < input.code.length
        ? input.code[machineState.pc]
        : Opcode.STOP; // (157)

    DEBUG &&
      console.log("Processing instruction: " + intToHex(currentInstruction));

    const instruction = Instructions.get(currentInstruction);

    if (instruction) {
      ({ worldState, machineState, accruedSubstate } =
        instruction.getExecutionResult(
          worldState,
          machineState,
          accruedSubstate,
          input
        ));

      machineState.pc = updateProgramCounter(machineState, currentInstruction);

      if (DEBUG) {
        console.log(machineState);
        console.log(worldState);
      }
    } else {
      throw new Error(
        `Undefined instruction for opcode: ${intToHex(currentInstruction)}`
      );
    }
  }

  return { worldState, gasRemaining, accruedSubstate, output };
};

// Update program counter (169)
const updateProgramCounter = (
  machineState: MachineState,
  currentInstruction: number
): number => {
  return nextInstructionOffset(machineState.pc, currentInstruction);
};

// Get the offset of the next instruction skipping any
// data portion of PUSH* instructions. (162)
const nextInstructionOffset = (
  currentOffset: number,
  currentInstruction: number
): number => {
  if (
    currentInstruction >= Opcode.PUSH1 &&
    currentInstruction <= Opcode.PUSH32
  ) {
    return currentOffset + currentInstruction - Opcode.PUSH1 + 2;
  }
  return currentOffset + 1;
};
