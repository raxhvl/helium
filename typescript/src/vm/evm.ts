import { intToHex } from "../types/Data";
import { AccruedSubstate, Input, MachineState } from "../types/Vm";
import { PostState, PreState, Transaction } from "../types/State";
import { Instructions } from "./opcodes";
import { DEBUG } from "../lib/env";

export const ipsilon = (tx: Transaction, preState: PreState): PostState => {
  const intrinsicGas = 0n; // TODO: Calculate intrinsic gas
  const gasRemaining = tx.gasLimit.toBigInt() - intrinsicGas; // (81)

  const sender = Buffer.alloc(20); // TODO: Calculate sender

  const input = new Input(tx.to, sender, sender, preState[tx.to.toHex()].code);

  const accruedSubstate = new AccruedSubstate();

  const { postState } = theta(preState, gasRemaining, accruedSubstate, input);

  //   const code = pre[tx.to.toHex()].code;

  //   if (code.length) {
  //     let machineState = new MachineState(tx.gasLimit.toBigInt());

  //     for (
  //       machineState.pc = 0;
  //       machineState.pc < code.length;
  //       machineState.pc++
  //     ) {
  //       const opcode = machineState.getCurrentOpcode();

  //       const instruction = Instructions.get(opcode);

  //       if (instruction) {
  //         machineState = instruction.getExecutionResult(machineState);

  //         if (DEBUG) {
  //           console.log({ machineState });
  //         }
  //       } else {
  //         throw new Error(
  //           `Undefined instruction for opcode: ${intToHex(opcode)}`
  //         );
  //       }
  //     }
  //   }

  //   return post;
  return postState;
};

export const theta = (
  preState: PreState,
  gasRemaining: bigint,
  accruedSubstate: AccruedSubstate,
  input: Input
): {
  postState: PostState;
  gasRemaining: bigint;
  accruedSubstate: AccruedSubstate;
  output: Buffer;
} => {
  let postState = {} as PostState;
  let output = Buffer.alloc(0);

  return { postState, gasRemaining, accruedSubstate, output };
};
