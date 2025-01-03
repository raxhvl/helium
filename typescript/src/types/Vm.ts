export class Input {
  // Important information used in the execution environment that the
  // execution agent must provide.

  // The address of the account which owns the code that is executing.
  executionAddress = Buffer.alloc(20);

  // The sender address of the transaction that originated this execution.
  originAddress = Buffer.alloc(20);

  // The address of the account which caused the code to be executing.
  senderAddress = Buffer.alloc(20);

  // The byte array that is the machine code to be executed.
  code = Buffer.alloc(0);

  constructor(
    executionAddress: Buffer,
    originAddress: Buffer,
    senderAddress: Buffer,
    code: Buffer
  ) {
    this.executionAddress = executionAddress;
    this.originAddress = originAddress;
    this.senderAddress = senderAddress;
    this.code = code;
  }
}

export class AccruedSubstate {
  // Throughout transaction execution, we accrue certain
  // information that is acted upon immediately
  // following the transaction.
}

export class MachineState {
  // The machine state μ is defined as
  // the tuple (g, pc, m, i, s, o) which are the gas available, the
  // program counter pc ∈ N_{256} , the memory contents, the
  // active number of words in memory (counting continuously
  // from position 0), the stack contents, and the returndata
  // buffer.

  gasAvailable: bigint;
  pc: number;
  memory: Memory;
  stack: Stack;
  returnData: Buffer;

  constructor(gasStipend: bigint) {
    this.gasAvailable = gasStipend;
    this.pc = 0;
    this.memory = new Memory();
    this.stack = new Stack();
    this.returnData = Buffer.alloc(0);
  }

  getActiveMemoryWordSize(): number {
    return this.memory.size() / 32;
  }
}

export class Memory {
  private _memory: Buffer = Buffer.alloc(0);

  public read(offset): Buffer {
    return this._memory.subarray(offset, offset + 32);
  }

  public write(offset: number, value: number, mode: "byte" | "word"): void {
    if (mode === "byte") {
      this._memory.writeUInt8(value, offset);
    } else {
      this._memory.writeUInt32BE(value, offset);
    }
  }

  /**
   * The size of the memory in bytes
   * @returns The size of the memory in bytes
   * */
  public size(): number {
    return this._memory.length;
  }
}

export class Stack {
  private _stack: bigint[] = [];

  maxLength: number = 1024;

  public push(value: bigint | string): void {
    if (typeof value === "string") {
      value = BigInt(value);
    }

    if (this._stack.length == this.maxLength - 1) {
      throw new Error("Stack overflow");
    }
    this._stack.push(value);
  }

  public pop(): bigint {
    let value = this._stack.pop();
    if (value == undefined) {
      throw new Error("Stack underflow");
    }
    return value;
  }

  public peek(): bigint {
    if (this._stack.length === 0) {
      throw new Error("Stack underflow");
    }
    return this._stack[this._stack.length - 1];
  }

  public get length(): number {
    return this._stack.length;
  }
}
