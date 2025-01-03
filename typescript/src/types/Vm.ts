export class Input {
  // Important information used in the execution environment that the
  // execution agent must provide.

  // The address of the account which owns the code that is executing.
  executionAddress = Buffer.alloc(20);

  // The sender address of the transaction that originated this execution.
  originAddress = Buffer.alloc(20);

  // The address of the account which caused the code to be executing.
  senderAddress = Buffer.alloc(20);

  // The byte array that is the input data to this execution.
  data = Buffer.alloc(0);

  // The byte array that is the machine code to be executed.
  code = Buffer.alloc(0);

  constructor(
    executionAddress: Buffer,
    originAddress: Buffer,
    senderAddress: Buffer,
    code: Buffer,
    data: Buffer
  ) {
    this.executionAddress = executionAddress;
    this.originAddress = originAddress;
    this.senderAddress = senderAddress;
    this.code = code;
    this.data = data;
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

/**
 * Represents a simple memory model.
 * It provides the ability to read and write to memory using byte and word modes.
 */
export class Memory {
  private _memory: Buffer = Buffer.alloc(0);

  /**
   * Reads a chunk of memory starting from the specified offset.
   * @param offset The offset in memory to start reading from.
   * @returns A buffer containing the 32 bytes starting from the offset.
   */
  public read(offset: number): Buffer {
    return this._memory.subarray(offset, offset + 32);
  }

  /**
   * Writes a value to memory at the specified offset in either byte or word mode.
   *
   * In byte mode, the value is written as a single byte (8 bits).
   * In word mode, the value is written as a 32-bit unsigned integer (4 bytes).
   *
   * @param offset The offset in memory to write the value to.
   * @param value The value to write. It can be a byte (0-255) or a 32-bit integer.
   * @param mode The mode to write the value: "byte" for byte mode, "word" for word mode.
   */
  public write(offset: number, value: number, mode: "byte" | "word"): void {
    if (mode === "byte") {
      this._memory.writeUInt8(value, offset);
    } else {
      this._memory.writeUInt32BE(value, offset);
    }
  }

  /**
   * Returns the size of the memory in bytes.
   * @returns The current size of the memory in bytes.
   */
  public size(): number {
    return this._memory.length;
  }
}

/**
 * Represents a stack that stores values of up to 32 bytes.
 * It supports typical stack operations like push, pop, and peek.
 */
export class Stack {
  private _stack: Buffer[] = [];

  // Maximum stack length
  maxLength: number = 1024;

  /**
   * Pushes a value onto the stack. The value can be a string, bigint, or Buffer.
   *
   * The string is expected to be a hex string (e.g., "0xabcdef").
   * The bigint is converted into a hexadecimal string and then to a Buffer.
   *
   * @param value The value to push onto the stack, which can be a Buffer, string, or bigint.
   * @throws Error if the value exceeds 32 bytes or if the stack overflows.
   */
  public push(value: Buffer | string | bigint): void {
    if (typeof value === "string") {
      value = Buffer.from(value, "hex");
    }

    if (typeof value === "bigint") {
      value = Buffer.from(value.toString(16), "hex");
    }

    if (value.length > 32) {
      throw new Error("Stack value exceeds 32 bytes");
    }

    if (this._stack.length == this.maxLength - 1) {
      throw new Error("Stack overflow");
    }
    this._stack.push(value);
  }

  /**
   * Pops a value from the stack.
   * @returns The value popped from the top of the stack.
   * @throws Error if the stack is empty (underflow).
   */
  public pop(): Buffer {
    let value = this._stack.pop();
    if (value == undefined) {
      throw new Error("Stack underflow");
    }
    return value;
  }

  /**
   * Peeks at the top value of the stack without removing it.
   * @returns The value at the top of the stack.
   * @throws Error if the stack is empty (underflow).
   */
  public peek(): Buffer {
    if (this._stack.length === 0) {
      throw new Error("Stack underflow");
    }
    return this._stack[this._stack.length - 1];
  }

  /**
   * Returns the current length of the stack.
   * @returns The number of items currently in the stack.
   */
  public get length(): number {
    return this._stack.length;
  }
}
