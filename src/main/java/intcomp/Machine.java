package intcomp;

public class Machine {

    final static int
        CST = 0, VAR = 1, ADD = 2, SUB = 3,
        MUL = 4, POP = 5, SWAP = 6;

    public static int seval(int[] code) {                              // bytecode
        int[] stack = new int[1000];
        int sp = -1;                                                   // stack pointer
        int pc = 0;                                                    // program counter
        int instr;                                                     // current instruction

        while (pc < code.length) {                                     // while bytecode to read
            switch (instr = code[pc++]) {                              // fetch current code and check case
                case CST:                                              // if constant [0 c]; `c` constant
                    stack[sp+1] = code[pc++]; sp++;                    // store next value into stack
                    break;
                case VAR:                                              // if variable [1 x]; `x` variable index
                    stack[sp+1] = stack[sp-code[pc++]]; sp++;          // store value into stack indexed at sp-`x index`
                    break;
                case ADD:                                              // if add
                    stack[sp-1] = stack[sp-1] + stack[sp]; sp--;       // read last two value and add it to stack
                    break;
                case SUB:
                    stack[sp-1] = stack[sp-1] - stack[sp]; sp--;
                    break;
                case MUL:
                    stack[sp-1] = stack[sp-1] * stack[sp]; sp--;
                    break;
                case POP:                                              // remove top of the value by decrementing `sp`
                    sp--;
                    break;
                case SWAP:                                             // swap top two values
                {
                    int tmp = stack[sp];
                    stack[sp] = stack[sp-1];
                    stack[sp-1] = tmp;
                    break;
                }
                default:
                    break;
            }
        }

        return stack[sp];                                              // return value at the stack pointer
    }
}
