package main

import (
	"unsafe"
	"fmt"
	"os"
	"strings"
	//"encoding/binary" // @todo: do the parsing properly
)

// @todo: AT&T syntax printing

type (
	Exec struct {
		dontCareForNow int64
		sizeText int32
		sizeData int32
		sizeBSS int32
		entryPoint int32
		memTotal int32
		sizeSym int32
	}
	Operation int
	Operand interface {
		AsmString() string
	}
	Operands []Operand
	Instruction struct {
		offset, size int
		bytes [4]byte
		operation Operation
		operands Operands
	}
	Register struct {
		name, width byte
	}
	Memory struct {
		mod, rm, dispHigh, dispLow byte
	}
	Immediate struct {
		width byte
		value int16
	}
)

const (
	OpMovRegRm Operation = iota
	OpMovRegImm
	OpAddRegRm
	OpIntType3
	OpIntTypeSpecified
)

func (op Operation) Description() string {
	switch op {
	default:
		panic("unknown operation")
	case OpMovRegRm:
		return "MOV Register/Memory to/from Register"
	case OpMovRegImm:
		return "MOV Immediate to Register"
	case OpAddRegRm:
		return "ADD Register/Memory with Register to Either"
	case OpIntType3:
		return "INT Type 3"
	case OpIntTypeSpecified:
		return "INT Type Specified"
	}
}

func (op Operation) String() string {
	switch op {
	default:
		panic("unknown operation")
	case OpMovRegRm, OpMovRegImm:
		return "mov"
	case OpAddRegRm:
		return "add"
	case OpIntType3, OpIntTypeSpecified:
		return "int"
	}
}

func (ops Operands) String() string {
	var builder strings.Builder
	for i, op := range ops {
		if i > 0 {
			builder.WriteString(", ")
		}
		builder.WriteString(op.AsmString())
	}
	return builder.String()
}

func (reg Register) String() string {
	return reg.AsmString()
}

func (reg Register) AsmString() string {
	names := []string{"al", "cl", "dl", "bl", "ah", "ch", "dh", "bh", "ax", "cx", "dx", "bx", "sp", "bp", "si", "di"}
	return names[reg.name+reg.width*8]
}

func (mem Memory) String() string {
	return mem.AsmString()
}

func (mem Memory) AsmString() string {
	names := []string{"bx+si", "bx+di", "bp+si", "bp+di", "si", "di", "bp", "bx"}
	switch mem.mod {
	default:
		panic("invalid mod")
	case 0b00:
		if mem.rm == 0b110 {
			disp := (int16(mem.dispHigh) << 8) ^ int16(mem.dispLow)
			return fmt.Sprintf("[%s+%04x]", names[mem.rm], disp)
		}
		return "[" + names[mem.rm] + "]"
	case 0b01:
		return fmt.Sprintf("[%s+%04x]", names[mem.rm], int16(mem.dispLow))
	case 0b10:
		disp := (int16(mem.dispHigh) << 8) ^ int16(mem.dispLow)
		return fmt.Sprintf("[%s+%04x]", names[mem.rm], disp)
	case 0b11:
		panic("rm is reg")
	}
}

func (imm Immediate) String() string {
	return imm.AsmString()
}

func (imm Immediate) AsmString() string {
	return fmt.Sprintf("%0[1]*x", (imm.width+1)*2, imm.value)
}

func (inst Instruction) String() string {
	return fmt.Sprintf("%04x: %-14x %s %s", inst.offset, inst.bytes[:inst.size], inst.operation, inst.operands)
}

func decode(text []byte) (insts []Instruction, err error) {
	for i := 0; i < len(text); {
		offset := i
		i1 := text[i]; i++
		switch {
		case (i1 & 0b11111100) == 0b10001000: // MOV REG R/M
			i2 := text[i]; i++
			d := (i1 & 0b00000010) >> 1
			w := i1 & 0b00000001 // w == 0 -> byte, w == 1 -> word (copy 2 bytes at once)
			mod := (i2 & 0b11000000) >> 6
			reg := (i2 & 0b00111000) >> 3
			rm := i2 & 0b00000111
			inst := Instruction {
				offset: offset,
				size: i - offset,
				bytes: [4]byte{i1,i2,0,0},
				operation: OpMovRegRm,
				operands: nil,
			}
			opReg := Register{name: reg, width: w}
			var opRM Operand
			if mod == 0b11 {
				opRM = Register{name: rm, width: w}
			} else {
				opRM = Memory{mod: mod, rm: rm}
			}
			if d == 0 { // from reg
				inst.operands = Operands{opRM, opReg}
			} else { // to reg
				inst.operands = Operands{opReg, opRM}
			}
			insts = append(insts, inst)
		case (i1 & 0b11110000) == 0b10110000: // MOV REG, IMM8/IMM16
			i2 := text[i]; i++
			i3 := byte(0)
			w := (i1 & 0b00001000) >> 3
			data := int16(i2)
			if w == 1 {
				i3 = text[i]; i++
				data = (int16(i3) << 8) ^ data
			}
			reg := i1 & 0b00000111
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [4]byte{i1,i2,i3,0},
				operation: OpMovRegImm,
				operands: Operands{
					Register{name: reg, width: w},
					Immediate{width: w, value: data},
				},
			})
		case (i1 & 0b11111100) == 0: // ADD REG R/M
			i2 := text[i]; i++
			d := (i1 & 0b00000010) >> 1
			w := i1 & 0b00000001
			mod := (i2 & 0b11000000) >> 6
			reg := (i2 & 0b00111000) >> 3
			rm := i2 & 0b00000111
			inst := Instruction {
				offset: offset,
				size: i - offset,
				bytes: [4]byte{i1,i2,0,0},
				operation: OpAddRegRm,
				operands: nil,
			}
			opReg := Register{name: reg, width: w}
			var opRM Operand
			if mod == 0b11 {
				opRM = Register{name: rm, width: w}
			} else {
				opRM = Memory{mod: mod, rm: rm}
			}
			if d == 0 { // from reg
				inst.operands = Operands{opRM, opReg}
			} else { // to reg
				inst.operands = Operands{opReg, opRM}
			}
			insts = append(insts, inst)
		case i1 == 0b11001100: // INT (TYPE 3)
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [4]byte{i1,0,0,0},
				operation: OpIntType3,
				operands: nil,
			})
		case i1 == 0b11001101: // INT TYPE
			i2 := text[i]; i++
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [4]byte{i1,i2,0,0},
				operation: OpIntTypeSpecified,
				operands: Operands{Immediate{width: 0, value: int16(i2)}},
			})
		}
	}
	return insts, err
}

func must[T any](t T, err error) T {
	if err != nil {
		panic(err)
	}
	return t
}

func main() {
	bin := must(os.ReadFile("a.out"))
	exec := *(*Exec)(unsafe.Pointer(&bin[0]))
	fmt.Printf("%#v\n", exec)
	text := bin[32:32+exec.sizeText]
	fmt.Printf("%x\n", text)

	insts, err := decode(text)
	if err != nil {
		fmt.Println(err)
	}
	for _, inst := range insts {
		fmt.Println(inst)
	}
}
