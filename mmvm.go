package main

import (
	"unsafe"
	"fmt"
	"os"
	"strings"
	"errors"
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
	Segment struct {
		name byte
	}
	// Relative
	Memory struct {
		mod, rm, dispHigh, dispLow byte
	}
	// Absolute
	Address struct {
		width byte
		addr int16
	}
	Immediate struct {
		width byte
		value int16
	}
)

const (
	RegA byte = iota
	RegC
	RegD
	RegB
	RegSP
	RegBP
	RegSI
	RegDI
	RegAH = RegSP
	RegCH = RegBP
	RegDH = RegSI
	RegBH = RegDI
)

const (
	OpMovRegRm Operation = iota
	OpMovRmImm
	OpMovRegImm
	OpMovAccMem
	OpMovMemAcc
	OpMovRmSeg
	OpPushRm
	OpPushReg
	OpPushSeg
	OpPopRm
	OpPopReg
	OpPopSeg
	OpXchgRmReg
	OpXchgAccReg
	OpInFixedPort
	OpInVarPort
	OpOutFixedPort
	OpOutVarPort
	OpXLAT
	OpLEA
	OpLDS
	OpLES
	OpLAHF
	OpSAHF
	OpPUSHF
	OpPOPF
	OpAddRegRm
	OpAddRmImm
	OpAddAccImm
	OpAdcRegRm
	OpAdcRmImm
	OpAdcAccImm
	OpIncRm
	OpIncReg

	OpIntType3
	OpIntTypeSpecified
)

func (op Operation) Description() string {
	switch op {
	default:
		panic("unknown operation")
	case OpMovRegRm:
		return "MOV Register/Memory to/from Register"
	case OpMovRmImm:
		return "MOV Immediate to Register/Memory"
	case OpMovRegImm:
		return "MOV Immediate to Register"
	case OpMovAccMem:
		return "MOV Memory to Accumulator"
	case OpMovMemAcc:
		return "MOV Accumulator to Memory"
	case OpMovRmSeg:
		return "MOV Register/Memory to/from Segment Register"
	case OpPushRm:
		return "PUSH Register/Memory"
	case OpPushReg:
		return "PUSH Register"
	case OpPushSeg:
		return "PUSH Segment Register"
	case OpPopRm:
		return "POP Register/Memory"
	case OpPopReg:
		return "POP Register"
	case OpPopSeg:
		return "POP Segment Register"
	case OpXchgRmReg:
		return "XCHG Register/Memory with Register"
	case OpXchgAccReg:
		return "XCHG Register with Accumulator"
	case OpInFixedPort:
		return "IN Fixed Port"
	case OpInVarPort:
		return "IN Variable Port"
	case OpOutFixedPort:
		return "OUT Fixed Port"
	case OpOutVarPort:
		return "OUT Variable Port"
	case OpXLAT:
		return "XLAT Translate Byte to AL"
	case OpLEA:
		return "LEA Load EA to Register"
	case OpLDS:
		return "LDS Load Pointer to DS"
	case OpLES:
		return "LES Load Pointer to ES"
	case OpLAHF:
		return "LAHF Load AH with Flags"
	case OpSAHF:
		return "SAHF Store AH into Flags"
	case OpPUSHF:
		return "PUSHF Push Flags"
	case OpPOPF:
		return "POPF Pop Flags"
	case OpAddRegRm:
		return "ADD Register/Memory with Register to Either"
	case OpAddRmImm:
		return "ADD Immediate to Register/Memory"
	case OpAddAccImm:
		return "ADD Immediate to Accumulator"
	case OpAdcRegRm:
		return "ADC Register/Memory with Register to Either"
	case OpAdcRmImm:
		return "ADC Immediate to Register/Memory"
	case OpAdcAccImm:
		return "ADC Immediate to Accumulator"
	case OpIncRm:
		return "INC Register/Memory"
	case OpIncReg:
		return "INC Register"
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
	case OpMovRegRm, OpMovRmImm, OpMovRegImm, OpMovAccMem, OpMovMemAcc, OpMovRmSeg:
		return "mov"
	case OpPushRm, OpPushReg, OpPushSeg:
		return "push"
	case OpPopRm, OpPopReg, OpPopSeg:
		return "pop"
	case OpXchgRmReg, OpXchgAccReg:
		return "xchg"
	case OpInFixedPort, OpInVarPort:
		return "in"
	case OpOutFixedPort, OpOutVarPort:
		return "out"
	case OpXLAT:
		return "xlat"
	case OpLEA:
		return "lea"
	case OpLDS:
		return "lds"
	case OpLES:
		return "les"
	case OpLAHF:
		return "lahf"
	case OpSAHF:
		return "sahf"
	case OpPUSHF:
		return "pushf"
	case OpPOPF:
		return "popf"
	case OpAddRegRm, OpAddRmImm, OpAddAccImm:
		return "add"
	case OpAdcRegRm, OpAdcRmImm, OpAdcAccImm:
		return "adc"
	case OpIncRm, OpIncReg:
		return "inc"
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

func (seg Segment) String() string {
	return seg.AsmString()
}

func (seg Segment) AsmString() string {
	names := []string{"es", "cs", "ss", "ds"}
	return names[seg.name]
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

func (addr Address) String() string {
	return addr.AsmString()
}

func (addr Address) AsmString() string {
	return fmt.Sprintf("[%04x]", addr.addr)
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

func W(i byte) byte {
	return i & 1
}

func REG1(i byte) byte {
	return i & 0b111
}

func WREG(i byte) (w, reg byte) {
	return (i >> 3) & 1, i & 0b111
}

func DW(i byte) (d, w byte) {
	return D(i), W(i)
}

func SW(i byte) (s, w byte) {
	return S(i), W(i)
}

func D(i byte) byte {
	return (i >> 1) & 1
}

func S(i byte) byte {
	return (i >> 1) & 1
}

func MODREGRM(i byte) (mod, reg, rm byte) {
	return MOD(i), REG(i), RM(i)
}

func MOD(i byte) byte {
	return (i >> 6) & 0b11
}

func RM(i byte) byte {
	return i & 0b111
}

func REG(i byte) byte {
	return (i >> 3) & 0b111
}

func SEG(i byte) byte {
	return (i >> 3) & 0b11
}

func decode(text []byte) (insts []Instruction, err error) {
	for i := 0; i < len(text); {
		offset := i
		i1 := text[i]; i++
		switch {
		default:
			err = errors.Join(err, fmt.Errorf("unrecognized instruction: %x", i1))
		case (i1 & 0b11111100) == 0b10001000:
			i2 := text[i]; i++
			d, w := DW(i1)
			mod, reg, rm := MODREGRM(i2)
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
		case (i1 & 0b11111110) == 0b11000110:
			i2 := text[i]; i++
			i3 := text[i]; i++
			i4 := byte(0)
			w := W(i1)
			data := int16(i3)
			if w == 1 {
				i4 = text[i]; i++
				data = (int16(i4) << 8) ^ data
			}
			mod, zzz, rm := MODREGRM(i2)
			if zzz != 0 {
				err = errors.Join(err, fmt.Errorf("unexpected bit pattern"))
			}
			var opRm Operand
			if mod == 0b11 {
				opRm = Register{name: rm, width: w}
			} else {
				opRm = Memory{mod: mod, rm: rm}
			}
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [4]byte{i1,i2,i3,i4},
				operation: OpMovRmImm,
				operands: Operands{
					opRm,
					Immediate{width: w, value: data},
				},
			})
		case (i1 & 0b11110000) == 0b10110000:
			i2 := text[i]; i++
			i3 := byte(0)
			w, reg := WREG(i1)
			data := int16(i2)
			if w == 1 {
				i3 = text[i]; i++
				data = (int16(i3) << 8) ^ data
			}
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
		case (i1 & 0b11111110) == 0b10100000:
			i2 := text[i]; i++
			i3 := text[i]; i++
			w := W(i1)
			addr := (int16(i3) << 8) ^ int16(i2)
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [4]byte{i1,i2,i3,0},
				operation: OpMovAccMem,
				operands: Operands{
					Register{name: RegA, width: w},
					Address{width: w, addr: addr},
				},
			})
		case i1 == 0b10100010:
			i2 := text[i]; i++
			i3 := text[i]; i++
			w := W(i1)
			addr := (int16(i3) << 8) ^ int16(i2)
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [4]byte{i1,i2,i3,0},
				operation: OpMovMemAcc,
				operands: Operands{
					Address{width: w, addr: addr},
					Register{name: RegA, width: w},
				},
			})
		case i1 == 0b10001100:
			i2 := text[i]; i++
			d := D(i1)
			mod, reg, rm := MODREGRM(i2)
			if reg & 0b100 != 0 {
				err = errors.Join(err, fmt.Errorf("not a segment register"))
				reg = reg & 0b011
			}
			inst := Instruction {
				offset: offset,
				size: i - offset,
				bytes: [4]byte{i1,i2,0,0},
				operation: OpMovRmSeg,
				operands: nil,
			}
			opReg := Segment{name: reg}
			var opRm Operand
			if mod == 0b11 {
				opRm = Register{name: rm, width: 0b1}
			} else {
				opRm = Memory{mod: mod, rm: rm}
			}
			if d == 0 { // from rm (to segment)
				inst.operands = Operands{opReg, opRm}
			} else { // to rm (from segment)
				inst.operands = Operands{opRm, opReg}
			}
			insts = append(insts, inst)
		case i1 == 0b11111111:
			i2 := text[i]; i++
			mod, ooz, rm := MODREGRM(i2)
			if ooz != 0b110 {
				err = errors.Join(err, fmt.Errorf("invalid bit pattern"))
			}
			var opRm Operand
			if mod == 0b11 {
				opRm = Register{name: rm, width: 0b1}
			} else {
				opRm = Memory{mod: mod, rm: rm}
			}
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [4]byte{i1,i2,0,0},
				operation: OpPushRm,
				operands: Operands{opRm},
			})
		case (i1 & 0b11111000) == 0b01010000:
			reg := REG1(i1)
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [4]byte{i1,0,0,0},
				operation: OpPushReg,
				operands: Operands{Register{name: reg, width: 0b1}},
			})
		case (i1 & 0b11100111) == 0b00000110:
			reg := SEG(i1)
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [4]byte{i1,0,0,0},
				operation: OpPushSeg,
				operands: Operands{Segment{name: reg}},
			})
		case i1 == 0b10001111:
			i2 := text[i]; i++
			mod, zzz, rm := MODREGRM(i2)
			if zzz != 0 {
				err = errors.Join(err, fmt.Errorf("invalid bit pattern"))
			}
			var opRm Operand
			if mod == 0b11 {
				opRm = Register{name: rm, width: 0b1}
			} else {
				opRm = Memory{mod: mod, rm: rm}
			}
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [4]byte{i1,i2,0,0},
				operation: OpPopRm,
				operands: Operands{opRm},
			})
		case (i1 & 0b11111000) == 0b01011000:
			reg := REG1(i1)
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [4]byte{i1,0,0,0},
				operation: OpPopReg,
				operands: Operands{Register{name: reg, width: 0b1}},
			})
		case (i1 & 0b11100111) == 0b00000111:
			reg := SEG(i1)
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [4]byte{i1,0,0,0},
				operation: OpPopSeg,
				operands: Operands{Segment{name: reg}},
			})
		case (i1 & 0b11111110) == 0b10000110:
			i2 := text[i]; i++
			w := W(i1)
			mod, reg, rm := MODREGRM(i2)
			var opRm Operand
			if mod == 0b11 {
				opRm = Register{name: rm, width: w}
			} else {
				opRm = Memory{mod: mod, rm: rm}
			}
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [4]byte{i1,i2,0,0},
				operation: OpXchgRmReg,
				operands: Operands{opRm, Register{name: reg, width: w}},
			})
		case (i1 & 0b11111000) == 0b10010000:
			reg := REG1(i1)
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [4]byte{i1,0,0,0},
				operation: OpXchgAccReg,
				operands: Operands{
					Register{name: reg, width: 0b1},
					Register{name: RegA, width: 0b1},
				},
			})
		case (i1 & 0b11111110) == 0b11100100:
			//w := W(i1)
			i2 := text[i]; i++
			port := int16(i2)
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [4]byte{i1,i2,0,0},
				operation: OpInFixedPort,
				operands: Operands{
					Immediate{width: 0, value: port},
				},
			})
		case (i1 & 0b11111110) == 0b11101100:
			//w := W(i1)
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [4]byte{i1,0,0,0},
				operation: OpInVarPort,
				operands: nil,
			})
		case (i1 & 0b11111110) == 0b11100110:
			//w := W(i1)
			i2 := text[i]; i++
			port := int16(i2)
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [4]byte{i1,i2,0,0},
				operation: OpInFixedPort,
				operands: Operands{
					Immediate{width: 0, value: port},
				},
			})
		case (i1 & 0b11111110) == 0b11101110:
			//w := W(i1)
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [4]byte{i1,0,0,0},
				operation: OpOutVarPort,
				operands: nil,
			})
		case i1 == 0b11010111:
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [4]byte{i1,0,0,0},
				operation: OpXLAT,
				operands: nil,
			})
		case i1 == 0b10001101:
			i2 := text[i]; i++
			mod, reg, rm := MODREGRM(i2)
			var opRM Operand
			if mod == 0b11 {
				opRM = Register{name: rm, width: 1}
			} else {
				opRM = Memory{mod: mod, rm: rm}
			}
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [4]byte{i1,i2,0,0},
				operation: OpLEA,
				operands: Operands{
					Register{name: reg, width: 1},
					opRM,
				},
			})
		case i1 == 0b11000101:
			i2 := text[i]; i++
			mod, reg, rm := MODREGRM(i2)
			var opRM Operand
			if mod == 0b11 {
				opRM = Register{name: rm, width: 1}
			} else {
				opRM = Memory{mod: mod, rm: rm}
			}
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [4]byte{i1,i2,0,0},
				operation: OpLDS,
				operands: Operands{
					Register{name: reg, width: 1},
					opRM,
				},
			})
		case i1 == 0b11000100:
			i2 := text[i]; i++
			mod, reg, rm := MODREGRM(i2)
			var opRM Operand
			if mod == 0b11 {
				opRM = Register{name: rm, width: 1}
			} else {
				opRM = Memory{mod: mod, rm: rm}
			}
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [4]byte{i1,i2,0,0},
				operation: OpLES,
				operands: Operands{
					Register{name: reg, width: 1},
					opRM,
				},
			})
		case i1 == 0b10011111:
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [4]byte{i1,0,0,0},
				operation: OpLAHF,
				operands: nil,
			})
		case i1 == 0b10011110:
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [4]byte{i1,0,0,0},
				operation: OpSAHF,
				operands: nil,
			})
		case i1 == 0b10011100:
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [4]byte{i1,0,0,0},
				operation: OpPUSHF,
				operands: nil,
			})
		case i1 == 0b10011101:
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [4]byte{i1,0,0,0},
				operation: OpPOPF,
				operands: nil,
			})
		case (i1 & 0b11111100) == 0:
			i2 := text[i]; i++
			d, w := DW(i1)
			mod, reg, rm := MODREGRM(i2)
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
		case (i1 & 0b11111100) == 0b10000000:
			i2 := text[i]; i++
			i3 := text[i]; i++
			i4 := byte(0)
			data := int16(i3)
			s, w := SW(i1)
			if s == 0 && w == 1 {
				i4 = text[i]; i++
				data = (int16(i4) << 8) ^ data
			}
			mod, zzz, rm := MODREGRM(i2)
			if zzz != 0 {
				err = errors.Join(err, fmt.Errorf("unexpected bit pattern"))
			}
			var opRM Operand
			if mod == 0b11 {
				opRM = Register{name: rm, width: w}
			} else {
				opRM = Memory{mod: mod, rm: rm}
			}
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [4]byte{i1,i2,i3,i4},
				operation: OpAddRmImm,
				operands: Operands{
					opRM,
					Immediate{width: w, value: data},
				},
			})
		case (i1 & 0b11111110) == 0b00000100:
			i2 := text[i]; i++
			i3 := byte(0)
			w := W(i1)
			data := int16(i2)
			if w == 1 {
				i3 = text[i]; i++
				data = (int16(i3) << 8) ^ data
			}
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [4]byte{i1,i2,i3,0},
				operation: OpAddAccImm,
				operands: Operands{
					Register{name: RegA, width: w},
					Immediate{width: w, value: data},
				},
			})
		case (i1 & 0b11111100) == 0b00010000:
			i2 := text[i]; i++
			d, w := DW(i1)
			mod, reg, rm := MODREGRM(i2)
			var opRM Operand
			inst := Instruction {
				offset: offset,
				size: i - offset,
				bytes: [4]byte{i1,i2,0,0},
				operation: OpAddRegRm,
				operands: nil,
			}
			opReg := Register{name: reg, width: w}
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
		case (i1 & 0b11111100) == 0b10000000:
			i2 := text[i]; i++
			i3 := text[i]; i++
			i4 := byte(0)
			s, w := SW(i1)
			mod, zoz, rm := MODREGRM(i2)
			if zoz != 0b010 {
				err = errors.Join(err, fmt.Errorf("invalid bit pattern"))
			}
			data := int16(i3)
			if s == 0 && w == 1 {
				i4 = text[i]; i++
				data = (int16(i4) << 8) ^ data
			}
			var opRM Operand
			if mod == 0b11 {
				opRM = Register{name: rm, width: w}
			} else {
				opRM = Memory{mod: mod, rm: rm}
			}
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [4]byte{i1,i2,i3,i4},
				operation: OpAdcRmImm,
				operands: Operands{
					opRM,
					Immediate{width: w, value: data},
				},
			})
		case (i1 & 0b11111110) == 0b00010100:
			i2 := text[i]; i++
			i3 := byte(0)
			w := W(i1)
			data := int16(i3)
			if w == 1 {
				i3 = text[i]; i++
				data = (int16(i3) << 8) ^ data
			}
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [4]byte{i1,i2,i3,0},
				operation: OpAdcAccImm,
				operands: Operands{
					Register{name: RegA, width: w},
					Immediate{width: w, value: data},
				},
			})
		case (i1 & 0b11111110) == 0b11111110:
			i2 := text[i]; i++
			mod, zzz, rm := MODREGRM(i2)
			if zzz != 0 {
				err = errors.Join(err, fmt.Errorf("invalid bit pattern"))
			}
			w := W(i1)
			var opRM Operand
			if mod == 0b11 {
				opRM = Register{name: rm, width: w}
			} else {
				opRM = Memory{mod: mod, rm: rm}
			}
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [4]byte{i1,i2,0,0},
				operation: OpIncRm,
				operands: Operands{opRM},
			})
		case (i1 & 0b11111000) == 0b01000000:
			reg := REG(i1)
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [4]byte{i1,0,0,0},
				operation: OpIncReg,
				operands: Operands{
					Register{name: reg, width: 1},
				},
			})
		case i1 == 0b11001100:
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [4]byte{i1,0,0,0},
				operation: OpIntType3,
				operands: nil,
			})
		case i1 == 0b11001101:
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
	bin := must(os.ReadFile("a3.out"))
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
