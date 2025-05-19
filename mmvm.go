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
		bytes [6]byte
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
	OpInvalid Operation = iota
	OpMovRegRm
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
	OpAAA
	OpBAA
	OpSubRegRm
	OpSubRmImm
	OpSubAccImm
	OpSsbRegRm
	OpSsbRmImm
	OpSsbAccImm
	OpDecRm
	OpDecReg
	OpNeg
	OpCmpRegRm
	OpCmpRmImm
	OpCmpAccImm
	OpAAS
	OpDAS
	OpMul
	OpImul
	OpAAM
	OpDiv
	OpIdiv
	OpAAD
	OpCBW
	OpCWD
	OpNot
	OpShlSal
	OpShr
	OpSar
	OpRol
	OpRor
	OpRcl
	OpRcr
	OpAndRegRm
	OpAndRmImm
	OpAndAccImm

	OpTestRmImm

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
	case OpAAA:
		return "AAA ASCII Adjust for Add"
	case OpBAA:
		return "BAA Decimal Adjust for Add"
	case OpSubRegRm:
		return "SUB Register/Memory and Register to Either"
	case OpSubRmImm:
		return "SUB Immediate from Register/Memory"
	case OpSubAccImm:
		return "SUB Immediate from Accumulator"
	case OpSsbRegRm:
		return "SSB Register/Memory and Register to Either"
	case OpSsbRmImm:
		return "SSB Immediate from Register/Memory"
	case OpSsbAccImm:
		return "SSB Immediate from Accumulator"
	case OpDecRm:
		return "DEC Register/Memory"
	case OpDecReg:
		return "DEC Register"
	case OpNeg:
		return "NEG Change Sign"
	case OpCmpRegRm:
		return "CMP Register/Memory and Register"
	case OpCmpRmImm:
		return "CMP Immediate with Register/Memory"
	case OpCmpAccImm:
		return "CMP Immediate with Accumulator"
	case OpAAS:
		return "AAS ASCII Adjust for Subtract"
	case OpDAS:
		return "DAS Decimal Adjust for Subtract"
	case OpMul:
		return "MUL Multiply (Unsigned)"
	case OpImul:
		return "IMUL Integer Multiply (Signed)"
	case OpAAM:
		return "AAM ASCII Adjust for Multiply"
	case OpDiv:
		return "DIV Divide (Unsigned)"
	case OpIdiv:
		return "IDIV Integer Divide (Signed)"
	case OpAAD:
		return "AAD ASCII Adjust for Divide"
	case OpCBW:
		return "CBW Convert Byte to Word"
	case OpCWD:
		return "CWD Convert Word to Double Word"
	case OpNot:
		return "NOT Invert"
	case OpShlSal:
		return "SHL/SAL Shift Logical/Arithmetic Left"
	case OpShr:
		return "SHR Shift Logical Right"
	case OpSar:
		return "SAR Shift Arithmetic Right"
	case OpRol:
		return "ROL Rotate Left"
	case OpRor:
		return "ROR Rotate Right"
	case OpRcl:
		return "RCL Rotate Through Carry Flag Left"
	case OpRcr:
		return "RCR Rotate Through Carry Right"
	case OpAndRegRm:
		return "AND Register/Memory and Register to Either"
	case OpAndRmImm:
		return "AND Immediate to Register/Memory"
	case OpAndAccImm:
		return "AND Immediate to Accumulator"
	case OpTestRmImm:
		return "TEST Immediate Data and Register/Memory"
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
	case OpAAA:
		return "aaa"
	case OpBAA:
		return "baa"
	case OpSubRegRm, OpSubRmImm, OpSubAccImm:
		return "sub"
	case OpSsbRegRm, OpSsbRmImm, OpSsbAccImm:
		return "ssb"
	case OpDecRm, OpDecReg:
		return "dec"
	case OpNeg:
		return "neg"
	case OpCmpRegRm, OpCmpRmImm, OpCmpAccImm:
		return "cmp"
	case OpAAS:
		return "aas"
	case OpDAS:
		return "das"
	case OpMul:
		return "mul"
	case OpImul:
		return "imul"
	case OpAAM:
		return "aam"
	case OpDiv:
		return "div"
	case OpIdiv:
		return "idiv"
	case OpAAD:
		return "aad"
	case OpCBW:
		return "cbw"
	case OpCWD:
		return "cwd"
	case OpNot:
		return "not"
	case OpShlSal:
		return "shl" // or sal
	case OpShr:
		return "shr"
	case OpSar:
		return "sar"
	case OpRol:
		return "rol"
	case OpRor:
		return "ror"
	case OpRcl:
		return "rcl"
	case OpRcr:
		return "rcr"
	case OpAndRegRm, OpAndRmImm, OpAndAccImm:
		return "and"
	case OpTestRmImm:
		return "test"
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
			return fmt.Sprintf("[%s+%x]", names[mem.rm], disp)
		}
		return "[" + names[mem.rm] + "]"
	case 0b01:
		return fmt.Sprintf("[%s+%x]", names[mem.rm], int16(mem.dispLow))
	case 0b10:
		disp := (int16(mem.dispHigh) << 8) ^ int16(mem.dispLow)
		return fmt.Sprintf("[%s+%x]", names[mem.rm], disp)
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

func VW(i byte) (v, w byte) {
	return V(i), W(i)
}

func D(i byte) byte {
	return (i >> 1) & 1
}

func S(i byte) byte {
	return (i >> 1) & 1
}

func V(i byte) byte {
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
			i3 := byte(0)
			i4 := byte(0)
			d, w := DW(i1)
			mod, reg, rm := MODREGRM(i2)
			opReg := Register{name: reg, width: w}
			var opRM Operand
			dispHigh := byte(0)
			dispLow := byte(0)
			switch {
			case mod == 0b00 && rm == 0b110:
				fallthrough
			case mod == 0b10:
				i4 = text[i]; i++
				dispHigh = i4
				fallthrough
			case mod == 0b01:
				i3 = text[i]; i++
				dispLow = i3
				fallthrough
			case mod == 0b00:
				opRM = Memory{mod: mod, rm: rm, dispHigh: dispHigh, dispLow: dispLow}
			case mod == 0b11:
				opRM = Register{name: rm, width: w}
			}
			inst := Instruction {
				offset: offset,
				size: i - offset,
				bytes: [6]byte{i1,i2,i3,i4},
				operation: OpMovRegRm,
				operands: nil,
			}
			if d == 0 { // from reg
				inst.operands = Operands{opRM, opReg}
			} else { // to reg
				inst.operands = Operands{opReg, opRM}
			}
			insts = append(insts, inst)
		case (i1 & 0b11111110) == 0b11000110:
			i2 := text[i]; i++
			bs := make([]byte, 0, 6)
			bs = append(bs, i1, i2)
			w := W(i1)
			mod, zzz, rm := MODREGRM(i2)
			if zzz != 0 {
				err = errors.Join(err, fmt.Errorf("unexpected bit pattern"))
			}
			var opRM Operand
			dispHigh := byte(0)
			dispLow := byte(0)
			switch {
			case mod == 0b00 && rm == 0b110:
				fallthrough
			case mod == 0b10:
				dispHigh = text[i]; i++
				bs = append(bs, dispHigh)
				fallthrough
			case mod == 0b01:
				dispLow = text[i]; i++
				bs = append(bs, dispLow)
				fallthrough
			case mod == 0b00:
				opRM = Memory{mod: mod, rm: rm, dispHigh: dispHigh, dispLow: dispLow}
			case mod == 0b11:
				opRM = Register{name: rm, width: w}
			}
			data1 := text[i]; i++
			bs = append(bs, data1)
			data := int16(data1)
			if w == 1 {
				data2 := text[i]; i++
				bs = append(bs, data2)
				data = (int16(data2) << 8) ^ data
			}
			// @fixme: brother eewww [:slice-to-array:]
			for len(bs) < 6 {
				bs = append(bs, 0)
			}
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [6]byte(bs),
				operation: OpMovRmImm,
				operands: Operands{
					opRM,
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
				bytes: [6]byte{i1,i2,i3},
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
				bytes: [6]byte{i1,i2,i3},
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
				bytes: [6]byte{i1,i2,i3},
				operation: OpMovMemAcc,
				operands: Operands{
					Address{width: w, addr: addr},
					Register{name: RegA, width: w},
				},
			})
		case (i1 & 0b11111101) == 0b10001100:
			i2 := text[i]; i++
			i3 := byte(0)
			i4 := byte(0)
			d := D(i1)
			mod, reg, rm := MODREGRM(i2)
			if reg & 0b100 != 0 {
				err = errors.Join(err, fmt.Errorf("not a segment register"))
				reg = reg & 0b011
			}
			opReg := Segment{name: reg}
			var opRM Operand
			dispHigh := byte(0)
			dispLow := byte(0)
			switch {
			case mod == 0b00 && rm == 0b110:
				fallthrough
			case mod == 0b10:
				i4 = text[i]; i++
				dispHigh = i4
				fallthrough
			case mod == 0b01:
				i3 = text[i]; i++
				dispLow = i3
				fallthrough
			case mod == 0b00:
				opRM = Memory{mod: mod, rm: rm, dispHigh: dispHigh, dispLow: dispLow}
			case mod == 0b11:
				opRM = Register{name: rm, width: 1}
			}
			inst := Instruction {
				offset: offset,
				size: i - offset,
				bytes: [6]byte{i1,i2,i3,i4},
				operation: OpMovRmSeg,
				operands: nil,
			}
			if d == 0 { // from rm (to segment)
				inst.operands = Operands{opReg, opRM}
			} else { // to rm (from segment)
				inst.operands = Operands{opRM, opReg}
			}
			insts = append(insts, inst)
		case i1 == 0b11111111:
			i2 := text[i]; i++
			i3 := byte(0)
			i4 := byte(0)
			mod, ooz, rm := MODREGRM(i2)
			if ooz != 0b110 {
				err = errors.Join(err, fmt.Errorf("invalid bit pattern"))
			}
			var opRM Operand
			dispHigh := byte(0)
			dispLow := byte(0)
			switch {
			case mod == 0b00 && rm == 0b110:
				fallthrough
			case mod == 0b10:
				i4 = text[i]; i++
				dispHigh = i4
				fallthrough
			case mod == 0b01:
				i3 = text[i]; i++
				dispLow = i3
				fallthrough
			case mod == 0b00:
				opRM = Memory{mod: mod, rm: rm, dispHigh: dispHigh, dispLow: dispLow}
			case mod == 0b11:
				opRM = Register{name: rm, width: 1}
			}
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [6]byte{i1,i2,i3,i4},
				operation: OpPushRm,
				operands: Operands{opRM},
			})
		case (i1 & 0b11111000) == 0b01010000:
			reg := REG1(i1)
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [6]byte{i1},
				operation: OpPushReg,
				operands: Operands{Register{name: reg, width: 0b1}},
			})
		case (i1 & 0b11100111) == 0b00000110:
			reg := SEG(i1)
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [6]byte{i1},
				operation: OpPushSeg,
				operands: Operands{Segment{name: reg}},
			})
		case i1 == 0b10001111:
			i2 := text[i]; i++
			i3 := byte(0)
			i4 := byte(0)
			mod, zzz, rm := MODREGRM(i2)
			if zzz != 0 {
				err = errors.Join(err, fmt.Errorf("invalid bit pattern"))
			}
			var opRM Operand
			dispHigh := byte(0)
			dispLow := byte(0)
			switch {
			case mod == 0b00 && rm == 0b110:
				fallthrough
			case mod == 0b10:
				i4 = text[i]; i++
				dispHigh = i4
				fallthrough
			case mod == 0b01:
				i3 = text[i]; i++
				dispLow = i3
				fallthrough
			case mod == 0b00:
				opRM = Memory{mod: mod, rm: rm, dispHigh: dispHigh, dispLow: dispLow}
			case mod == 0b11:
				opRM = Register{name: rm, width: 1}
			}
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [6]byte{i1,i2,i3,i4},
				operation: OpPopRm,
				operands: Operands{opRM},
			})
		case (i1 & 0b11111000) == 0b01011000:
			reg := REG1(i1)
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [6]byte{i1},
				operation: OpPopReg,
				operands: Operands{Register{name: reg, width: 0b1}},
			})
		case (i1 & 0b11100111) == 0b00000111:
			reg := SEG(i1)
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [6]byte{i1},
				operation: OpPopSeg,
				operands: Operands{Segment{name: reg}},
			})
		case (i1 & 0b11111110) == 0b10000110:
			i2 := text[i]; i++
			i3 := byte(0)
			i4 := byte(0)
			w := W(i1)
			mod, reg, rm := MODREGRM(i2)
			var opRM Operand
			dispHigh := byte(0)
			dispLow := byte(0)
			switch {
			case mod == 0b00 && rm == 0b110:
				fallthrough
			case mod == 0b10:
				i4 = text[i]; i++
				dispHigh = i4
				fallthrough
			case mod == 0b01:
				i3 = text[i]; i++
				dispLow = i3
				fallthrough
			case mod == 0b00:
				opRM = Memory{mod: mod, rm: rm, dispHigh: dispHigh, dispLow: dispLow}
			case mod == 0b11:
				opRM = Register{name: rm, width: w}
			}
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [6]byte{i1,i2,i3,i4},
				operation: OpXchgRmReg,
				operands: Operands{opRM, Register{name: reg, width: w}},
			})
		case (i1 & 0b11111000) == 0b10010000:
			reg := REG1(i1)
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [6]byte{i1},
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
				bytes: [6]byte{i1,i2},
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
				bytes: [6]byte{i1},
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
				bytes: [6]byte{i1,i2},
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
				bytes: [6]byte{i1},
				operation: OpOutVarPort,
				operands: nil,
			})
		case i1 == 0b11010111:
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [6]byte{i1},
				operation: OpXLAT,
				operands: nil,
			})
		case i1 == 0b10001101:
			i2 := text[i]; i++
			i3 := byte(0)
			i4 := byte(0)
			mod, reg, rm := MODREGRM(i2)
			var opRM Operand
			dispHigh := byte(0)
			dispLow := byte(0)
			switch {
			case mod == 0b00 && rm == 0b110:
				fallthrough
			case mod == 0b10:
				i4 = text[i]; i++
				dispHigh = i4
				fallthrough
			case mod == 0b01:
				i3 = text[i]; i++
				dispLow = i3
				fallthrough
			case mod == 0b00:
				opRM = Memory{mod: mod, rm: rm, dispHigh: dispHigh, dispLow: dispLow}
			case mod == 0b11:
				opRM = Register{name: rm, width: 1}
			}
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [6]byte{i1,i2,i3,i4},
				operation: OpLEA,
				operands: Operands{
					Register{name: reg, width: 1},
					opRM,
				},
			})
		case i1 == 0b11000101:
			i2 := text[i]; i++
			i3 := byte(0)
			i4 := byte(0)
			mod, reg, rm := MODREGRM(i2)
			var opRM Operand
			dispHigh := byte(0)
			dispLow := byte(0)
			switch {
			case mod == 0b00 && rm == 0b110:
				fallthrough
			case mod == 0b10:
				i4 = text[i]; i++
				dispHigh = i4
				fallthrough
			case mod == 0b01:
				i3 = text[i]; i++
				dispLow = i3
				fallthrough
			case mod == 0b00:
				opRM = Memory{mod: mod, rm: rm, dispHigh: dispHigh, dispLow: dispLow}
			case mod == 0b11:
				opRM = Register{name: rm, width: 1}
			}
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [6]byte{i1,i2,i3,i4},
				operation: OpLDS,
				operands: Operands{
					Register{name: reg, width: 1},
					opRM,
				},
			})
		case i1 == 0b11000100:
			i2 := text[i]; i++
			i3 := byte(0)
			i4 := byte(0)
			mod, reg, rm := MODREGRM(i2)
			var opRM Operand
			dispHigh := byte(0)
			dispLow := byte(0)
			switch {
			case mod == 0b00 && rm == 0b110:
				fallthrough
			case mod == 0b10:
				i4 = text[i]; i++
				dispHigh = i4
				fallthrough
			case mod == 0b01:
				i3 = text[i]; i++
				dispLow = i3
				fallthrough
			case mod == 0b00:
				opRM = Memory{mod: mod, rm: rm, dispHigh: dispHigh, dispLow: dispLow}
			case mod == 0b11:
				opRM = Register{name: rm, width: 1}
			}
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [6]byte{i1,i2,i3,i4},
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
				bytes: [6]byte{i1},
				operation: OpLAHF,
				operands: nil,
			})
		case i1 == 0b10011110:
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [6]byte{i1},
				operation: OpSAHF,
				operands: nil,
			})
		case i1 == 0b10011100:
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [6]byte{i1},
				operation: OpPUSHF,
				operands: nil,
			})
		case i1 == 0b10011101:
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [6]byte{i1},
				operation: OpPOPF,
				operands: nil,
			})
		case (i1 & 0b11111100) == 0:
			i2 := text[i]; i++
			i3 := byte(0)
			i4 := byte(0)
			d, w := DW(i1)
			mod, reg, rm := MODREGRM(i2)
			opReg := Register{name: reg, width: w}
			var opRM Operand
			dispHigh := byte(0)
			dispLow := byte(0)
			switch {
			case mod == 0b00 && rm == 0b110:
				fallthrough
			case mod == 0b10:
				i4 = text[i]; i++
				dispHigh = i4
				fallthrough
			case mod == 0b01:
				i3 = text[i]; i++
				dispLow = i3
				fallthrough
			case mod == 0b00:
				opRM = Memory{mod: mod, rm: rm, dispHigh: dispHigh, dispLow: dispLow}
			case mod == 0b11:
				opRM = Register{name: rm, width: w}
			}
			inst := Instruction {
				offset: offset,
				size: i - offset,
				bytes: [6]byte{i1,i2,i3,i4},
				operation: OpAddRegRm,
				operands: nil,
			}
			if d == 0 { // from reg
				inst.operands = Operands{opRM, opReg}
			} else { // to reg
				inst.operands = Operands{opReg, opRM}
			}
			insts = append(insts, inst)
		case (i1 & 0b11111100) == 0b10000000:
			i2 := text[i]; i++
			bs := make([]byte, 0, 6)
			bs = append(bs, i1, i2)
			s, w := SW(i1)
			mod, zxz, rm := MODREGRM(i2)
			var op Operation
			switch zxz {
			default:
				err = errors.Join(err, fmt.Errorf("invalid bit pattern"))
			case 0b000:
				op = OpAddRmImm
			case 0b010:
				op = OpAdcRmImm
			case 0b101:
				op = OpSubRmImm
			case 0b011:
				op = OpSsbRmImm
			case 0b111:
				op = OpCmpRmImm
			}
			var opRM Operand
			dispHigh := byte(0)
			dispLow := byte(0)
			switch {
			case mod == 0b00 && rm == 0b110:
				fallthrough
			case mod == 0b10:
				dispHigh = text[i]; i++
				bs = append(bs, dispHigh)
				fallthrough
			case mod == 0b01:
				dispLow = text[i]; i++
				bs = append(bs, dispLow)
				fallthrough
			case mod == 0b00:
				opRM = Memory{mod: mod, rm: rm, dispHigh: dispHigh, dispLow: dispLow}
			case mod == 0b11:
				opRM = Register{name: rm, width: w}
			}
			data1 := text[i]; i++
			bs = append(bs, data1)
			// @fixme: s == 0 then don't sign extend [:sign-extend-s:]
			data := int16(data1)
			if s == 0 && w == 1 {
				data2 := text[i]; i++
				bs = append(bs, data2)
				data = (int16(data2) << 8) ^ data
			}
			// @fixme: brother eewww [:slice-to-array:]
			for len(bs) < 6 {
				bs = append(bs, 0)
			}
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [6]byte(bs),
				operation: op,
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
				bytes: [6]byte{i1,i2,i3},
				operation: OpAddAccImm,
				operands: Operands{
					Register{name: RegA, width: w},
					Immediate{width: w, value: data},
				},
			})
		case (i1 & 0b11111100) == 0b00010000:
			i2 := text[i]; i++
			i3 := byte(0)
			i4 := byte(0)
			d, w := DW(i1)
			mod, reg, rm := MODREGRM(i2)
			opReg := Register{name: reg, width: w}
			var opRM Operand
			dispHigh := byte(0)
			dispLow := byte(0)
			switch {
			case mod == 0b00 && rm == 0b110:
				fallthrough
			case mod == 0b10:
				i4 = text[i]; i++
				dispHigh = i4
				fallthrough
			case mod == 0b01:
				i3 = text[i]; i++
				dispLow = i3
				fallthrough
			case mod == 0b00:
				opRM = Memory{mod: mod, rm: rm, dispHigh: dispHigh, dispLow: dispLow}
			case mod == 0b11:
				opRM = Register{name: rm, width: w}
			}
			inst := Instruction {
				offset: offset,
				size: i - offset,
				bytes: [6]byte{i1,i2,i3,i4},
				operation: OpAdcRegRm,
				operands: nil,
			}
			if d == 0 { // from reg
				inst.operands = Operands{opRM, opReg}
			} else { // to reg
				inst.operands = Operands{opReg, opRM}
			}
			insts = append(insts, inst)
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
				bytes: [6]byte{i1,i2,i3},
				operation: OpAdcAccImm,
				operands: Operands{
					Register{name: RegA, width: w},
					Immediate{width: w, value: data},
				},
			})
		case (i1 & 0b11111110) == 0b11111110:
			i2 := text[i]; i++
			i3 := byte(0)
			i4 := byte(0)
			mod, zzx, rm := MODREGRM(i2)
			var op Operation
			switch zzx {
			default:
				err = errors.Join(err, fmt.Errorf("invalid bit pattern"))
			case 0b000:
				op = OpIncRm
			case 0b001:
				op = OpDecRm
			}
			w := W(i1)
			var opRM Operand
			dispHigh := byte(0)
			dispLow := byte(0)
			switch {
			case mod == 0b00 && rm == 0b110:
				fallthrough
			case mod == 0b10:
				i4 = text[i]; i++
				dispHigh = i4
				fallthrough
			case mod == 0b01:
				i3 = text[i]; i++
				dispLow = i3
				fallthrough
			case mod == 0b00:
				opRM = Memory{mod: mod, rm: rm, dispHigh: dispHigh, dispLow: dispLow}
			case mod == 0b11:
				opRM = Register{name: rm, width: w}
			}
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [6]byte{i1,i2,i3,i4},
				operation: op,
				operands: Operands{opRM},
			})
		case (i1 & 0b11111000) == 0b01000000:
			reg := REG(i1)
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [6]byte{i1},
				operation: OpIncReg,
				operands: Operands{
					Register{name: reg, width: 1},
				},
			})
		case i1 == 0b00110111:
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [6]byte{i1},
				operation: OpAAA,
				operands: nil,
			})
		case i1 == 0b00100111:
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [6]byte{i1},
				operation: OpBAA,
				operands: nil,
			})
		case (i1 & 0b11111100) == 0b00101000:
			i2 := text[i]; i++
			i3 := byte(0)
			i4 := byte(0)
			d, w := DW(i1)
			mod, reg, rm := MODREGRM(i2)
			opReg := Register{name: reg, width: w}
			var opRM Operand
			dispHigh := byte(0)
			dispLow := byte(0)
			switch {
			case mod == 0b00 && rm == 0b110:
				fallthrough
			case mod == 0b10:
				i4 = text[i]; i++
				dispHigh = i4
				fallthrough
			case mod == 0b01:
				i3 = text[i]; i++
				dispLow = i3
				fallthrough
			case mod == 0b00:
				opRM = Memory{mod: mod, rm: rm, dispHigh: dispHigh, dispLow: dispLow}
			case mod == 0b11:
				opRM = Register{name: rm, width: w}
			}
			inst := Instruction{
				offset: offset,
				size: i - offset,
				bytes: [6]byte{i1,i2,i3,i4},
				operation: OpSubRegRm,
				operands: nil,
			}
			if d == 0 { // from reg
				inst.operands = Operands{opRM, opReg}
			} else { // to reg
				inst.operands = Operands{opReg, opRM}
			}
			insts = append(insts, inst)
		case (i1 & 0b11111110) == 0b00101100:
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
				bytes: [6]byte{i1,i2,i3},
				operation: OpSubAccImm,
				operands: Operands{
					Register{name: RegA, width: w},
					Immediate{width: w, value: data},
				},
			})
		case (i1 & 0b11111100) == 0b00011000:
			i2 := text[i]; i++
			i3 := byte(0)
			i4 := byte(0)
			d, w := DW(i1)
			mod, reg, rm := MODREGRM(i2)
			opReg := Register{name: reg, width: w}
			var opRM Operand
			dispHigh := byte(0)
			dispLow := byte(0)
			switch {
			case mod == 0b00 && rm == 0b110:
				fallthrough
			case mod == 0b10:
				i4 = text[i]; i++
				dispHigh = i4
				fallthrough
			case mod == 0b01:
				i3 = text[i]; i++
				dispLow = i3
				fallthrough
			case mod == 0b00:
				opRM = Memory{mod: mod, rm: rm, dispHigh: dispHigh, dispLow: dispLow}
			case mod == 0b11:
				opRM = Register{name: rm, width: w}
			}
			inst := Instruction {
				offset: offset,
				size: i - offset,
				bytes: [6]byte{i1,i2,i3,i4},
				operation: OpSsbRegRm,
				operands: nil,
			}
			if d == 0 { // from reg
				inst.operands = Operands{opRM, opReg}
			} else { // to reg
				inst.operands = Operands{opReg, opRM}
			}
			insts = append(insts, inst)
		case (i1 & 0b11111100) == 0b00011000: // @fixme: reference seems to be missing a bit
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
				bytes: [6]byte{i1,i2,i3},
				operation: OpSsbAccImm,
				operands: Operands{
					Register{name: RegA, width: w},
					Immediate{width: w, value: data},
				},
			})
		case (i1 & 0b11111000) == 0b01001000:
			reg := REG1(i1)
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [6]byte{i1},
				operation: OpDecReg,
				operands: Operands{
					Register{name: reg, width: 1},
				},
			})
		case (i1 & 0b11111110) == 0b11110110:
			i2 := text[i]; i++
			i3 := byte(0)
			i4 := byte(0)
			w := W(i1)
			mod, xxx, rm := MODREGRM(i2)
			var op Operation
			switch xxx {
			default:
				err = errors.Join(err, fmt.Errorf("invalid bit pattern"))
			case 0b000:
				op = OpTestRmImm
				// @fixme(test): has one or two data bytes!!!
			case 0b010:
				op = OpNot
			case 0b011:
				op = OpNeg
			case 0b100:
				op = OpMul
			case 0b101:
				op = OpImul
			case 0b110:
				op = OpDiv
			case 0b111:
				op = OpIdiv
			}
			var opRM Operand
			dispHigh := byte(0)
			dispLow := byte(0)
			switch {
			case mod == 0b00 && rm == 0b110:
				fallthrough
			case mod == 0b10:
				i4 = text[i]; i++
				dispHigh = i4
				fallthrough
			case mod == 0b01:
				i3 = text[i]; i++
				dispLow = i3
				fallthrough
			case mod == 0b00:
				opRM = Memory{mod: mod, rm: rm, dispHigh: dispHigh, dispLow: dispLow}
			case mod == 0b11:
				opRM = Register{name: rm, width: w}
			}
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [6]byte{i1,i2,i3,i4},
				operation: op,
				operands: Operands{
					opRM,
				},
			})
		case (i1 & 0b11111100) == 0b00111000:
			i2 := text[i]; i++
			i3 := byte(0)
			i4 := byte(0)
			d, w := DW(i1)
			mod, reg, rm := MODREGRM(i2)
			opReg := Register{name: reg, width: w}
			var opRM Operand
			dispHigh := byte(0)
			dispLow := byte(0)
			switch {
			case mod == 0b00 && rm == 0b110:
				fallthrough
			case mod == 0b10:
				i4 = text[i]; i++
				dispHigh = i4
				fallthrough
			case mod == 0b01:
				i3 = text[i]; i++
				dispLow = i3
				fallthrough
			case mod == 0b00:
				opRM = Memory{mod: mod, rm: rm, dispHigh: dispHigh, dispLow: dispLow}
			case mod == 0b11:
				opRM = Register{name: rm, width: w}
			}
			inst := Instruction {
				offset: offset,
				size: i - offset,
				bytes: [6]byte{i1,i2,i3,i4},
				operation: OpCmpRegRm,
				operands: nil,
			}
			if d == 0 { // from reg
				inst.operands = Operands{opRM, opReg}
			} else { // to reg
				inst.operands = Operands{opReg, opRM}
			}
			insts = append(insts, inst)
		case (i1 & 0b11111110) == 0b00111100:
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
				bytes: [6]byte{i1,i2,i3},
				operation: OpCmpAccImm,
				operands: Operands{
					Register{name: RegA, width: w},
					Immediate{width: w, value: data},
				},
			})
		case i1 == 0b00111111:
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [6]byte{i1},
				operation: OpAAS,
				operands: nil,
			})
		case i1 == 0b00101111:
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [6]byte{i1},
				operation: OpDAS,
				operands: nil,
			})
		case i1 == 0b11010100:
			i2 := text[i]; i++
			if i2 != 0b00001010 {
				err = errors.Join(err, fmt.Errorf("unexpected bit pattern"))
			}
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [6]byte{i1,i2},
				operation: OpAAM,
				operands: nil,
			})
		case i1 == 0b11010101:
			i2 := text[i]; i++
			if i2 != 0b00001010 {
				err = errors.Join(err, fmt.Errorf("unexpected bit pattern"))
			}
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [6]byte{i1,i2},
				operation: OpAAD,
				operands: nil,
			})
		case i1 == 0b10011000:
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [6]byte{i1},
				operation: OpCBW,
				operands: nil,
			})
		case i1 == 0b10011001:
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [6]byte{i1},
				operation: OpCWD,
				operands: nil,
			})
		case (i1 & 0b11111100) == 0b11010000:
			i2 := text[i]; i++
			i3 := byte(0)
			i4 := byte(0)
			v, w := VW(i1)
			mod, xxx, rm := MODREGRM(i2)
			var op Operation
			switch xxx {
			default:
				err = errors.Join(err, fmt.Errorf("invalid bit pattern"))
			case 0b100:
				op = OpShlSal
			case 0b101:
				op = OpShr
			case 0b111:
				op = OpSar
			case 0b000:
				op = OpRol
			case 0b001:
				op = OpRor
			case 0b010:
				op = OpRcl
			case 0b011:
				op = OpRcr
			}
			var opRM Operand
			dispHigh := byte(0)
			dispLow := byte(0)
			switch {
			case mod == 0b00 && rm == 0b110:
				fallthrough
			case mod == 0b10:
				i4 = text[i]; i++
				dispHigh = i4
				fallthrough
			case mod == 0b01:
				i3 = text[i]; i++
				dispLow = i3
				fallthrough
			case mod == 0b00:
				opRM = Memory{mod: mod, rm: rm, dispHigh: dispHigh, dispLow: dispLow}
			case mod == 0b11:
				opRM = Register{name: rm, width: w}
			}
			inst := Instruction {
				offset: offset,
				size: i - offset,
				bytes: [6]byte{i1,i2,i3,i4},
				operation: op,
				operands: nil,
			}
			if v == 0 { // count is 1
				inst.operands = Operands{opRM, Immediate{width: 0, value: 1}}
			} else { // count is in CL
				inst.operands = Operands{opRM, Register{name: RegC, width: 0}}
			}
			insts = append(insts, inst)
		case (i1 & 0b11111100) == 0b00100000:
			i2 := text[i]; i++
			i3 := byte(0)
			i4 := byte(0)
			d, w := DW(i1)
			mod, reg, rm := MODREGRM(i2)
			opReg := Register{name: reg, width: w}
			var opRM Operand
			dispHigh := byte(0)
			dispLow := byte(0)
			switch {
			case mod == 0b00 && rm == 0b110:
				fallthrough
			case mod == 0b10:
				i4 = text[i]; i++
				dispHigh = i4
				fallthrough
			case mod == 0b01:
				i3 = text[i]; i++
				dispLow = i3
				fallthrough
			case mod == 0b00:
				opRM = Memory{mod: mod, rm: rm, dispHigh: dispHigh, dispLow: dispLow}
			case mod == 0b11:
				opRM = Register{name: rm, width: w}
			}
			inst := Instruction {
				offset: offset,
				size: i - offset,
				bytes: [6]byte{i1,i2,i3,i4},
				operation: OpAndRegRm,
				operands: nil,
			}
			if d == 0 { // from reg
				inst.operands = Operands{opRM, opReg}
			} else { // to reg
				inst.operands = Operands{opReg, opRM}
			}
			insts = append(insts, inst)
		case (i1 & 0b11111110) == 0b0000000:
			// @todo
			i2 := text[i]; i++
			bs := make([]byte, 0, 6)
			bs = append(bs, i1, i2)
			w := W(i1)
			mod, xxx, rm := MODREGRM(i2)
			var op Operation
			switch xxx {
			default:
				err = errors.Join(err, fmt.Errorf("unexpected bit pattern"))
			case 0b100:
				op = OpAndRmImm
			}
			var opRM Operand
			dispHigh := byte(0)
			dispLow := byte(0)
			switch {
			case mod == 0b00 && rm == 0b110:
				fallthrough
			case mod == 0b10:
				dispHigh = text[i]; i++
				bs = append(bs, dispHigh)
				fallthrough
			case mod == 0b01:
				dispLow = text[i]; i++
				bs = append(bs, dispLow)
				fallthrough
			case mod == 0b00:
				opRM = Memory{mod: mod, rm: rm, dispHigh: dispHigh, dispLow: dispLow}
			case mod == 0b11:
				opRM = Register{name: rm, width: w}
			}
			data1 := text[i]; i++
			bs = append(bs, data1)
			data := int16(data1)
			if w == 1 {
				data2 := text[i]; i++
				bs = append(bs, data2)
				data = (int16(data2) << 8) ^ data
			}
			// @fixme: brother eewww [:slice-to-array:]
			for len(bs) < 6 {
				bs = append(bs, 0)
			}
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [6]byte(bs),
				operation: op,
				operands: Operands{
					opRM,
					Immediate{width: w, value: data},
				},
			})
		case (i1 & 0b11111110) == 0b00100100:
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
				bytes: [6]byte{i1,i2,i3},
				operation: OpAndAccImm,
				operands: Operands{
					Register{name: RegA, width: w},
					Immediate{width: w, value: data},
				},
			})
		case i1 == 0b11001100:
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [6]byte{i1},
				operation: OpIntType3,
				operands: nil,
			})
		case i1 == 0b11001101:
			i2 := text[i]; i++
			insts = append(insts, Instruction {
				offset: offset,
				size: i - offset,
				bytes: [6]byte{i1,i2},
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
