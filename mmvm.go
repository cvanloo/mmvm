package main

import (
	//"unsafe"
	"fmt"
	"os"
	"strings"
	"errors"
	"encoding/binary"
	"bytes"
	"log"
	"flag"
)

// @fixme: sign extension is only perform by doing a double cast int16(int8(byte))
// @todo: AT&T syntax printing

type (
	Exec struct {
		DontCareForNow int64
		SizeText int32
		SizeData int32
		SizeBSS int32
		EntryPoint int32
		MemTotal int32
		SizeSym int32
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
	Repeated struct {
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
	// Segment:Offset
	SegmentOffset struct {
		segment, offset int16
	}
	Immediate struct {
		width byte
		value uint16
	}
	SignedImmediate struct {
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
	OpTestRegRm
	OpTestRmImm
	OpTestAccImm
	OpOrRegRm
	OpOrRmImm
	OpOrAccImm
	OpXorRegRm
	OpXorRmImm
	OpXorAccImm
	OpRep
	OpMovsb
	OpCmpsb
	OpScasb
	OpLodsb
	OpStosb
	OpMovsw
	OpCmpsw
	OpScasw
	OpLodsw
	OpStosw
	OpCallDirSeg
	OpCallIndirSeg
	OpCallDirInterSeg
	OpCallIndirInterSeg
	OpJmpDirSeg
	OpJmpShortDirSeg
	OpJmpIndirSeg
	OpJmpDirInterSeg
	OpJmpIndirInterSeg
	OpRetSeg
	OpRetSegImm
	OpRetInterSeg
	OpRetInterSegImm
	OpJe
	OpJl
	OpJle
	OpJb
	OpJbe
	OpJp
	OpJo
	OpJs
	OpJne
	OpJnl
	OpJnle
	OpJnb
	OpJnbe
	OpJnp
	OpJno
	OpJns
	OpLoop
	OpLoopz
	OpLoopnz
	OpJcxz
	OpIntType3
	OpIntTypeSpecified
	OpInto
	OpIret
	OpClc
	OpCmc
	OpStc
	OpCld
	OpStd
	OpCli
	OpSti
	OpHlt
	OpWait
	OpEsc
	OpLock
)

func (op Operation) Description() string {
	switch op {
	default:
		panic("unknown operation")
	case OpInvalid:
		panic("invalid operation")
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
	case OpTestRegRm:
		return "TEST Register/Memory and Register"
	case OpTestRmImm:
		return "TEST Immediate Data and Register/Memory"
	case OpTestAccImm:
		return "TEST Immediate Data and Accumulator"
	case OpOrRegRm:
		return "OR Register/Memory and Register to Either"
	case OpOrRmImm:
		return "OR Immediate to Register/Memory"
	case OpOrAccImm:
		return "OR Immediate to Accumulator"
	case OpXorRegRm:
		return "XOR Register/Memory and Register to Either"
	case OpXorRmImm:
		return "XOR Immediate to Register/Memory"
	case OpXorAccImm:
		return "XOR Immediate to Accumulator"
	case OpRep:
		return "REP Repeat"
	case OpMovsb, OpMovsw:
		return "MOVS Move Byte/Word"
	case OpCmpsb, OpCmpsw:
		return "CMPS Compare Byte/Word"
	case OpScasb, OpScasw:
		return "SCAS Scan Byte/Word"
	case OpLodsb, OpLodsw:
		return "LODS Load Byte/Word to AL/AX"
	case OpStosb, OpStosw:
		return "STOS Store Byte/Word from AL/AX"
	case OpCallDirSeg:
		return "CALL Direct within Segment"
	case OpCallIndirSeg:
		return "CALL Indirect within Segment"
	case OpCallDirInterSeg:
		return "CALL Direct Intersegment"
	case OpCallIndirInterSeg:
		return "CALL Indirect Intersegment"
	case OpJmpDirSeg:
		return "JMP Direct within Segment"
	case OpJmpShortDirSeg:
		return "JMP Direct within Segment-Short"
	case OpJmpIndirSeg:
		return "JMP Indirect within Segment"
	case OpJmpDirInterSeg:
		return "JMP Direct Intersegment"
	case OpJmpIndirInterSeg:
		return "JMP Indirect Intersegment"
	case OpRetSeg:
		return "RET Within Segment"
	case OpRetSegImm:
		return "RET Within Segment Adding Immediate to SP"
	case OpRetInterSeg:
		return "RET Intersegment"
	case OpRetInterSegImm:
		return "RET Intersegment Adding Immediate to SP"
	case OpJe:
		return "JE/JZ Jump on Equal/Zero"
	case OpJl:
		return "JL/JNGE Jump on Less/Not Greater or Equal"
	case OpJle:
		return "JLE/JNG Jump on Less or Equal/Not Greater"
	case OpJb:
		return "JB/JNAE Jump on Below/Not Above or Equal"
	case OpJbe:
		return "JBE/JNA Jump on Below or Equal/Not Above"
	case OpJp:
		return "JP/JPE Jump on Parity/Parity Even"
	case OpJo:
		return "JO Jump on Overflow"
	case OpJs:
		return "JS Jump on Sign"
	case OpJne:
		return "JNE/JNZ Jump on Not Equal/Not Zero"
	case OpJnl:
		return "JNL/JGE Jump on Not Less/Greater or Equal"
	case OpJnle:
		return "JNLE/JG Jump on Not Less or Equal/Greater"
	case OpJnb:
		return "JNB/JAE Jump on Not Below/Above or Equal"
	case OpJnbe:
		return "JNBE/JA Jump on Not Below or Equal/Above"
	case OpJnp:
		return "JNP/JPO Jump on Not Parity/Parity Odd"
	case OpJno:
		return "JNO Jump on Not Overflow"
	case OpJns:
		return "JNS Jump on Not Sign"
	case OpLoop:
		return "LOOP Loop CX Times"
	case OpLoopz:
		return "LOOPZ/LOOPE Loop While Zero/Equal"
	case OpLoopnz:
		return "LOOPNZ/LOOPNE Loop While Not Zero/Equal"
	case OpJcxz:
		return "JCXZ Jump on CX Zero"
	case OpIntType3:
		return "INT Type 3"
	case OpIntTypeSpecified:
		return "INT Type Specified"
	case OpInto:
		return "INTO Interrupt on Overflow"
	case OpIret:
		return "IRET Interrupt Return"
	case OpClc:
		return "CLC Clear Carry"
	case OpCmc:
		return "CMC Complement Carry"
	case OpStc:
		return "STC Set Carry"
	case OpCld:
		return "CLD Clear Direction"
	case OpStd:
		return "STD Set Direction"
	case OpCli:
		return "CLI Clear Interrupt"
	case OpSti:
		return "STI Set Interrupt"
	case OpHlt:
		return "HLT Halt"
	case OpWait:
		return "WAIT Wait"
	case OpEsc:
		return "ESC Escape (to External Device)"
	case OpLock:
		return "LOCK Bus Lock Prefix"
	}
}

func (op Operation) String() string {
	switch op {
	default:
		panic("unknown operation")
	case OpInvalid:
		panic("invalid operation")
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
		return "sbb"
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
	case OpTestRegRm, OpTestRmImm, OpTestAccImm:
		return "test"
	case OpOrRegRm, OpOrRmImm, OpOrAccImm:
		return "or"
	case OpXorRegRm, OpXorRmImm, OpXorAccImm:
		return "xor"
	case OpRep:
		return "rep"
	case OpMovsb:
		return "movsb"
	case OpCmpsb:
		return "cmpsb"
	case OpScasb:
		return "scasb"
	case OpLodsb:
		return "lodsb"
	case OpStosb:
		return "stosb"
	case OpMovsw:
		return "movsw"
	case OpCmpsw:
		return "cmpsw"
	case OpScasw:
		return "scasw"
	case OpLodsw:
		return "lodsw"
	case OpStosw:
		return "stosw"
	case OpJmpDirSeg, OpJmpIndirSeg, OpJmpDirInterSeg, OpJmpIndirInterSeg:
		return "jmp"
	case OpJmpShortDirSeg:
		return "jmp short"
	case OpCallDirSeg, OpCallIndirSeg, OpCallDirInterSeg, OpCallIndirInterSeg:
		return "call"
	case OpRetSeg, OpRetSegImm, OpRetInterSeg, OpRetInterSegImm:
		return "ret"
	case OpJe:
		return "je"
	case OpJl:
		return "jl"
	case OpJle:
		return "jle"
	case OpJb:
		return "jb"
	case OpJbe:
		return "jbe"
	case OpJp:
		return "jp"
	case OpJo:
		return "jo"
	case OpJs:
		return "js"
	case OpJne:
		return "jne"
	case OpJnl:
		return "jnl"
	case OpJnle:
		return "jnle"
	case OpJnb:
		return "jnb"
	case OpJnbe:
		return "jnbe"
	case OpJnp:
		return "jnp"
	case OpJno:
		return "jno"
	case OpJns:
		return "jns"
	case OpLoop:
		return "loop"
	case OpLoopz:
		return "loopz"
	case OpLoopnz:
		return "loopnz"
	case OpJcxz:
		return "jcxz"
	case OpIntType3, OpIntTypeSpecified:
		return "int"
	case OpInto:
		return "into"
	case OpIret:
		return "iret"
	case OpClc:
		return "clc"
	case OpCmc:
		return "cmc"
	case OpStc:
		return "stc"
	case OpCld:
		return "cld"
	case OpStd:
		return "std"
	case OpCli:
		return "cli"
	case OpSti:
		return "sti"
	case OpHlt:
		return "hlt"
	case OpWait:
		return "wait"
	case OpEsc:
		return "esc"
	case OpLock:
		return "lock"
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

func (reg Register) IsSegment() bool {
	return reg.name >= 0 && reg.name <= 3
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
			return fmt.Sprintf("[%04x]", disp)
		}
		return "[" + names[mem.rm] + "]"
	case 0b01:
		return fmt.Sprintf("[%s%+x]", names[mem.rm], int16(int8(mem.dispLow)))
	case 0b10:
		disp := (int16(mem.dispHigh) << 8) ^ int16(mem.dispLow)
		return fmt.Sprintf("[%s%+x]", names[mem.rm], disp)
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

func (so SegmentOffset) String() string {
	return so.AsmString()
}

func (so SegmentOffset) AsmString() string {
	return fmt.Sprintf("%04x:%04x", so.segment, so.offset)
}

func (imm Immediate) String() string {
	return imm.AsmString()
}

func (imm Immediate) AsmString() string {
	return fmt.Sprintf("%0[1]*x", (imm.width)*4, imm.value)
}

func (imm SignedImmediate) String() string {
	return imm.AsmString()
}

func (imm SignedImmediate) AsmString() string {
	return fmt.Sprintf("%x", imm.value)
}

func (r Repeated) String() string {
	return r.AsmString()
}

func (r Repeated) AsmString() string {
	if len(r.operands) > 0 {
		return fmt.Sprintf("%s %s", r.operation, r.operands)
	} else {
		return fmt.Sprintf("%s", r.operation)
	}
}

func (inst Instruction) String() string {
	sizeSpecifier := func() string {
		if len(inst.operands) >= 2 {
			switch inst.operands[0].(type) {
			case Memory:
				switch imm := inst.operands[1].(type) {
				case Immediate:
					if imm.width == 0 {
						return " byte"
					}
				case SignedImmediate:
					if imm.width == 0 {
						return " byte"
					}
				}
			}
		}
		return ""
	}
	if len(inst.operands) > 0 {
		return fmt.Sprintf("%04x: %-13x %s%s %s", inst.offset, inst.bytes[:inst.size], inst.operation, sizeSpecifier(), inst.operands)
	} else {
		return fmt.Sprintf("%04x: %-13x %s", inst.offset, inst.bytes[:inst.size], inst.operation)
	}
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

type Source struct {
	Text []byte
	Consumed, Pos int
}

func (s *Source) Peek() byte {
	return s.Text[s.Pos]
}

func (s *Source) Next() (b byte) {
	b = s.Text[s.Pos]
	s.Pos += 1
	return b
}

func (s *Source) B(i int) (b byte) {
	return s.Text[s.Consumed+i]
}

func (s *Source) Consume() (bs []byte, offset, size int) {
	offset = s.Consumed
	size = s.Pos - s.Consumed
	bs = s.Text[s.Consumed:s.Pos]
	s.Consumed = s.Pos
	return
}

func (s *Source) IsEnd() bool {
	return s.Pos >= len(s.Text)
}

func decodeImmediate(src *Source, width byte) Immediate {
	var data uint16
	if width == 1 {
		b1 := src.Next()
		b2 := src.Next()
		data = (uint16(b2) << 8) ^ uint16(b1)
	} else {
		b1 := src.Next()
		//data = uint16(int16(int8(b1)))
		data = uint16(b1)
	}
	return Immediate{width: width, value: data} // @fixme: ?u?int16
}

func decodeSignedImmediate(src *Source, width byte) SignedImmediate {
	var data int16
	if width == 1 {
		b1 := src.Next()
		b2 := src.Next()
		data = (int16(b2) << 8) ^ int16(b1)
	} else {
		b1 := src.Next()
		data = int16(int8(b1))
	}
	return SignedImmediate{width: width, value: data}
}

func decodeDisposition(src *Source) Immediate {
	b1 := src.Next()
	b2 := src.Next()
	disp := (uint16(b2) << 8) ^ uint16(b1)
	dispTotal := uint16(src.Pos) + disp
	return Immediate{width: 1, value: dispTotal}
}

func decodeDispositionShort(src *Source) Immediate {
	disp := uint16(int16(int8(src.Next())))
	dispTotal := uint16(src.Pos) + disp
	return Immediate{width: 1, value: dispTotal}
}

func decodeAddress(src *Source, width byte) Address {
	// @fixme: instruction formatting????
	//        mov byte opn1, opn2
	//        mov word opn1, opn2
	b1 := src.Next()
	b2 := src.Next()
	addr := (uint16(b2) << 8) ^ uint16(b1)
	return Address{width: width, addr: int16(addr)} // @fixme: ?u?int16
}

func decodeDirectIntersegment(src *Source) SegmentOffset {
	o1 := src.Next()
	o2 := src.Next()
	offset := (int16(o2) << 8) ^ int16(o1)
	s1 := src.Next()
	s2 := src.Next()
	segment := (int16(s2) << 8) ^ int16(s1)
	return SegmentOffset{segment, offset}
}

func decodeModRegRm(src *Source, width byte) (oreg, orm Operand) {
	mod, reg, rm := MODREGRM(src.Next())
	oreg = Register{name: reg, width: width}
	switch mod {
	case 0b00:
		if rm == 0b110 {
			orm = Memory{mod: mod, rm: rm, dispLow: src.Next(), dispHigh: src.Next()}
		} else {
			orm = Memory{mod: mod, rm: rm}
		}
	case 0b01:
		orm = Memory{mod: mod, rm: rm, dispHigh: 0, dispLow: src.Next()}
	case 0b10:
		// @fixme: will this code execute in the right order?
		orm = Memory{mod: mod, rm: rm, dispLow: src.Next(), dispHigh: src.Next()}
	case 0b11:
		orm = Register{name: rm, width: width}
	}
	return oreg, orm
}

func decodeModSegRm(src *Source, width byte) (oreg, orm Operand) {
	mod, reg, rm := MODREGRM(src.Next())
	if reg & 0b100 != 0 {
		// @todo: error handling
		reg = reg & 0b011
	}
	oreg = Segment{name: reg}
	switch mod {
	case 0b00:
		if rm == 0b110 {
			orm = Memory{mod: mod, rm: rm, dispLow: src.Next(), dispHigh: src.Next()}
		} else {
			orm = Memory{mod: mod, rm: rm}
		}
	case 0b01:
		orm = Memory{mod: mod, rm: rm, dispHigh: 0, dispLow: src.Next()}
	case 0b10:
		// @fixme: will this code execute in the right order?
		orm = Memory{mod: mod, rm: rm, dispLow: src.Next(), dispHigh: src.Next()}
	case 0b11:
		orm = Register{name: rm, width: width}
	}
	return oreg, orm
}

func srcdst(d byte, op1, op2 Operand) Operands {
	if d == 0 {
		return Operands{op1, op2}
	} else {
		return Operands{op2, op1}
	}
}

var ErrDecode = errors.New("decode error")

func decodeRepeated(src *Source) (repd Repeated, err error) {
	var (
		op Operation
		opn Operands
	)
	switch i1 := src.Next(); {
	default:
		err = fmt.Errorf("unrecognized opcode: %02x", i1)
	case i1 == 0b10101111: // scasw
		op = OpScasw
	case i1 == 0b10101110: // scasb
		op = OpScasb
	case i1 == 0b10101101: // lodsw
		op = OpLodsw
	case i1 == 0b10101100: // lodsb
		op = OpLodsb
	case i1 == 0b10101011: // stosw
		op = OpStosw
	case i1 == 0b10101010: // stosb
		op = OpStosb
	case i1 == 0b10100111: // cmpsw
		op = OpCmpsw
	case i1 == 0b10100110: // cmpsb
		op = OpCmpsb
	case i1 == 0b10100101: // movsw
		op = OpMovsw
	case i1 == 0b10100100: // movsb
		op = OpMovsb
	}
	return Repeated {
		operation: op,
		operands: opn,
	}, err
}

func decode(src *Source) (inst Instruction, err error) {
	var (
		op Operation
		opn Operands
	)
	switch i1 := src.Next(); {
	default:
		err = fmt.Errorf("unrecognized opcode: %02x", i1)
	case i1 == 0b11111101: // std
		op = OpStd
	case i1 == 0b11111100: // cld
		op = OpCld
	case i1 == 0b11111011: // sti
		op = OpSti
	case i1 == 0b11111010: // cli
		op = OpCli
	case i1 == 0b11111001: // stc
		op = OpStc
	case i1 == 0b11111000: // clc
		op = OpClc
	case i1 == 0b11110101: // cmc
		op = OpCmc
	case i1 == 0b11110100: // hlt
		op = OpHlt
	case i1 == 0b11110000: // lock
		op = OpLock
	case i1 == 0b11101011: // jmp short ???
		disp := decodeDispositionShort(src)
		op = OpJmpShortDirSeg
		opn = Operands{disp}
	case i1 == 0b11101010:
		so := decodeDirectIntersegment(src)
		op = OpJmpDirInterSeg
		opn = Operands{so}
	case i1 == 0b11101001: // jmp short label
		disp := decodeDisposition(src)
		op = OpJmpDirSeg
		opn = Operands{disp}
	case i1 == 0b11101000: // call short label
		disp := decodeDisposition(src)
		op = OpCallDirSeg
		opn = Operands{disp}
	case i1 == 0b11100011: // jcxz ???
		disp := decodeDispositionShort(src)
		op = OpJcxz
		opn = Operands{disp}
	case i1 == 0b11100010: // loop ???
		disp := decodeDispositionShort(src)
		op = OpLoop
		opn = Operands{disp}
	case i1 == 0b11100001: // loopz ???
		disp := decodeDispositionShort(src)
		op = OpLoopz
		opn = Operands{disp}
	case i1 == 0b11100000: // loopnz ???
		disp := decodeDispositionShort(src)
		op = OpLoopnz
		opn = Operands{disp}
	case i1 == 0b11010111: // xlat
		op = OpXLAT
	case i1 == 0b11010101: // aad
		i2 := src.Next()
		if i2 != 0b00001010 {
			err = ErrDecode
		}
		op = OpAAD
	case i1 == 0b11010100: // aam
		i2 := src.Next()
		if i2 != 0b00001010 {
			err = ErrDecode
		}
		op = OpAAM
	case i1 == 0b11001111: // iret
		op = OpIret
	case i1 == 0b11001110: // into
		op = OpInto
	case i1 == 0b11001101: // int type
		op = OpIntTypeSpecified
		opn = Operands{Immediate{width: 0, value: uint16(src.Next())}}
	case i1 == 0b11001100: // int 3
		op = OpIntType3
	case i1 == 0b11001011: // ret ???
		op = OpRetInterSeg
	case i1 == 0b11001010: // ret @fixme: ???
		data := decodeImmediate(src, 1)
		op = OpRetInterSegImm
		opn = Operands{data}
	case i1 == 0b11000101: // lds reg, rm
		reg, rm := decodeModRegRm(src, 1)
		op = OpLDS
		opn = Operands{reg, rm}
	case i1 == 0b11000100: // les reg, rm
		reg, rm := decodeModRegRm(src, 1)
		op = OpLES
		opn = Operands{reg, rm}
	case i1 == 0b11000011: // ret ???
		op = OpRetSeg
	case i1 == 0b11000010: // ret imm
		data := decodeImmediate(src, 1)
		op = OpRetSegImm
		opn = Operands{data}
	case i1 == 0b10101111: // scasw
		op = OpScasw
	case i1 == 0b10101110: // scasb
		op = OpScasb
	case i1 == 0b10101101: // lodsw
		op = OpLodsw
	case i1 == 0b10101100: // lodsb
		op = OpLodsb
	case i1 == 0b10101011: // stosw
		op = OpStosw
	case i1 == 0b10101010: // stosb
		op = OpStosb
	case i1 == 0b10100111: // cmpsw
		op = OpCmpsw
	case i1 == 0b10100110: // cmpsb
		op = OpCmpsb
	case i1 == 0b10100101: // movsw
		op = OpMovsw
	case i1 == 0b10100100: // movsb
		op = OpMovsb
	case i1 == 0b10100010: // mov mem, ax // mov mem, al
		w := W(i1)
		addr := decodeAddress(src, w)
		op = OpMovMemAcc
		opn = Operands{addr, Register{name: RegA, width: w}}
	case i1 == 0b10011111: // lahf
		op = OpLAHF
	case i1 == 0b10011110: // sahf
		op = OpSAHF
	case i1 == 0b10011101: // popf
		op = OpPOPF
	case i1 == 0b10011100: // pushf
		op = OpPUSHF
	case i1 == 0b10011011: // wait
		op = OpWait
	case i1 == 0b10011010: // call segment:offset
		so := decodeDirectIntersegment(src)
		op = OpCallDirInterSeg
		opn = Operands{so}
	case i1 == 0b10011001: // cwd
		op = OpCWD
	case i1 == 0b10011000: // cbw
		op = OpCBW
	case i1 == 0b10001111: // pop rm
		_, rm := decodeModRegRm(src, 1) // @todo: we *could* verify that the reg part is 000 [:reg-000:]
		op = OpPopRm
		opn = Operands{rm}
	case i1 == 0b10001101: // lea reg, rm
		reg, rm := decodeModRegRm(src, 1)
		op = OpLEA
		opn = Operands{reg, rm}
	case i1 == 0b01111111: // jnle ???
		disp := decodeDispositionShort(src)
		op = OpJnle
		opn = Operands{disp}
	case i1 == 0b01111110: // jle ???
		disp := decodeDispositionShort(src)
		op = OpJle
		opn = Operands{disp}
	case i1 == 0b01111101: // jnl ???
		disp := decodeDispositionShort(src)
		op = OpJnl
		opn = Operands{disp}
	case i1 == 0b01111100: // jl ???
		disp := decodeDispositionShort(src)
		op = OpJl
		opn = Operands{disp}
	case i1 == 0b01111011: // jnp ???
		disp := decodeDispositionShort(src)
		op = OpJnp
		opn = Operands{disp}
	case i1 == 0b01111010: // jp ???
		disp := decodeDispositionShort(src)
		op = OpJp
		opn = Operands{disp}
	case i1 == 0b01111001: // jns ???
		disp := decodeDispositionShort(src)
		op = OpJns
		opn = Operands{disp}
	case i1 == 0b01111000: // js ???
		disp := decodeDispositionShort(src)
		op = OpJs
		opn = Operands{disp}
	case i1 == 0b01110111: // jnbe ???
		disp := decodeDispositionShort(src)
		op = OpJnbe
		opn = Operands{disp}
	case i1 == 0b01110110: // jbe ???
		disp := decodeDispositionShort(src)
		op = OpJbe
		opn = Operands{disp}
	case i1 == 0b01110101: // jne ???
		disp := decodeDispositionShort(src)
		op = OpJne
		opn = Operands{disp}
	case i1 == 0b01110100: // je ???
		disp := decodeDispositionShort(src)
		op = OpJe
		opn = Operands{disp}
	case i1 == 0b01110011: // jnb ???
		disp := decodeDispositionShort(src)
		op = OpJnb
		opn = Operands{disp}
	case i1 == 0b01110010: // jb ???
		disp := decodeDispositionShort(src)
		op = OpJb
		opn = Operands{disp}
	case i1 == 0b01110001: // jno ???
		disp := decodeDispositionShort(src)
		op = OpJno
		opn = Operands{disp}
	case i1 == 0b01110000: // jo ???
		disp := decodeDispositionShort(src)
		op = OpJo
		opn = Operands{disp}
	case i1 == 0b00111111: // aas
		op = OpAAS
	case i1 == 0b00110111: // aaa
		op = OpAAA
	case i1 == 0b00101111: // das
		op = OpDAS
	case i1 == 0b00100111: // baa
		op = OpBAA
	case (i1 & 0b11111110) == 0b11111110: // inc rm // dec rm // call rm ??? // jmp rm ??? // push rm
		w := W(i1)
		_, rm := decodeModRegRm(src, w)
		// @todo: validate that in all cases except for IncRm and DecRm w is always 1
		op = map[byte]Operation{
			0b000: OpIncRm,
			0b001: OpDecRm,
			0b010: OpCallIndirSeg,
			0b011: OpCallIndirInterSeg,
			0b100: OpJmpIndirSeg,
			0b101: OpJmpIndirInterSeg,
			0b110: OpPushRm,
		}[REG(src.B(1))]
		opn = Operands{rm}
	case (i1 & 0b11111110) == 0b11110110: // {test,not,neg,mul,imul,div,idiv} ???
		w := W(i1)
		_, rm := decodeModRegRm(src, w)
		op = map[byte]Operation{
			0b000: OpTestRmImm,
			0b010: OpNot,
			0b011: OpNeg,
			0b100: OpMul,
			0b101: OpImul,
			0b110: OpDiv,
			0b111: OpIdiv,
		}[REG(src.B(1))]
		opn = Operands{rm}
		if op == OpTestRmImm {
			if w == 1 {
				opn = append(opn, decodeImmediate(src, w))
			} else {
				opn = append(opn, decodeSignedImmediate(src, w))
			}
		}
	case (i1 & 0b11111110) == 0b11110010: // rep <string instruction>
		z := W(i1)
		op = OpRep
		var repd Operand
		repd, err = decodeRepeated(src)
		opn = Operands{repd}
		_ = z // @fixme: the heck is z?
	case (i1 & 0b11111110) == 0b11101110: // out ???
		//w := W(i1)
		op = OpOutVarPort
		// @fixme: operands?
	case (i1 & 0b11111110) == 0b11101100: // in ax, ds // in al, ds
		w := W(i1)
		op = OpInVarPort
		opn = Operands{Register{name: RegA, width: w}, Register{name: RegD, width: 1}}
	case (i1 & 0b11111110) == 0b11100110: // out ax, imm // out al, imm
		w := W(i1)
		port := uint16(src.Next()) // don't sign extend
		op = OpOutFixedPort
		opn = Operands{Register{name: RegA, width: w}, Immediate{width: w, value: port}}
	case (i1 & 0b11111110) == 0b11100100: // in ax, imm // in al, imm
		w := W(i1)
		port := int16(src.Next()) // don't sign extend
		op = OpInFixedPort
		opn = Operands{Register{name: RegA, width: w}, SignedImmediate{width: w, value: port}}
	case (i1 & 0b11111110) == 0b11000110: // mov rm, imm
		w := W(i1)
		_, rm := decodeModRegRm(src, w) // @todo: we *could* verify that reg part is 000 [:reg-000:]
		imm := decodeImmediate(src, w)
		op = OpMovRmImm
		opn = Operands{rm, imm}
	case (i1 & 0b11111110) == 0b10101000: // test ax, imm
		w := W(i1)
		imm := decodeImmediate(src, w)
		op = OpTestAccImm
		opn = Operands{Register{name: RegA, width: w}, imm}
	case (i1 & 0b11111110) == 0b10100000: // mov ax, addr // mov al, addr
		w := W(i1)
		addr := decodeAddress(src, w)
		op = OpMovAccMem
		opn = Operands{Register{name: RegA, width: w}, addr}
	case (i1 & 0b11111110) == 0b10000110: // xchg rm, reg
		w := W(i1)
		reg, rm := decodeModRegRm(src, w)
		op = OpXchgRmReg
		opn = Operands{rm, reg}
	case (i1 & 0b11111110) == 0b10000100: // test reg, rm
		w := W(i1)
		reg, rm := decodeModRegRm(src, w)
		op = OpTestRegRm
		opn = Operands{reg, rm}
	case (i1 & 0b11111110) == 0b00111100: // cmp ax, imm
		w := W(i1)
		imm := decodeImmediate(src, w)
		op = OpCmpAccImm
		opn = Operands{Register{name: RegA, width: w}, imm}
	case (i1 & 0b11111110) == 0b00110100: // xor ax, imm
		w := W(i1)
		imm := decodeImmediate(src, w)
		op = OpXorAccImm
		opn = Operands{Register{name: RegA, width: w}, imm}
	case (i1 & 0b11111110) == 0b00101100: // sub ax, imm
		w := W(i1)
		imm := decodeImmediate(src, w)
		op = OpSubAccImm
		opn = Operands{Register{name: RegA, width: w}, imm}
	case (i1 & 0b11111110) == 0b00100100: // and ax, imm
		w := W(i1)
		imm := decodeImmediate(src, w)
		op = OpAndAccImm
		opn = Operands{Register{name: RegA, width: w}, imm}
	case (i1 & 0b11111110) == 0b00010100: // adc ax, imm
		w := W(i1)
		imm := decodeImmediate(src, w)
		op = OpAdcAccImm
		opn = Operands{Register{name: RegA, width: w}, imm}
	case (i1 & 0b11111110) == 0b00001100: // or ax, imm
		w := W(i1)
		imm := decodeImmediate(src, w)
		op = OpOrAccImm
		opn = Operands{Register{name: RegA, width: w}, imm}
	case (i1 & 0b11111110) == 0b00000100: // add ax, imm
		w := W(i1)
		imm := decodeImmediate(src, w)
		op = OpAddAccImm
		opn = Operands{Register{name: RegA, width: w}, imm}
	case (i1 & 0b11111110) == 0b00011100: // sbb ax, imm
		w := W(i1)
		imm := decodeImmediate(src, w)
		op = OpSsbAccImm
		opn = Operands{Register{name: RegA, width: w}, imm}
	case (i1 & 0b11111101) == 0b10001100: // mov rm, seg // mov seg, rm
		d := D(i1)
		seg, rm := decodeModSegRm(src, 1)
		op = OpMovRmSeg
		opn = srcdst(d, seg, rm)
	case (i1 & 0b11111100) == 0b11010000: // {shl/sal,shr,sar,rol,ror,rcl,rcr} rm, 1 // {shl/sal,shr,sar,rol,ror,rcl,rcr} rm, reg
		v, w := VW(i1)
		_, rm := decodeModRegRm(src, w)
		op = map[byte]Operation{
			0b000: OpRol,
			0b001: OpRor,
			0b010: OpRcl,
			0b011: OpRcr,
			0b100: OpShlSal,
			0b101: OpShr,
			0b111: OpSar,
		}[REG(src.B(1))]
		if v == 0 { // count is 1
			opn = Operands{rm, SignedImmediate{width: w, value: 1}}
		} else { // count is in CL
			opn = Operands{rm, Register{name: RegC, width: 0}}
		}
	case (i1 & 0b11111100) == 0b10001000: // mov reg, rm // mov rm, reg
		d, w := DW(i1)
		reg, rm := decodeModRegRm(src, w)
		op = OpMovRegRm
		opn = srcdst(d, rm, reg)
	case (i1 & 0b11111100) == 0b10000000: // {add,adc,sub,sbb,cmp,and,or,xor} rm, imm
		s, w := SW(i1)
		_, rm := decodeModRegRm(src, w)
		op = map[byte]Operation{
			0b000: OpAddRmImm,
			0b001: OpOrRmImm,
			0b010: OpAdcRmImm,
			0b011: OpSsbRmImm,
			0b100: OpAndRmImm,
			0b101: OpSubRmImm,
			0b110: OpXorRegRm,
			0b111: OpCmpRmImm,
		}[REG(src.B(1))]
		_, sMustBeZero := map[Operation]struct{}{
			OpOrRmImm: {},
			OpAndRmImm: {},
			OpXorRegRm: {},
		}[op]
		if sMustBeZero && s != 0 {
			err = ErrDecode
		}
		d1 := src.Next()
		data := int16(d1)
		if s == 1 {
			data = int16(int8(d1))
		} else if s == 0 && w == 1 {
			d2 := src.Next()
			data = (int16(d2) << 8) ^ data
		}
		var imm Operand
		if s == 1 && w == 1 {
			imm = SignedImmediate{width: w, value: data}
		} else {
			imm = Immediate{width: w, value: uint16(data)}
		}
		opn = Operands{rm, imm}
	case (i1 & 0b11111100) == 0b00111000: // cmp reg, rm // cmp rm, reg
		d, w := DW(i1)
		reg, rm := decodeModRegRm(src, w)
		op = OpCmpRegRm
		opn = srcdst(d, rm, reg)
	case (i1 & 0b11111100) == 0b00110000: // xor reg, rm // xor rm, reg
		d, w := DW(i1)
		reg, rm := decodeModRegRm(src, w)
		op = OpXorRegRm
		opn = srcdst(d, rm, reg)
	case (i1 & 0b11111100) == 0b00101000: // sub reg, rm // sub rm, reg
		d, w := DW(i1)
		reg, rm := decodeModRegRm(src, w)
		op = OpSubRegRm
		opn = srcdst(d, rm, reg)
	case (i1 & 0b11111100) == 0b00100000: // and reg, rm // and rm, reg
		d, w := DW(i1)
		reg, rm := decodeModRegRm(src, w)
		op = OpAndRegRm
		opn = srcdst(d, rm, reg)
	case (i1 & 0b11111100) == 0b00011000: // sbb reg, rm // sbb rm, reg
		d, w := DW(i1)
		reg, rm := decodeModRegRm(src, w)
		op = OpSsbRegRm
		opn = srcdst(d, rm, reg)
	case (i1 & 0b11111100) == 0b00010000: // adc reg, rm // adc rm, reg
		d, w := DW(i1)
		reg, rm := decodeModRegRm(src, w)
		op = OpAdcRegRm
		opn = srcdst(d, rm, reg)
	case (i1 & 0b11111100) == 0b00001000: // or reg, rm // or rm, reg
		d, w := DW(i1)
		reg, rm := decodeModRegRm(src, w)
		op = OpOrRegRm
		opn = srcdst(d, rm, reg)
	case (i1 & 0b11111100) == 0: // add reg, rm // add rm, reg
		d, w := DW(i1)
		reg, rm := decodeModRegRm(src, w)
		op = OpAddRegRm
		opn = srcdst(d, rm, reg)
	case (i1 & 0b11111000) == 0b11011000: // esc rm // @fixme: device? nop?
		_, rm := decodeModRegRm(src, 1)
		op = OpEsc
		opn = Operands{rm}
	case (i1 & 0b11111000) == 0b10010000: // xchg reg, ax
		reg := REG1(i1)
		op = OpXchgAccReg
		opn = Operands{Register{name: reg, width: 1}, Register{name: RegA, width: 1}}
	case (i1 & 0b11111000) == 0b01011000: // pop reg
		reg := REG1(i1)
		op = OpPopReg
		opn = Operands{Register{name: reg, width: 1}}
	case (i1 & 0b11111000) == 0b01010000: // push reg
		reg := REG1(i1)
		op = OpPushReg
		opn = Operands{Register{name: reg, width: 1}}
	case (i1 & 0b11111000) == 0b01001000: // dec reg
		reg := REG1(i1)
		op = OpDecReg
		opn = Operands{Register{name: reg, width: 1}}
	case (i1 & 0b11111000) == 0b01000000: // inc reg
		reg := REG1(i1)
		op = OpIncReg
		opn = Operands{Register{name: reg, width: 1}}
	case (i1 & 0b11110000) == 0b10110000: // mov reg, imm
		w, reg := WREG(i1)
		imm := decodeImmediate(src, w)
		op = OpMovRegImm
		opn = Operands{Register{name: reg, width: w}, imm}
	case (i1 & 0b11100111) == 0b00000111: // pop seg
		seg := SEG(i1)
		op = OpPopSeg
		opn = Operands{Segment{name: seg}}
	case (i1 & 0b11100111) == 0b00000110: // push seg
		seg := SEG(i1)
		op = OpPushSeg
		opn = Operands{Segment{name: seg}}
	}
	bs, offset, size := src.Consume()
	bytes := [6]byte{}
	copy(bytes[:], bs)
	return Instruction {
		offset: offset,
		size: size,
		bytes: bytes,
		operation: op,
		operands: opn,
	}, err
}

func disassemble(text []byte) (insts []Instruction, disasErr error) {
	src := &Source{Text: text}
	for !src.IsEnd() {
		inst, err := decode(src)
		disasErr = errors.Join(disasErr, err)
		insts = append(insts, inst)
	}
	return insts, disasErr
}

func must[T any](t T, err error) T {
	if err != nil {
		panic(err)
	}
	return t
}

var d = flag.Bool("d", false, "disassemble")

func init() {
	flag.Parse()
}

func main() {
	restArgs := flag.Args()
	if len(restArgs) <= 0 {
		log.Fatal("no input file provided")
	}
	file := restArgs[0]
	bin := must(os.ReadFile(file))
	//exec := *(*Exec)(unsafe.Pointer(&bin[0]))
	var exec Exec
	if err := binary.Read(bytes.NewReader(bin), binary.LittleEndian, &exec); err != nil {
		log.Fatal(err)
	}
	//fmt.Printf("%#v\n", exec)
	text := bin[32:32+exec.SizeText]
	//fmt.Printf("%x\n", text)

	insts, err := disassemble(text)
	if err != nil {
		log.Fatal(err)
	}
	for _, inst := range insts {
		fmt.Println(inst)
	}
}
