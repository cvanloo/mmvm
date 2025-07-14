package main

import (
	"bytes"
	"encoding/binary"
	"errors"
	"flag"
	"fmt"
	"log"
	"math"
	"os"
	"runtime"
	"slices"
	"strings"
	"unsafe"
)

// @todo: https://www.muppetlabs.com/~breadbox/txt/mopb.html
// @todo: AT&T syntax printing

type (
	// a.out header (format of executable files)
	// defined in Minix2:SYS/include/a.out.h
	// and read in Minix2:SYS/src/mm/exec.c
	// and also relevant Minix2:SYS/src/lib/posix/_execve.c
	Exec struct {
		MidMag struct {
			Magic   [2]byte // needs to be 0x01 0x03
			Flags   byte    // 0x20 -> separate I/D (instruction/text and data)
			CPU     byte
			HdrLen  uint8
			Unused  byte
			Version uint16
		}
		SizeText   int32
		SizeData   int32
		SizeBSS    int32
		EntryPoint int32
		MemTotal   int32
		SizeSym    int32
		/* end of short form */
	}
	Operation int
	Operand   interface {
		String() string
		W() byte
	}
	Operands    []Operand
	Instruction struct {
		offset, size int
		bytes        [6]byte
		operation    Operation
		operands     Operands
	}
	Repeated struct {
		operation Operation
		operands  Operands
	}
	Register struct {
		name, width byte
	}
	Segment struct {
		name byte
	}
	// Relative
	Memory struct {
		width, mod, rm, dispHigh, dispLow byte
	}
	// Absolute
	Address struct { // @todo: do we really need this?
		width byte
		addr  int16
	}
	// Segment:Offset
	SegmentOffset struct { // @todo: where do we need this?
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

func (e Exec) BadMag() bool {
	return e.MidMag.Magic[0] != 0x01 || e.MidMag.Magic[1] != 0x03
}

func (e Exec) Text(bin []byte) []byte {
	s := int32(e.MidMag.HdrLen)
	text := bin[s : s+e.SizeText]
	return text
}

func (e Exec) Data(bin []byte) []byte {
	s := int32(e.MidMag.HdrLen)
	s += e.SizeText
	data := bin[s : s+e.SizeData]
	return data
}

const (
	RegA byte = iota
	RegC
	RegD
	RegB
	RegSP
	RegBP
	RegSI
	RegDI
	RegFLAGS
	RegIP
	RegAH   = RegSP
	RegCH   = RegBP
	RegDH   = RegSI
	RegBH   = RegDI
	RegLast = RegIP
)

const (
	SegES byte = iota
	SegCS
	SegSS
	SegDS
)

//go:generate stringer -type=Operation -trimprefix=Op -linecomment
const (
	OpInvalid           Operation = iota // (undefined)
	OpMovRegRm                           // mov
	OpMovRmImm                           // mov
	OpMovRegImm                          // mov
	OpMovAccMem                          // mov
	OpMovMemAcc                          // mov
	OpMovRmSeg                           // mov
	OpPushRm                             // push
	OpPushReg                            // push
	OpPushSeg                            // push
	OpPopRm                              // pop
	OpPopReg                             // pop
	OpPopSeg                             // pop
	OpXchgRmReg                          // xchg
	OpXchgAccReg                         // xchg
	OpInFixedPort                        // in
	OpInVarPort                          // in
	OpOutFixedPort                       // out
	OpOutVarPort                         // out
	OpXLAT                               // xlat
	OpLEA                                // lea
	OpLDS                                // lds
	OpLES                                // les
	OpLAHF                               // lahf
	OpSAHF                               // sahf
	OpPUSHF                              // pushf
	OpPOPF                               // popf
	OpAddRegRm                           // add
	OpAddRmImm                           // add
	OpAddAccImm                          // add
	OpAdcRegRm                           // adc
	OpAdcRmImm                           // adc
	OpAdcAccImm                          // adc
	OpIncRm                              // inc
	OpIncReg                             // inc
	OpAAA                                // aaa
	OpBAA                                // baa
	OpSubRegRm                           // sub
	OpSubRmImm                           // sub
	OpSubAccImm                          // sub
	OpSsbRegRm                           // sbb
	OpSsbRmImm                           // sbb
	OpSsbAccImm                          // sbb
	OpDecRm                              // dec
	OpDecReg                             // dec
	OpNeg                                // neg
	OpCmpRegRm                           // cmp
	OpCmpRmImm                           // cmp
	OpCmpAccImm                          // cmp
	OpAAS                                // aas
	OpDAS                                // das
	OpMul                                // mul
	OpImul                               // imul
	OpAAM                                // aam
	OpDiv                                // div
	OpIdiv                               // idiv
	OpAAD                                // aad
	OpCBW                                // cbw
	OpCWD                                // cwd
	OpNot                                // not
	OpShlSal                             // shl
	OpShr                                // shr
	OpSar                                // sar
	OpRol                                // rol
	OpRor                                // ror
	OpRcl                                // rcl
	OpRcr                                // rcr
	OpAndRegRm                           // and
	OpAndRmImm                           // and
	OpAndAccImm                          // and
	OpTestRegRm                          // test
	OpTestRmImm                          // test
	OpTestAccImm                         // test
	OpOrRegRm                            // or
	OpOrRmImm                            // or
	OpOrAccImm                           // or
	OpXorRegRm                           // xor
	OpXorRmImm                           // xor
	OpXorAccImm                          // xor
	OpRep                                // rep
	OpMovsb                              // movsb
	OpCmpsb                              // cmpsb
	OpScasb                              // scasb
	OpLodsb                              // lodsb
	OpStosb                              // stosb
	OpMovsw                              // movsw
	OpCmpsw                              // cmpsw
	OpScasw                              // scasw
	OpLodsw                              // lodsw
	OpStosw                              // stosw
	OpCallDirSeg                         // call
	OpCallIndirSeg                       // call
	OpCallDirInterSeg                    // call
	OpCallIndirInterSeg                  // call
	OpJmpDirSeg                          // jmp
	OpJmpShortDirSeg                     // jmp short
	OpJmpIndirSeg                        // jmp
	OpJmpDirInterSeg                     // jmp
	OpJmpIndirInterSeg                   // jmp
	OpRetSeg                             // ret
	OpRetSegImm                          // ret
	OpRetInterSeg                        // ret
	OpRetInterSegImm                     // ret
	OpJe                                 // je
	OpJl                                 // jl
	OpJle                                // jle
	OpJb                                 // jb
	OpJbe                                // jbe
	OpJp                                 // jp
	OpJo                                 // jo
	OpJs                                 // js
	OpJne                                // jne
	OpJnl                                // jnl
	OpJnle                               // jnle
	OpJnb                                // jnb
	OpJnbe                               // jnbe
	OpJnp                                // jnp
	OpJno                                // jno
	OpJns                                // jns
	OpLoop                               // loop
	OpLoopz                              // loopz
	OpLoopnz                             // loopnz
	OpJcxz                               // jcxz
	OpIntType3                           // int
	OpIntTypeSpecified                   // int
	OpInto                               // into
	OpIret                               // iret
	OpClc                                // clc
	OpCmc                                // cmc
	OpStc                                // stc
	OpCld                                // cld
	OpStd                                // std
	OpCli                                // cli
	OpSti                                // sti
	OpHlt                                // hlt
	OpWait                               // wait
	OpEsc                                // esc
	OpLock                               // lock
)

func (ops Operands) String() string {
	var builder strings.Builder
	for i, op := range ops {
		if i > 0 {
			builder.WriteString(", ")
		}
		builder.WriteString(op.String())
	}
	return builder.String()
}

func (reg Register) IsGeneralPurpose() bool {
	return reg.name >= RegA && reg.name <= RegB
}

func (reg Register) IsSpecialPurpose() bool {
	return reg.name >= RegSP && reg.name <= RegIP
}

func (reg Register) String() string {
	names := []string{"al", "cl", "dl", "bl", "ah", "ch", "dh", "bh", "ax", "cx", "dx", "bx", "sp", "bp", "si", "di"}
	return names[reg.name+reg.width*8]
}

func (reg Register) W() byte {
	return reg.width
}

func (seg Segment) String() string {
	names := []string{"es", "cs", "ss", "ds"}
	return names[seg.name]
}

func (seg Segment) W() byte {
	return 1
}

func (mem Memory) String() string {
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

func (mem Memory) W() byte {
	return mem.width
}

func (addr Address) String() string {
	return fmt.Sprintf("[%04x]", addr.addr)
}

func (addr Address) W() byte {
	return addr.width
}

func (so SegmentOffset) String() string {
	return fmt.Sprintf("%04x:%04x", so.segment, so.offset)
}

func (so SegmentOffset) W() byte {
	panic("not implemented")
}

func (imm Immediate) String() string {
	return fmt.Sprintf("%0[1]*x", (imm.width)*4, imm.value)
}

func (imm Immediate) W() byte {
	return imm.width
}

func (imm SignedImmediate) String() string {
	return fmt.Sprintf("%x", imm.value)
}

func (imm SignedImmediate) W() byte {
	return imm.width
}

func (r Repeated) String() string {
	if len(r.operands) > 0 {
		return fmt.Sprintf("%s %s", r.operation, r.operands)
	} else {
		return fmt.Sprintf("%s", r.operation)
	}
}

func (r Repeated) W() byte {
	panic("nonsense calling W() on Repeated")
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
		return fmt.Sprintf("%-13x %s%s %s", inst.bytes[:inst.size], inst.operation, sizeSpecifier(), inst.operands)
	} else {
		return fmt.Sprintf("%-13x %s", inst.bytes[:inst.size], inst.operation)
	}
}

type (
	InstructionFormatterWithOffset struct {
		Instruction
	}
	InstructionFormatterWithMemoryAccess struct {
		inst Instruction
		cpu  *CPU
	}
)

func (iswo InstructionFormatterWithOffset) String() string {
	return fmt.Sprintf("%04x: %s", iswo.Instruction.offset, iswo.Instruction)
}

func (iswma InstructionFormatterWithMemoryAccess) String() string {
	printInst := func(inst Instruction) string {
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
			return fmt.Sprintf("%-13x%s%s %s", inst.bytes[:inst.size], inst.operation, sizeSpecifier(), inst.operands)
		} else {
			return fmt.Sprintf("%-13x%s", inst.bytes[:inst.size], inst.operation)
		}
	}
	if len(iswma.inst.operands) <= 2 {
		for _, opnd := range iswma.inst.operands {
			switch m := opnd.(type) {
			case Memory:
				switch m.width {
				default:
					panic(fmt.Errorf("invalid width: ", m.width))
				case 0:
					return fmt.Sprintf("%s ;[%04x]%02x", printInst(iswma.inst), m.Addr(iswma.cpu), uint8(iswma.cpu.Get8(m)))
				case 1:
					return fmt.Sprintf("%s ;[%04x]%04x", printInst(iswma.inst), m.Addr(iswma.cpu), uint16(iswma.cpu.Get16(m)))
				}
			//@todo: case Address:
			}
		}
	}
	return printInst(iswma.inst)
}

func bitsExtract(s, m byte) func(byte) byte {
	return func(i byte) byte {
		return (i >> s) & m
	}
}

var (
	W    = bitsExtract(0, 1)
	REG1 = bitsExtract(0, 0b111)
	D    = bitsExtract(1, 1)
	S    = D
	V    = D
	MOD  = bitsExtract(6, 0b11)
	RM   = bitsExtract(0, 0b111)
	REG  = bitsExtract(3, 0b111)
	SEG  = bitsExtract(3, 0b11)
	WREG = func(i byte) (w, reg byte) {
		return (i >> 3) & 1, i & 0b111
	}
	DW = func(i byte) (d, w byte) {
		return D(i), W(i)
	}
	SW       = DW
	VW       = DW
	MODREGRM = func(i byte) (mod, reg, rm byte) {
		return MOD(i), REG(i), RM(i)
	}
)

type Source struct {
	Text          []byte
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
			orm = Memory{width: width, mod: mod, rm: rm, dispLow: src.Next(), dispHigh: src.Next()}
		} else {
			orm = Memory{width: width, mod: mod, rm: rm}
		}
	case 0b01:
		orm = Memory{width: width, mod: mod, rm: rm, dispHigh: 0, dispLow: src.Next()}
	case 0b10:
		// @fixme: will this code execute in the right order?
		orm = Memory{width: width, mod: mod, rm: rm, dispLow: src.Next(), dispHigh: src.Next()}
	case 0b11:
		orm = Register{name: rm, width: width}
	}
	return oreg, orm
}

func decodeModSegRm(src *Source, width byte) (oreg, orm Operand) {
	mod, reg, rm := MODREGRM(src.Next())
	if reg&0b100 != 0 {
		// @todo: error handling
		reg = reg & 0b011
	}
	oreg = Segment{name: reg}
	switch mod {
	case 0b00:
		if rm == 0b110 {
			orm = Memory{width: width, mod: mod, rm: rm, dispLow: src.Next(), dispHigh: src.Next()}
		} else {
			orm = Memory{width: width, mod: mod, rm: rm}
		}
	case 0b01:
		orm = Memory{width: width, mod: mod, rm: rm, dispHigh: 0, dispLow: src.Next()}
	case 0b10:
		// @fixme: will this code execute in the right order?
		orm = Memory{width: width, mod: mod, rm: rm, dispLow: src.Next(), dispHigh: src.Next()}
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
		op  Operation
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
	return Repeated{
		operation: op,
		operands:  opn,
	}, err
}

func decode(src *Source) (inst Instruction, err error) {
	defer func() {
		r := recover()
		if r != nil {
			bs, offset, size := src.Consume()
			bytes := [6]byte{}
			copy(bytes[:], bs)
			inst = Instruction{
				offset:    offset,
				size:      size,
				bytes:     bytes,
				operation: OpInvalid,
				operands:  nil,
			}
			err = errors.Join(err, fmt.Errorf("panicked: %v", r))
		}
	}()
	var (
		op  Operation
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
		// @todo: we could merge this together with OpIntTypeSpecified
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
			opn = append(opn, decodeImmediate(src, w))
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
			OpOrRmImm:  {},
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
	return Instruction{
		offset:    offset,
		size:      size,
		bytes:     bytes,
		operation: op,
		operands:  opn,
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

type (
	CPU struct {
		RegisterFile [10]uint16
		Text         RAM
		Data         RAM
		ProgramBreak uint16
	}
	Flags struct {
		reg *uint16
	}
	RAM [1 << 32]byte
)

func (r *RAM) WriteBytes(addr uint16, val []byte) {
	copy(r[addr:addr+uint16(len(val))], val)
}

func (r *RAM) WriteString(addr uint16, val string) {
	copy(r[addr:addr+uint16(len(val))], val[:])
	r[addr+uint16(len(val))] = 0
}

func (r *RAM) WriteZeros(addr uint16, count uint16) {
	for i := range count {
		r[addr+i] = 0
	}
}

func (r *RAM) Write16(addr uint16, val uint16) {
	r[addr+1] = byte(val >> 8)
	r[addr+0] = byte(val >> 0)
}

func (r *RAM) Read8(addr uint16) uint8 {
	return r[addr]
}

func (r *RAM) Read16(addr uint16) uint16 {
	return (uint16(r[addr+1]) << 8) | uint16(r[addr])
}

func flagExtract(offset byte) func(uint16) uint16 {
	return func(flags uint16) uint16 {
		return (flags >> offset) & 1
	}
}

var (
	CF = flagExtract(0)
	ZF = flagExtract(6)
	SF = flagExtract(7)
	OF = flagExtract(11)
)

func (cpu *CPU) Flags() Flags {
	return Flags{&cpu.RegisterFile[RegFLAGS]}
}

func (f Flags) Set(i, s uint16) Flags {
	*f.reg = (*f.reg & ^(uint16(1) << i)) | (s << i)
	return f
}

func (f Flags) SetB(i uint16, b bool) Flags {
	if b {
		return f.Set(i, 1)
	} else {
		return f.Set(i, 0)
	}
}

func (f Flags) SetZSCO(z, s, c, o bool) {
	f.SetB(6, z).SetB(7, s).SetB(0, c).SetB(11, o)
}

func (cpu *CPU) Get8(opnd Operand) int32 {
	switch o := opnd.(type) {
	default:
		panic(fmt.Errorf("invalid operand: %T", opnd))
	case Register:
		if o.width == 0 && o.name >= RegAH && o.name <= RegBH {
			return int32(int8(cpu.RegisterFile[o.name-RegAH] >> 8))
		}
		return int32(int8(cpu.RegisterFile[o.name]))
	case Memory:
		addr := o.Addr(cpu)
		return int32(int8(cpu.Data[addr]))
	case Immediate:
		return int32(int8(o.value))
	case SignedImmediate:
		return int32(int8(o.value))
	}
}

func (cpu *CPU) Get16(opnd Operand) int32 {
	switch o := opnd.(type) {
	default:
		panic(fmt.Errorf("invalid operand: %T", opnd))
	case Register:
		if o.width == 0 && o.name >= RegAH && o.name <= RegBH {
			return int32(int8(cpu.RegisterFile[o.name-RegAH] >> 8))
		}
		return int32(int16(cpu.RegisterFile[o.name]))
	case Memory:
		addr := o.Addr(cpu)
		return int32((int16(cpu.Data[addr+1]) << 8) | int16(cpu.Data[addr]))
	case Immediate:
		return int32(int16(o.value))
	case SignedImmediate:
		return int32(int16(o.value))
	}
}

func (cpu *CPU) Set8(opnd Operand, val int32) {
	switch o := opnd.(type) {
	default:
		panic(fmt.Errorf("invalid operand: %T", opnd))
	case Register:
		if o.width == 0 && o.name >= RegAH && o.name <= RegBH {
			// write to upper half does not zero the lower half
			cpu.RegisterFile[o.name-RegAH] = uint16(val<<8) | (cpu.RegisterFile[o.name-RegAH] & 0x00FF)
		} else {
			// write to lower half does not zero the upper half
			cpu.RegisterFile[o.name] = (cpu.RegisterFile[o.name] & 0xFF00) | uint16(uint8(val))
		}
	case Memory:
		addr := o.Addr(cpu)
		cpu.Data[addr] = byte(val)
	}
}

func (cpu *CPU) Set16(opnd Operand, val int32) {
	switch o := opnd.(type) {
	default:
		panic(fmt.Errorf("invalid operand: %T", opnd))
	case Register:
		if o.width == 0 && o.name >= RegAH && o.name <= RegBH {
			// @fixme: should it even be valid to call Set16 on a 8-bit reg?
			cpu.RegisterFile[o.name-RegAH] = uint16(val<<8) | (cpu.RegisterFile[o.name-RegAH] & 0x00FF)
		} else {
			cpu.RegisterFile[o.name] = uint16(val)
		}
	case Memory:
		addr := o.Addr(cpu)
		cpu.Data[addr+1] = byte(val >> 8)
		cpu.Data[addr+0] = byte(val >> 0)
	}
}

func BaseOffset(cpu *CPU, i byte, disp int16) uint16 {
	const RegInv = RegLast + 1
	type BaseOffset struct {
		Base, Offset byte
	}
	names := []BaseOffset{
		{RegB, RegSI},
		{RegB, RegDI},
		{RegBP, RegSI},
		{RegBP, RegDI},
		{RegSI, RegInv},
		{RegDI, RegInv},
		{RegBP, RegInv},
		{RegB, RegInv},
	}
	bo := names[i]
	base := int16(cpu.RegisterFile[bo.Base])
	offset := int16(0)
	if bo.Offset <= RegLast {
		offset = int16(cpu.RegisterFile[bo.Offset])
	}
	return uint16(base + offset + disp)
}

func (mem Memory) Addr(cpu *CPU) uint16 {
	switch mem.mod {
	default:
		panic("unreachable")
	case 0b00:
		if mem.rm == 0b110 {
			return (uint16(mem.dispHigh) << 8) ^ uint16(mem.dispLow)
		}
		return BaseOffset(cpu, mem.rm, 0)
	case 0b01:
		disp := int16(int8(mem.dispLow))
		return BaseOffset(cpu, mem.rm, disp)
	case 0b10:
		disp := (int16(mem.dispHigh) << 8) ^ int16(mem.dispLow)
		return BaseOffset(cpu, mem.rm, disp)
	}
}

func (cpu *CPU) String() string {
	flags := func(flags uint16) string {
		s := []byte("----")
		CF := (flags >> 0) & 1
		ZF := (flags >> 6) & 1
		SF := (flags >> 7) & 1
		OF := (flags >> 11) & 1
		if CF == 1 {
			s[3] = 'C'
		}
		if ZF == 1 {
			s[2] = 'Z'
		}
		if SF == 1 {
			s[1] = 'S'
		}
		if OF == 1 {
			s[0] = 'O'
		}
		return string(s)
	}
	return fmt.Sprintf(
		"%04x %04x %04x %04x %04x %04x %04x %04x %s %04x",
		cpu.RegisterFile[RegA], cpu.RegisterFile[RegB], cpu.RegisterFile[RegC],
		cpu.RegisterFile[RegD], cpu.RegisterFile[RegSP], cpu.RegisterFile[RegBP],
		cpu.RegisterFile[RegSI], cpu.RegisterFile[RegDI],
		flags(cpu.RegisterFile[RegFLAGS]), cpu.RegisterFile[RegIP],
	)
}

func (cpu *CPU) Fetch() *Source {
	return &Source{
		Text:     cpu.Text[:],
		Consumed: int(cpu.RegisterFile[RegIP]),
		Pos:      int(cpu.RegisterFile[RegIP]),
	}
}

func (cpu *CPU) Decode(src *Source) (Instruction, error) {
	return decode(src)
}

var (
	ErrIllegalInstruction   = errors.New("illegal instruction")
	ErrOperandWidthMismatch = errors.New("operands differ in width")
	ErrRegisterSize         = errors.New("register of wrong size")
	ErrRegisterGP           = errors.New("register not for general purpose")
)

func (cpu *CPU) Step(inst Instruction) {
	// @todo: processor exceptions?
	isImm := func(opnd Operand) bool {
		switch opnd.(type) {
		default:
			return false
		case Immediate:
			return true
			// @todo: or SignedImmediate
		}
	}
	intVector := map[int32]func(*CPU){
		0x20: func(cpu *CPU) { // syscall
			sysVector := [78]func(*CPU) uint16{
				0: func(*CPU) uint16 { // no_sys
					return uint16(EINVAL)
				},
				1: func(cpu *CPU) uint16 { // exit
					type Msg struct {
						source, type_, code uint16
					}
					addr := cpu.RegisterFile[RegB]
					bs := cpu.Data[addr : addr+uint16(unsafe.Sizeof(Msg{}))]
					msg := (*Msg)(unsafe.Pointer(&bs[0]))
					if *m {
						fmt.Printf("<exit(%d)>\n", msg.code)
					}
					os.Exit(int(msg.code))
					return msg.code
				},
				4: func(cpu *CPU) uint16 { // write
					type Msg struct {
						source, type_    uint16
						fd, len, _, addr uint16
					}
					addr := cpu.RegisterFile[RegB]
					bs := cpu.Data[addr : addr+uint16(unsafe.Sizeof(Msg{}))]
					msg := (*Msg)(unsafe.Pointer(&bs[0]))
					if *m {
						fmt.Printf("<write(%d, 0x%04x, %d)", msg.fd, msg.addr, msg.len)
					}
					str := string(cpu.Data[msg.addr : msg.addr+msg.len])
					fmt.Print(str)
					ret := len(str)
					if *m {
						fmt.Printf(" => %d>\n", ret)
					}
					cpu.RegisterFile[RegA] = 0 // ?
					return uint16(ret)
				},
				17: func(cpu *CPU) uint16 { // brk
					type Msg struct {
						source, type_ uint16
						_             [6]byte
						address       uint16
						_             [6]byte
						reply         uint16
					}
					addr := cpu.RegisterFile[RegB]
					bs := cpu.Data[addr : addr+uint16(unsafe.Sizeof(Msg{}))]
					msg := (*Msg)(unsafe.Pointer(&bs[0]))
					if *m {
						fmt.Printf("<brk(0x%04x) => ", msg.address)
					}
					ret := uint16(0)
					if true { // @todo: checks???
						cpu.ProgramBreak = msg.address
						msg.reply = cpu.ProgramBreak
						if *m {
							fmt.Printf("0>\n")
						}
					} else {
						ret = uint16(-ENOMEM)
						msg.reply = 0xFFFF // -1
						if *m {
							fmt.Printf("ENOMEM>\n")
						}
					}
					cpu.RegisterFile[RegA] = 0 // ?
					return ret
				},
				54: func(cpu *CPU) uint16 { // ioctl
					type Msg struct {
						source  uint16
						type_   uint16
						fd      uint16
						_       [2]byte
						request uint16
						_       [8]byte
						address uint16
					}
					addr := cpu.RegisterFile[RegB]
					bs := cpu.Data[addr : addr+uint16(unsafe.Sizeof(Msg{}))]
					msg := (*Msg)(unsafe.Pointer(&bs[0]))
					if *m {
						fmt.Printf("<ioctl(%d, 0x%04x, 0x%04x)>\n", msg.fd, msg.request, msg.address)
					}
					cpu.RegisterFile[RegA] = 0 // ?
					return uint16(-EINVAL)
				},
			}
			type Msg struct {
				source, type_ uint16
			}
			addr := cpu.RegisterFile[RegB]
			bs := cpu.Data[addr : addr+uint16(unsafe.Sizeof(Msg{}))]
			msg := (*Msg)(unsafe.Pointer(&bs[0]))
			ret := sysVector[msg.type_](cpu)
			msg.type_ = ret
		},
	}
	switch inst.operation {
	default:
		fallthrough
	case OpInvalid:
		assert(false, ErrIllegalInstruction)
	case OpMovRegRm, OpMovRmImm, OpMovRegImm, OpMovAccMem, OpMovMemAcc, OpMovRmSeg:
		dst := inst.operands[0]
		src := inst.operands[1]
		switch {
		default:
			panic(ErrOperandWidthMismatch)
		case dst.W() == 0 && src.W() == 0:
			cpu.Set8(dst, cpu.Get8(src))
		case dst.W() == 1 && src.W() == 1:
			cpu.Set16(dst, cpu.Get16(src))
		}
		// FLAGS none affected
	case OpPushRm, OpPushReg, OpPushSeg:
		arg := inst.operands[0]
		cpu.RegisterFile[RegSP] -= 2
		switch arg.W() {
		case 0:
			cpu.Data.Write16(cpu.RegisterFile[RegSP], uint16(cpu.Get8(arg)))
		case 1:
			cpu.Data.Write16(cpu.RegisterFile[RegSP], uint16(cpu.Get16(arg)))
		}
		// FLAGS none affected
	case OpPopRm, OpPopReg, OpPopSeg:
		dst := inst.operands[0]
		switch dst.W() {
		case 0:
			val := int32(cpu.Data.Read8(cpu.RegisterFile[RegSP]))
			cpu.Set8(dst, val)
			cpu.RegisterFile[RegSP] += 1
		case 1:
			val := int32(cpu.Data.Read16(cpu.RegisterFile[RegSP]))
			cpu.Set16(dst, val)
			cpu.RegisterFile[RegSP] += 2
		}
		// FLAGS none affected
	case OpXchgRmReg, OpXchgAccReg:
		a := inst.operands[0]
		b := inst.operands[1]
		switch {
		case a.W() == 0 && b.W() == 0:
			tmp := cpu.Get8(a)
			cpu.Set8(a, cpu.Get8(b))
			cpu.Set8(b, tmp)
		case a.W() == 1 && b.W() == 1:
			tmp := cpu.Get16(a)
			cpu.Set16(a, cpu.Get16(b))
			cpu.Set16(b, tmp)
		}
		// FLAGS none affected
	//case OpInFixedPort:
	//case OpInVarPort:
	//case OpOutFixedPort:
	//case OpOutVarPort:
	//case OpXLAT:
	case OpLEA:
		reg := inst.operands[0].(Register)
		mem := inst.operands[1].(Memory)
		assert(reg.IsGeneralPurpose(), ErrRegisterGP)
		assert(reg.width == 1, ErrRegisterSize)
		cpu.Set16(reg, int32(mem.Addr(cpu)))
		// FLAGS none affected
	//case OpLDS:
	//case OpLES:
	//case OpLAHF:
	//case OpSAHF:
	//case OpPUSHF:
	//case OpPOPF:
	case OpAddRegRm, OpAddRmImm, OpAddAccImm:
		dst := inst.operands[0]
		src := inst.operands[1]
		switch {
		default:
			panic(ErrOperandWidthMismatch)
		case dst.W() == 0 && src.W() == 0:
			r := cpu.Get8(dst) + cpu.Get8(src)
			cpu.Set8(dst, r)
			cpu.Flags().SetZSCO(r == 0, r < 0, r > math.MaxUint8, r > math.MaxInt8 || r < math.MinInt8)
		case dst.W() == 1 && src.W() == 1:
			r := cpu.Get16(dst) + cpu.Get16(src)
			cpu.Set16(dst, r)
			cpu.Flags().SetZSCO(r == 0, r < 0, r > math.MaxUint16, r > math.MaxInt16 || r < math.MinInt16)
		case dst.W() == 1 && src.W() == 0 && isImm(src):
			r := cpu.Get16(dst) + cpu.Get8(src)
			cpu.Set8(dst, r)
			cpu.Flags().SetZSCO(r == 0, r < 0, r > math.MaxUint16, r > math.MaxInt16 || r < math.MinInt16)
		}
	case OpAdcRegRm, OpAdcRmImm, OpAdcAccImm:
		dst := inst.operands[0]
		src := inst.operands[1]
		c := int32(CF(cpu.RegisterFile[RegFLAGS]))
		switch {
		default:
			panic(ErrOperandWidthMismatch)
		case dst.W() == 0 && src.W() == 0:
			r := cpu.Get8(dst) + cpu.Get8(src) + c
			cpu.Set8(dst, r)
			cpu.Flags().SetZSCO(r == 0, r < 0, r > math.MaxUint8, r > math.MaxInt8 || r < math.MinInt8)
		case dst.W() == 1 && src.W() == 1:
			r := cpu.Get16(dst) + cpu.Get16(src) + c
			cpu.Set16(dst, r)
			cpu.Flags().SetZSCO(r == 0, r < 0, r > math.MaxUint16, r > math.MaxInt16 || r < math.MinInt16)
		case dst.W() == 1 && src.W() == 0 && isImm(src):
			r := cpu.Get16(dst) + cpu.Get8(src) + c
			cpu.Set8(dst, r)
			cpu.Flags().SetZSCO(r == 0, r < 0, r > math.MaxUint16, r > math.MaxInt16 || r < math.MinInt16)
		}
	case OpIncRm, OpIncReg:
		dst := inst.operands[0]
		switch dst.W() {
		case 0:
			r := cpu.Get8(dst) + 1
			cpu.Set8(dst, r)
			cpu.Flags().SetZSCO(r == 0, r < 0, CF(cpu.RegisterFile[RegFLAGS]) == 1, r > math.MaxInt8 || r < math.MinInt8)
		case 1:
			r := cpu.Get16(dst) + 1
			cpu.Set16(dst, r)
			cpu.Flags().SetZSCO(r == 0, r < 0, CF(cpu.RegisterFile[RegFLAGS]) == 1, r > math.MaxInt16 || r < math.MinInt16)
		}
	//case OpAAA:
	//case OpBAA:
	case OpSubRegRm, OpSubRmImm, OpSubAccImm:
		s1 := inst.operands[0]
		s2 := inst.operands[1]
		switch {
		default:
			panic(ErrOperandWidthMismatch)
		case s1.W() == 0 && s2.W() == 0:
			a := cpu.Get8(s1)
			b := cpu.Get8(s2)
			r := a - b
			cpu.Set8(s1, r)
			cpu.Flags().SetZSCO(r == 0, r < 0, uint16(a) < uint16(b), r > math.MaxInt8 || r < math.MinInt8)
		case s1.W() == 1 && s2.W() == 1:
			a := cpu.Get16(s1)
			b := cpu.Get16(s2)
			r := a - b
			cpu.Set16(s1, r)
			cpu.Flags().SetZSCO(r == 0, r < 0, uint16(a) < uint16(b), r > math.MaxInt16 || r < math.MinInt16)
		case s1.W() == 1 && s2.W() == 0 && isImm(s2):
			a := cpu.Get16(s1)
			b := cpu.Get8(s2)
			r := a - b
			cpu.Set16(s1, r)
			cpu.Flags().SetZSCO(r == 0, r < 0, uint16(a) < uint16(b), r > math.MaxInt16 || r < math.MinInt16)
		}
	//case OpSsbRegRm, OpSsbRmImm, OpSsbAccImm:
	case OpDecRm, OpDecReg:
		dst := inst.operands[0]
		switch dst.W() {
		case 0:
			r := cpu.Get8(dst) - 1
			cpu.Set8(dst, r)
			cpu.Flags().SetZSCO(r == 0, r < 0, CF(cpu.RegisterFile[RegFLAGS]) == 1, r > math.MaxInt8 || r < math.MinInt8)
		case 1:
			r := cpu.Get16(dst) - 1
			cpu.Set16(dst, r)
			cpu.Flags().SetZSCO(r == 0, r < 0, CF(cpu.RegisterFile[RegFLAGS]) == 1, r > math.MaxInt16 || r < math.MinInt16)
		}
	case OpNeg:
		dst := inst.operands[0]
		switch dst.W() {
		case 0:
			r := -cpu.Get8(dst)
			cpu.Set8(dst, r)
			cpu.Flags().SetZSCO(r == 0, r < 0, r != 0, r > math.MaxInt16 || r < math.MinInt16)
		case 1:
			r := -cpu.Get16(dst)
			cpu.Set16(dst, r)
			cpu.Flags().SetZSCO(r == 0, r < 0, r != 0, r > math.MaxInt16 || r < math.MinInt16)
		}
	case OpCmpRegRm, OpCmpRmImm, OpCmpAccImm:
		s1 := inst.operands[0]
		s2 := inst.operands[1]
		switch {
		default:
			panic(ErrOperandWidthMismatch)
		case s1.W() == 0 && s2.W() == 0:
			a := cpu.Get8(s1)
			b := cpu.Get8(s2)
			r := a - b
			cpu.Flags().SetZSCO(r == 0, r < 0, uint16(a) < uint16(b), r > math.MaxInt8 || r < math.MinInt8)
		case s1.W() == 1 && s2.W() == 1:
			a := cpu.Get16(s1)
			b := cpu.Get16(s2)
			r := a - b
			cpu.Flags().SetZSCO(r == 0, r < 0, uint16(a) < uint16(b), r > math.MaxInt16 || r < math.MinInt16)
		case s1.W() == 1 && s2.W() == 0 && isImm(s2):
			a := cpu.Get16(s1)
			b := cpu.Get8(s2)
			r := a - b
			cpu.Flags().SetZSCO(r == 0, r < 0, uint16(a) < uint16(b), r > math.MaxInt16 || r < math.MinInt16)
		}
	//case OpAAS:
	//case OpDAS:
	//case OpMul:
	//case OpImul:
	//case OpAAM:
	case OpDiv:
		arg := inst.operands[0]
		switch arg.W() {
		case 0:
			dividend := cpu.RegisterFile[RegA]
			divisor := uint16(cpu.Get8(arg))
			quotient := dividend / divisor
			remainder := dividend - quotient*divisor
			cpu.RegisterFile[RegA] = ((remainder << 8) & 0xFF00) | (quotient & 0x00FF)
		case 1:
			dividend := (uint32(cpu.RegisterFile[RegD]) << 8) | uint32(cpu.RegisterFile[RegA])
			divisor := uint32(uint16(cpu.Get16(arg)))
			quotient := dividend / divisor
			remainder := dividend - quotient*divisor
			cpu.RegisterFile[RegA] = uint16(quotient)
			cpu.RegisterFile[RegD] = uint16(remainder)
		}
		// FLAGS none affected (undefined)
	//case OpIdiv:
	//case OpAAD:
	case OpCBW:
		al := int8(cpu.RegisterFile[RegA])
		cpu.RegisterFile[RegA] = uint16(int16(al))
		// FLAGS none affected
	case OpCWD:
		a := cpu.RegisterFile[RegA]
		d := int32(int16(a)) // sign-extend
		cpu.RegisterFile[RegD] = uint16(d >> 16)
		// FLAGS none affected
	//case OpNot:
	case OpShlSal:
		dst := inst.operands[0]
		c := inst.operands[1]
		switch dst.W() {
		case 0:
			a := cpu.Get8(dst)
			b := cpu.Get8(c)
			r := a << b
			cpu.Set8(dst, r)
			carry := ((a << (b - 1)) >> 7) & 1
			cpu.Flags().SetZSCO(r == 0, int8(r) < 0, carry == 1, ((r>>7)&1) != carry)
		case 1:
			a := cpu.Get16(dst)
			b := cpu.Get16(c)
			r := a << b
			cpu.Set16(dst, r)
			carry := ((a << (b - 1)) >> 15) & 1
			cpu.Flags().SetZSCO(r == 0, int16(r) < 0, carry == 1, ((r>>15)&1) != carry)
		}
	//case OpShr:
	case OpSar:
		dst := inst.operands[0]
		c := inst.operands[1]
		switch dst.W() {
		case 0:
			a := cpu.Get8(dst)
			b := cpu.Get8(c)
			r := a >> b
			cpu.Set8(dst, r)
			carry := (a >> (b - 1)) & 1
			cpu.Flags().SetZSCO(r == 0, int8(r) < 0, carry == 1, (b != 1 && OF(cpu.RegisterFile[RegFLAGS]) == 1))
		case 1:
			a := cpu.Get16(dst)
			b := cpu.Get16(c)
			r := a >> b
			cpu.Set16(dst, r)
			carry := (a >> (b - 1)) & 1
			cpu.Flags().SetZSCO(r == 0, int16(r) < 0, carry == 1, (b != 1 && OF(cpu.RegisterFile[RegFLAGS]) == 1))
		}
	//case OpRol:
	//case OpRor:
	//case OpRcl:
	//case OpRcr:
	case OpAndRegRm, OpAndRmImm, OpAndAccImm:
		dst := inst.operands[0]
		src := inst.operands[1]
		switch {
		default:
			panic(ErrOperandWidthMismatch)
		case dst.W() == 0 && src.W() == 0:
			r := cpu.Get8(dst) & cpu.Get8(src)
			cpu.Set8(dst, r)
			cpu.Flags().SetZSCO(r == 0, r < 0, false, false)
		case dst.W() == 1 && src.W() == 1:
			r := cpu.Get16(dst) & cpu.Get16(src)
			cpu.Set16(dst, r)
			cpu.Flags().SetZSCO(r == 0, r < 0, false, false)
		case dst.W() == 1 && src.W() == 0 && isImm(src):
			r := cpu.Get16(dst) & cpu.Get8(src)
			cpu.Set8(dst, r)
			cpu.Flags().SetZSCO(r == 0, r < 0, false, false)
		}
	case OpTestRegRm, OpTestRmImm, OpTestAccImm:
		s1 := inst.operands[0]
		s2 := inst.operands[1]
		switch {
		default:
			panic(ErrOperandWidthMismatch)
		case s1.W() == 0 && s2.W() == 0:
			a := cpu.Get8(s1)
			b := cpu.Get8(s2)
			r := a & b
			cpu.Flags().SetZSCO(r == 0, r < 0, false, false)
		case s1.W() == 1 && s2.W() == 1:
			a := cpu.Get16(s1)
			b := cpu.Get16(s2)
			r := a & b
			cpu.Flags().SetZSCO(r == 0, r < 0, false, false)
		case s1.W() == 1 && s2.W() == 0 && isImm(s2):
			a := cpu.Get16(s1)
			b := cpu.Get8(s2)
			r := a & b
			cpu.Flags().SetZSCO(r == 0, r < 0, false, false)
		}
	case OpOrRegRm, OpOrRmImm, OpOrAccImm:
		dst := inst.operands[0]
		src := inst.operands[1]
		switch {
		default:
			panic(ErrOperandWidthMismatch)
		case dst.W() == 0 && src.W() == 0:
			r := cpu.Get8(dst) | cpu.Get8(src)
			cpu.Set8(dst, r)
			cpu.Flags().SetZSCO(r == 0, r < 0, false, false)
		case dst.W() == 1 && src.W() == 1:
			r := cpu.Get16(dst) | cpu.Get16(src)
			cpu.Set16(dst, r)
			cpu.Flags().SetZSCO(r == 0, r < 0, false, false)
		case dst.W() == 1 && src.W() == 0 && isImm(src):
			r := cpu.Get16(dst) | cpu.Get8(src)
			cpu.Set8(dst, r)
			cpu.Flags().SetZSCO(r == 0, r < 0, false, false)
		}
	case OpXorRegRm, OpXorRmImm, OpXorAccImm:
		dst := inst.operands[0]
		src := inst.operands[1]
		switch {
		default:
			panic(ErrOperandWidthMismatch)
		case dst.W() == 0 && src.W() == 0:
			r := cpu.Get8(dst) ^ cpu.Get8(src)
			cpu.Set8(dst, r)
			cpu.Flags().SetZSCO(r == 0, r < 0, false, false)
		case dst.W() == 1 && src.W() == 1:
			r := cpu.Get16(dst) ^ cpu.Get16(src)
			cpu.Set16(dst, r)
			cpu.Flags().SetZSCO(r == 0, r < 0, false, false)
		case dst.W() == 1 && src.W() == 0 && isImm(src):
			r := cpu.Get16(dst) ^ cpu.Get8(src)
			cpu.Set8(dst, r)
			cpu.Flags().SetZSCO(r == 0, r < 0, false, false)
		}
	//case OpRep:
	//case OpMovsb:
	//case OpCmpsb:
	//case OpScasb:
	//case OpLodsb:
	//case OpStosb:
	//case OpMovsw:
	//case OpCmpsw:
	//case OpScasw:
	//case OpLodsw:
	//case OpStosw:
	case OpCallDirSeg, OpCallIndirSeg:
		cpu.RegisterFile[RegSP] -= 2
		cpu.Data.Write16(cpu.RegisterFile[RegSP], uint16(inst.offset+inst.size))
		arg := inst.operands[0]
		addr := uint16(cpu.Get16(arg))
		cpu.RegisterFile[RegIP] = addr
		return // don't increment IP
		// FLAGS none affected
	//case OpCallDirInterSeg:
	//case OpCallIndirInterSeg:
	case OpJmpDirSeg, OpJmpShortDirSeg, OpJmpIndirSeg:
		arg := inst.operands[0]
		addr := uint16(cpu.Get16(arg))
		cpu.RegisterFile[RegIP] = addr
		return // don't increment IP
		// FLAGS none affected
	//case OpJmpDirInterSeg:
	//case OpJmpIndirInterSeg:
	case OpRetSeg:
		addr := cpu.Data.Read16(cpu.RegisterFile[RegSP])
		cpu.RegisterFile[RegSP] += 2
		cpu.RegisterFile[RegIP] = addr
		return // don't increment IP
		// FLAGS none affected
	case OpRetSegImm:
		imm := inst.operands[0]
		addr := cpu.Data.Read16(cpu.RegisterFile[RegSP])
		cpu.RegisterFile[RegSP] += 2 + uint16(cpu.Get16(imm))
		cpu.RegisterFile[RegIP] = addr
		return // don't increment IP
		// FLAGS none affected
	//case OpRetInterSeg:
	//case OpRetInterSegImm:
	case OpJe:
		if ZF(cpu.RegisterFile[RegFLAGS]) == 1 {
			arg := inst.operands[0]
			addr := uint16(cpu.Get16(arg))
			cpu.RegisterFile[RegIP] = addr
			return // don't increment IP
		}
		// FLAGS none affected
	case OpJl:
		if SF(cpu.RegisterFile[RegFLAGS]) != OF(cpu.RegisterFile[RegFLAGS]) {
			arg := inst.operands[0]
			addr := uint16(cpu.Get16(arg))
			cpu.RegisterFile[RegIP] = addr
			return // don't increment IP
		}
		// FLAGS none affected
	case OpJle:
		flags := cpu.RegisterFile[RegFLAGS]
		if ZF(flags) == 1 || SF(flags) != OF(flags) {
			arg := inst.operands[0]
			addr := uint16(cpu.Get16(arg))
			cpu.RegisterFile[RegIP] = addr
			return // don't increment IP
		}
		// FLAGS none affected
	case OpJb:
		if CF(cpu.RegisterFile[RegFLAGS]) == 1 {
			arg := inst.operands[0]
			addr := uint16(cpu.Get16(arg))
			cpu.RegisterFile[RegIP] = addr
			return // don't increment IP
		}
		// FLAGS none affected
	case OpJbe:
		flags := cpu.RegisterFile[RegFLAGS]
		if CF(flags) == 1 || ZF(flags) == 1 {
			arg := inst.operands[0]
			addr := uint16(cpu.Get16(arg))
			cpu.RegisterFile[RegIP] = addr
			return // don't increment IP
		}
		// FLAGS none affected
	case OpJp:
		panic("jp (jump if PF == 1) not supported")
	case OpJo:
		if OF(cpu.RegisterFile[RegFLAGS]) == 1 {
			arg := inst.operands[0]
			addr := uint16(cpu.Get16(arg))
			cpu.RegisterFile[RegIP] = addr
			return // don't increment IP
		}
		// FLAGS none affected
	case OpJs:
		if SF(cpu.RegisterFile[RegFLAGS]) == 1 {
			arg := inst.operands[0]
			addr := uint16(cpu.Get16(arg))
			cpu.RegisterFile[RegIP] = addr
			return // don't increment IP
		}
		// FLAGS none affected
	case OpJne:
		if ZF(cpu.RegisterFile[RegFLAGS]) == 0 {
			arg := inst.operands[0]
			addr := uint16(cpu.Get16(arg))
			cpu.RegisterFile[RegIP] = addr
			return // don't increment IP
		}
		// FLAGS none affected
	case OpJnl:
		if SF(cpu.RegisterFile[RegFLAGS]) == OF(cpu.RegisterFile[RegFLAGS]) {
			arg := inst.operands[0]
			addr := uint16(cpu.Get16(arg))
			cpu.RegisterFile[RegIP] = addr
			return // don't increment IP
		}
		// FLAGS none affected
	case OpJnle:
		flags := cpu.RegisterFile[RegFLAGS]
		if ZF(flags) == 0 && SF(flags) == OF(flags) {
			arg := inst.operands[0]
			addr := uint16(cpu.Get16(arg))
			cpu.RegisterFile[RegIP] = addr
			return // don't increment IP
		}
		// FLAGS none affected
	case OpJnb:
		if CF(cpu.RegisterFile[RegFLAGS]) == 0 {
			arg := inst.operands[0]
			addr := uint16(cpu.Get16(arg))
			cpu.RegisterFile[RegIP] = addr
			return // don't increment IP
		}
		// FLAGS none affected
	case OpJnbe:
		flags := cpu.RegisterFile[RegFLAGS]
		if CF(flags) == 0 && ZF(flags) == 0 {
			arg := inst.operands[0]
			addr := uint16(cpu.Get16(arg))
			cpu.RegisterFile[RegIP] = addr
			return // don't increment IP
		}
		// FLAGS none affected
	case OpJnp:
		panic("jnp (jump if PF == 0) not supported")
	case OpJno:
		if CF(cpu.RegisterFile[RegFLAGS]) == 0 {
			arg := inst.operands[0]
			addr := uint16(cpu.Get16(arg))
			cpu.RegisterFile[RegIP] = addr
			return // don't increment IP
		}
		// FLAGS none affected
	case OpJns:
		if SF(cpu.RegisterFile[RegFLAGS]) == 0 {
			arg := inst.operands[0]
			addr := uint16(cpu.Get16(arg))
			cpu.RegisterFile[RegIP] = addr
			return // don't increment IP
		}
		// FLAGS none affected
	//case OpLoop:
	//case OpLoopz:
	//case OpLoopnz:
	//case OpJcxz:
	case OpIntType3:
		intVector[3](cpu)
		// FLAGS none affected
	case OpIntTypeSpecified:
		t := inst.operands[0]
		v := cpu.Get8(t)
		intVector[v](cpu)
		// FLAGS none affected
	//case OpInto:
	//case OpIret:
	//case OpClc:
	//case OpCmc:
	//case OpStc:
	//case OpCld:
	//case OpStd:
	//case OpCli:
	//case OpSti:
	//case OpHlt:
	//case OpWait:
	//case OpEsc:
	//case OpLock:
	}
	cpu.RegisterFile[RegIP] += uint16(inst.size)
}

var (
	ErrHaltAndCatchFire = errors.New("halted and on fire")
)

type MinixError int

func (err MinixError) Error() string {
	return "minix: " + map[int]string{
		7:  "argument list too long",
		12: "cannot allocate memory",
		22: "invalid argument",
	}[int(err)]
}

// errno -l
var (
	E2BIG  = MinixError(7)
	ENOMEM = MinixError(12)
	EINVAL = MinixError(22)
)

func (cpu *CPU) execve(argv, envp []string) {
	totalLen := 3 * uint16(2) // argc, two nil pointers
	for _, s := range slices.Concat(envp, argv) {
		totalLen += uint16(len(s)) + 1
		totalLen += 2
	}
	frame := cpu.RegisterFile[RegSP]
	// padding for alignment if necessary
	if (frame-totalLen)%2 != 0 {
		frame -= 1
		cpu.Data.WriteZeros(frame, 1)
	}
	//fmt.Printf("totalLen: %d, frameStart: %04x\n", totalLen, frame - totalLen)
	addrs := make([]uint16, len(argv)+len(envp))
	ss := slices.Concat(argv, envp)
	slices.Reverse(ss)
	for i, s := range ss {
		frame -= uint16(len(s)) + 1
		cpu.Data.WriteString(frame, s)
		addrs[i] = frame
	}
	frame -= 2; cpu.Data.Write16(frame, 0)
	for _, p := range addrs[:len(envp)] {
		frame -= 2; cpu.Data.Write16(frame, p)
	}
	frame -= 2; cpu.Data.Write16(frame, 0)
	for _, p := range addrs[len(envp):] {
		frame -= 2; cpu.Data.Write16(frame, p)
	}
	frame -= 2; cpu.Data.Write16(frame, uint16(len(argv)))
	cpu.RegisterFile[RegSP] = frame
}

func emulate(exec Exec, name string, bin []byte, args []string, debug bool) error {
	cpu := &CPU{}
	cpu.RegisterFile[RegSP] = 0xFFFF // 0xFFFE, 0x0000
	// @note: Minix2:SYS/src/mm/exec.c +50
	cpu.execve(append([]string{name}, args...), []string{"PATH=/usr:/usr/bin"})
	// @todo: flags = 0x20 for separate text/data

	/* Memory Layout
	 *
	 * ╭─────────╮ 0xFFFF
	 * │ nil ptr │
	 * │---------│ 0xFFFE
	 * │ stack   │
	 * │   |     │
	 * │   v     │ <- sp
	 * ¦         ¦
	 * ¦         ¦
	 * ¦         ¦
	 * │   ^     │ <- brksize (pointer to (after the) end of data segment)
	 * │   |     │
	 * │ heap    │
	 * │---------│
	 * │ bss     │ initialized to 0, size of which is read from a.out
	 * │---------│
	 * │ data    │ read from a.out
	 * │---------│
	 * │ text    │ read from a.out
	 * ╰─────────╯
	 */
	text := exec.Text(bin)
	data := exec.Data(bin)
	cpu.Text.WriteBytes(0, text)
	off := uint16(0)
	cpu.Data.WriteBytes(off, data); off += uint16(len(data))
	cpu.Data.WriteZeros(off, uint16(exec.SizeBSS)); off += uint16(exec.SizeBSS)
	// @todo: allocate symbols
	cpu.RegisterFile[RegIP] = uint16(exec.EntryPoint)
	if debug {
		fmt.Println(" AX   BX   CX   DX   SP   BP   SI   DI  FLAGS IP")
	}
	for {
		src := cpu.Fetch()
		inst, err := cpu.Decode(src)
		if err != nil {
			return err
		}
		if debug {
			// @todo: print calculated address and read value for memory accesses
			fmt.Printf("%s:%s\n", cpu, InstructionFormatterWithMemoryAccess{inst, cpu})
		}
		cpu.Step(inst)
	}
	return ErrHaltAndCatchFire
}

func assert(v bool, msg any) {
	if !v {
		pc, f, l, _ := runtime.Caller(1)
		panic(fmt.Sprintf("assertion in %s[%s:%d] failed: %v", runtime.FuncForPC(pc).Name(), f, l, msg))
	}
}

func must[T any](t T, err error) T {
	if err != nil {
		panic(err)
	}
	return t
}

var d = flag.Bool("d", false, "disassemble")
var m = flag.Bool("m", false, "debug")

func init() {
	flag.Usage = func() {
		fmt.Fprintf(flag.CommandLine.Output(), "Usage: %s [-d|-m] a.out\n", os.Args[0])
		flag.PrintDefaults()
	}
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
	//fmt.Printf("%x\n", exec.Data(bin))
	if exec.BadMag() {
		log.Fatal("bad magic")
	}

	if *d {
		text := exec.Text(bin)
		insts, err := disassemble(text)
		if err != nil {
			//log.Fatal(err)
			log.Println(err)
		}
		for _, inst := range insts {
			fmt.Println(InstructionFormatterWithOffset{inst})
		}
	} else {
		err := emulate(exec, file, bin, restArgs[1:], *m)
		if err != nil {
			log.Println(err)
		}
	}
}
