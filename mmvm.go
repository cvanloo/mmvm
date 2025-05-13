package main

import (
	"unsafe"
	"fmt"
	"os"
	//"encoding/binary" // @todo: do the parsing properly
)

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
)

func must[T any](t T, err error) T {
	if err != nil {
		panic(err)
	}
	return t
}

func registerName(w byte, b byte) string {
	names := []string{"al", "cl", "dl", "bl", "ah", "ch", "dh", "bh", "ax", "cx", "dx", "bx", "sp", "bp", "si", "di"}
	return names[b+w*8]
}

func memoryName(mod, rm, dispHigh, dispLow byte) string {
	names := []string{"bx+si", "bx+di", "bp+si", "bp+di", "si", "di", "bp", "bx"}
	switch mod {
	case 0b00:
		if rm == 0b110 {
			disp := (int16(dispHigh) << 8) ^ int16(dispLow)
			return names[rm] + fmt.Sprintf("+%04x", disp)
		}
		return names[rm]
	case 0b01:
		return names[rm] + fmt.Sprintf("+%04x", int16(dispLow)) // @fixme: does this cast actually sign extend?
	case 0b10:
		disp := (int16(dispHigh) << 8) ^ int16(dispLow)
		return names[rm] + fmt.Sprintf("+%04x", disp)
	case 0b11:
		panic("rm is reg")
	default:
		panic("invalid mod")
	}
}

func main() {
	bin := must(os.ReadFile("a.out"))
	exec := *(*Exec)(unsafe.Pointer(&bin[0]))
	fmt.Printf("%#v\n", exec)
	text := bin[32:32+exec.sizeText]
	fmt.Printf("%x\n", text)
	for i := int32(0); i < exec.sizeText; {
		fmt.Printf("%04x: ", i)
		i1 := text[i]; i++ // consume
		switch {
		case (i1 & 0b11111100) == 0b10001000: // MOV REG R/M
			i2 := text[i]; i++
			fmt.Printf("%02x", i1)
			fmt.Printf("%02x", i2)
			fmt.Printf("          ")
			d := (i1 & 0b00000010) >> 1
			w := i1 & 0b00000001 // w == 0 -> byte, w == 1 -> word (copy 2 bytes at once)
			mod := i2 & 0b11000000
			reg := (i2 & 0b00111000) >> 3
			rm := i2 & 0b00000111
			if d == 0 { // from reg
				if mod == 0b11000000 { // r/m is reg
					fmt.Printf("mov %s, %s", registerName(w, rm), registerName(w, reg))
				} else { // r/m is mem
					fmt.Printf("mov [%s], %s", memoryName(mod >> 6, rm, 0, 0), registerName(w, reg))
				}
			} else if d == 1 { // to reg
				if mod == 0b11000000 { // r/m is reg
					fmt.Printf("mov %s, %s", registerName(w, reg), registerName(w, rm))
				} else { // r/m is mem
					fmt.Printf("mov %s, [%s]", registerName(w, reg), memoryName(mod >> 6, rm, 0, 0))
				}
			}
		case (i1 & 0b11110000) == 0b10110000: // MOV REG, IMM8/IMM16
			i2 := text[i]; i++
			fmt.Printf("%02x", i1)
			fmt.Printf("%02x", i2)
			w := (i1 & 0b00001000) >> 3
			data := int16(i2)
			if w == 1 {
				i3 := text[i]; i++
				fmt.Printf("%02x", i3)
				fmt.Printf("        ")
				data = (int16(i3) << 8) ^ data
			} else {
				fmt.Printf("          ")
			}
			reg := i1 & 0b00000111
			fmt.Printf("mov %s, %0[3]*[2]x", registerName(w, reg), data, (w+1)*2)
		case (i1 & 0b11111100) == 0: // ADD REG R/M
			i2 := text[i]; i++
			fmt.Printf("%02x", i1)
			fmt.Printf("%02x", i2)
			fmt.Printf("          ")
			d := (i1 & 0b00000010) >> 1
			w := i1 & 0b00000001
			mod := i2 & 0b11000000
			reg := (i2 & 0b00111000) >> 3
			rm := i2 & 0b00000111
			if d == 0 { // from reg
				if mod == 0b10000000 { // r/m is reg
					fmt.Printf("add %s, %s", registerName(w, rm), registerName(w, reg))
				} else { // r/m is mem
					fmt.Printf("add [%s], %s", memoryName(mod >> 6, rm, 0, 0), registerName(w, reg))
				}
			} else if d == 1 { // to reg
				if mod == 0b10000000 { // r/m is reg
					fmt.Printf("add %s, %s", registerName(w, reg), registerName(w, rm))
				} else { // r/m is mem
					fmt.Printf("add %s, [%s]", registerName(w, reg), memoryName(mod >> 6, rm, 0, 0))
				}
			}
		case i1 == 0b11001100: // INT (TYPE 3)
			fmt.Printf("%02x", i1)
			fmt.Printf("            ")
			fmt.Printf("int")
		case i1 == 0b11001101: // INT TYPE
			i2 := text[i]; i++
			fmt.Printf("%02x", i1)
			fmt.Printf("%02x", i2)
			fmt.Printf("          ")
			fmt.Printf("int %02x", i2)
		}
		fmt.Println("")
	}
}
