### 8086 16-bit Disassembler

**Tested on DOSBox 0.74 using TASM v3.1 and TLINK v3.0**

**How to compile using TASM:**

tasm diss
tlink diss

**Usage example:**
diss destination.asm source.com (source.exe) 

**Other functions:**
- diss /? - help menu

**Notes:**
- Works best with `.com` files.
- Can be used with `.exe` files; however, the program does not skip the header, so expect garbage at the start of the file.

**Commands that have not been tested:**
- Jump (internal `imm16`)
- Call (internal `imm16`)
- Jump (external `imm16`)

---

### Todo:

- [x] Add `h` at the end of every number (hex).
- [ ] Move segment change to the same line as the command. 
- [ ] Move `rep` to the same line as the command. 
- [x] Fix most of the internal jump names. 
- [x] Fix the order of the procedures. 
- [ ] Remove unnecessary code. 
- [ ] Improve `.exe` file handling. 
- [ ] Optimize lookups (make the tree higher). 

