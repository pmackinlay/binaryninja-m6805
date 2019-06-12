import struct
import traceback

import binaryninja._binaryninjacore as core
import binaryninja.enum as enum

from binaryninja.architecture import Architecture
from binaryninja.binaryview import BinaryView
from binaryninja.enums import (BranchType, Endianness, InstructionTextTokenType, LowLevelILOperation, LowLevelILFlagCondition, FlagRole, SegmentFlag, SymbolType, SectionSemantics)
from binaryninja.function import RegisterInfo, InstructionInfo, InstructionTextToken
from binaryninja.log import log_error
from binaryninja.lowlevelil import LowLevelILInstruction, LowLevelILLabel, ILRegister, LLIL_TEMP
from binaryninja.platform import Platform
from binaryninja.types import Symbol, Type

class M6805_AddressMode(enum.IntEnum):
    INH =  1 # inherent
    IMM =  2 # immediate
    DIR =  3 # direct
    EXT =  4 # extended
    IX  =  5 # indexed, no offset
    IX1 =  6 # indexed, 1 byte offset
    IX2 =  7 # indexed, 2 byte offset
    BSC =  8 # bit set/clear
    BTB =  9 # bit test and branch
    REL = 10 # relative

class M6805(Architecture):
    name = 'm6805'
    endianness = Endianness.BigEndian
    address_size = 2
    default_int_size = 1
    instr_alignment = 1
    max_instr_length = 3

    regs = {
        'A': RegisterInfo('A', 1),
        'X': RegisterInfo('X', 1),
        'CC': RegisterInfo('CC', 1),
        'SP': RegisterInfo('SP', 1),
        'PC': RegisterInfo('PC', 2),
    }

    stack_pointer = 'SP'

    flags = [
        'H', # half carry
        'I', # interrupt disable
        'N', # negative
        'Z', # zero
        'C', # carry/borrow
        'INT', # interrupt
    ]
    flag_write_types = ['NZ', 'NZC', 'HNZC']
    flag_roles = {
        'H': FlagRole.HalfCarryFlagRole,
        'I': FlagRole.SpecialFlagRole,
        'N': FlagRole.NegativeSignFlagRole,
        'Z': FlagRole.ZeroFlagRole,
        'C': FlagRole.CarryFlagRole,
        'INT': FlagRole.SpecialFlagRole,
    }
    flags_written_by_flag_write_type = {
        'NZ': ['N', 'Z'],
        'NZC': ['N', 'Z', 'C'],
        'HNZC': ['H', 'N', 'Z', 'C'],
    }
    flags_required_for_flag_condition = {
        LowLevelILFlagCondition.LLFC_E:   ['Z'],
        LowLevelILFlagCondition.LLFC_NE:  ['Z'],
        LowLevelILFlagCondition.LLFC_ULT: ['C'],
        LowLevelILFlagCondition.LLFC_ULE: ['C', 'Z'],
        LowLevelILFlagCondition.LLFC_UGE: ['C'],
        LowLevelILFlagCondition.LLFC_UGT: ['C', 'Z'],
        LowLevelILFlagCondition.LLFC_NEG: ['N'],
        LowLevelILFlagCondition.LLFC_POS: ['N'],
    }

    instructions = [
        # 0x00-0x0f: Bit Manipulation BTB
        [('BRSET', 3, M6805_AddressMode.BTB), ['0', 'DIR', 'REL'], lambda self, il, m, t: M6805.cond_branch(il, 3, t, il.test_bit(1, il.load(1, m), il.const(1, 0)))],
        [('BRCLR', 3, M6805_AddressMode.BTB), ['0', 'DIR', 'REL'], lambda self, il, m, t: M6805.cond_branch(il, 3, t, il.not_expr(0, il.test_bit(1, il.load(1, m), il.const(1, 0))))],
        [('BRSET', 3, M6805_AddressMode.BTB), ['1', 'DIR', 'REL'], lambda self, il, m, t: M6805.cond_branch(il, 3, t, il.test_bit(1, il.load(1, m), il.const(1, 1)))],
        [('BRCLR', 3, M6805_AddressMode.BTB), ['1', 'DIR', 'REL'], lambda self, il, m, t: M6805.cond_branch(il, 3, t, il.not_expr(0, il.test_bit(1, il.load(1, m), il.const(1, 1))))],
        [('BRSET', 3, M6805_AddressMode.BTB), ['2', 'DIR', 'REL'], lambda self, il, m, t: M6805.cond_branch(il, 3, t, il.test_bit(1, il.load(1, m), il.const(1, 2)))],
        [('BRCLR', 3, M6805_AddressMode.BTB), ['2', 'DIR', 'REL'], lambda self, il, m, t: M6805.cond_branch(il, 3, t, il.not_expr(0, il.test_bit(1, il.load(1, m), il.const(1, 2))))],
        [('BRSET', 3, M6805_AddressMode.BTB), ['3', 'DIR', 'REL'], lambda self, il, m, t: M6805.cond_branch(il, 3, t, il.test_bit(1, il.load(1, m), il.const(1, 3)))],
        [('BRCLR', 3, M6805_AddressMode.BTB), ['3', 'DIR', 'REL'], lambda self, il, m, t: M6805.cond_branch(il, 3, t, il.not_expr(0, il.test_bit(1, il.load(1, m), il.const(1, 3))))],
        [('BRSET', 3, M6805_AddressMode.BTB), ['4', 'DIR', 'REL'], lambda self, il, m, t: M6805.cond_branch(il, 3, t, il.test_bit(1, il.load(1, m), il.const(1, 4)))],
        [('BRCLR', 3, M6805_AddressMode.BTB), ['4', 'DIR', 'REL'], lambda self, il, m, t: M6805.cond_branch(il, 3, t, il.not_expr(0, il.test_bit(1, il.load(1, m), il.const(1, 4))))],
        [('BRSET', 3, M6805_AddressMode.BTB), ['5', 'DIR', 'REL'], lambda self, il, m, t: M6805.cond_branch(il, 3, t, il.test_bit(1, il.load(1, m), il.const(1, 5)))],
        [('BRCLR', 3, M6805_AddressMode.BTB), ['5', 'DIR', 'REL'], lambda self, il, m, t: M6805.cond_branch(il, 3, t, il.not_expr(0, il.test_bit(1, il.load(1, m), il.const(1, 5))))],
        [('BRSET', 3, M6805_AddressMode.BTB), ['6', 'DIR', 'REL'], lambda self, il, m, t: M6805.cond_branch(il, 3, t, il.test_bit(1, il.load(1, m), il.const(1, 6)))],
        [('BRCLR', 3, M6805_AddressMode.BTB), ['6', 'DIR', 'REL'], lambda self, il, m, t: M6805.cond_branch(il, 3, t, il.not_expr(0, il.test_bit(1, il.load(1, m), il.const(1, 6))))],
        [('BRSET', 3, M6805_AddressMode.BTB), ['7', 'DIR', 'REL'], lambda self, il, m, t: M6805.cond_branch(il, 3, t, il.test_bit(1, il.load(1, m), il.const(1, 7)))],
        [('BRCLR', 3, M6805_AddressMode.BTB), ['7', 'DIR', 'REL'], lambda self, il, m, t: M6805.cond_branch(il, 3, t, il.not_expr(0, il.test_bit(1, il.load(1, m), il.const(1, 7))))],

        # 0x10-0x1f: Bit Manipulation BSC
        [('BSET', 2, M6805_AddressMode.BSC), ['0', 'DIR'], lambda self, il, m: il.store(1, m, il.or_expr(1, il.load(1, m), il.const(1, 0x01)))],
        [('BCLR', 2, M6805_AddressMode.BSC), ['0', 'DIR'], lambda self, il, m: il.store(1, m, il.and_expr(1, il.load(1, m), il.const(1, 0xfe)))],
        [('BSET', 2, M6805_AddressMode.BSC), ['1', 'DIR'], lambda self, il, m: il.store(1, m, il.or_expr(1, il.load(1, m), il.const(1, 0x02)))],
        [('BCLR', 2, M6805_AddressMode.BSC), ['1', 'DIR'], lambda self, il, m: il.store(1, m, il.and_expr(1, il.load(1, m), il.const(1, 0xfd)))],
        [('BSET', 2, M6805_AddressMode.BSC), ['2', 'DIR'], lambda self, il, m: il.store(1, m, il.or_expr(1, il.load(1, m), il.const(1, 0x04)))],
        [('BCLR', 2, M6805_AddressMode.BSC), ['2', 'DIR'], lambda self, il, m: il.store(1, m, il.and_expr(1, il.load(1, m), il.const(1, 0xfb)))],
        [('BSET', 2, M6805_AddressMode.BSC), ['3', 'DIR'], lambda self, il, m: il.store(1, m, il.or_expr(1, il.load(1, m), il.const(1, 0x08)))],
        [('BCLR', 2, M6805_AddressMode.BSC), ['3', 'DIR'], lambda self, il, m: il.store(1, m, il.and_expr(1, il.load(1, m), il.const(1, 0xf7)))],
        [('BSET', 2, M6805_AddressMode.BSC), ['4', 'DIR'], lambda self, il, m: il.store(1, m, il.or_expr(1, il.load(1, m), il.const(1, 0x10)))],
        [('BCLR', 2, M6805_AddressMode.BSC), ['4', 'DIR'], lambda self, il, m: il.store(1, m, il.and_expr(1, il.load(1, m), il.const(1, 0xef)))],
        [('BSET', 2, M6805_AddressMode.BSC), ['5', 'DIR'], lambda self, il, m: il.store(1, m, il.or_expr(1, il.load(1, m), il.const(1, 0x20)))],
        [('BCLR', 2, M6805_AddressMode.BSC), ['5', 'DIR'], lambda self, il, m: il.store(1, m, il.and_expr(1, il.load(1, m), il.const(1, 0xdf)))],
        [('BSET', 2, M6805_AddressMode.BSC), ['6', 'DIR'], lambda self, il, m: il.store(1, m, il.or_expr(1, il.load(1, m), il.const(1, 0x40)))],
        [('BCLR', 2, M6805_AddressMode.BSC), ['6', 'DIR'], lambda self, il, m: il.store(1, m, il.and_expr(1, il.load(1, m), il.const(1, 0xbf)))],
        [('BSET', 2, M6805_AddressMode.BSC), ['7', 'DIR'], lambda self, il, m: il.store(1, m, il.or_expr(1, il.load(1, m), il.const(1, 0x80)))],
        [('BCLR', 2, M6805_AddressMode.BSC), ['7', 'DIR'], lambda self, il, m: il.store(1, m, il.and_expr(1, il.load(1, m), il.const(1, 0x7f)))],

        # 0x20-0x2f: Branch REL
        [('BRA', 2, M6805_AddressMode.REL), ['REL'], lambda self, il, t: M6805.branch(il, t)],
        [('BRN', 2, M6805_AddressMode.REL), ['REL'], lambda self, il, t: M6805.cond_branch(il, 2, t, il.const(0, 0))],
        [('BHI', 2, M6805_AddressMode.REL), ['REL'], lambda self, il, t: M6805.cond_branch(il, 2, t, il.flag_condition(LowLevelILFlagCondition.LLFC_UGT))],
        [('BLS', 2, M6805_AddressMode.REL), ['REL'], lambda self, il, t: M6805.cond_branch(il, 2, t, il.flag_condition(LowLevelILFlagCondition.LLFC_ULE))],
        [('BCC', 2, M6805_AddressMode.REL), ['REL'], lambda self, il, t: M6805.cond_branch(il, 2, t, il.flag_condition(LowLevelILFlagCondition.LLFC_UGE))],
        [('BCS', 2, M6805_AddressMode.REL), ['REL'], lambda self, il, t: M6805.cond_branch(il, 2, t, il.flag_condition(LowLevelILFlagCondition.LLFC_ULT))],
        [('BNE', 2, M6805_AddressMode.REL), ['REL'], lambda self, il, t: M6805.cond_branch(il, 2, t, il.flag_condition(LowLevelILFlagCondition.LLFC_NE))],
        [('BEQ', 2, M6805_AddressMode.REL), ['REL'], lambda self, il, t: M6805.cond_branch(il, 2, t, il.flag_condition(LowLevelILFlagCondition.LLFC_E))],
        [('BHCC', 2, M6805_AddressMode.REL), ['REL'], lambda self, il, t: M6805.cond_branch(il, 2, t, il.not_expr(0, il.flag('H')))],
        [('BHCS', 2, M6805_AddressMode.REL), ['REL'], lambda self, il, t: M6805.cond_branch(il, 2, t, il.flag('H'))],
        [('BPL', 2, M6805_AddressMode.REL), ['REL'], lambda self, il, t: M6805.cond_branch(il, 2, t, il.flag_condition(LowLevelILFlagCondition.LLFC_POS))],
        [('BMI', 2, M6805_AddressMode.REL), ['REL'], lambda self, il, t: M6805.cond_branch(il, 2, t, il.flag_condition(LowLevelILFlagCondition.LLFC_NEG))],
        [('BMC', 2, M6805_AddressMode.REL), ['REL'], lambda self, il, t: M6805.cond_branch(il, 2, t, il.not_expr(0, il.flag('I')))],
        [('BMS', 2, M6805_AddressMode.REL), ['REL'], lambda self, il, t: M6805.cond_branch(il, 2, t, il.flag('I'))],
        [('BIL', 2, M6805_AddressMode.REL), ['REL'], lambda self, il, t: M6805.cond_branch(il, 2, t, il.not_expr(0, il.flag('INT')))],
        [('BIH', 2, M6805_AddressMode.REL), ['REL'], lambda self, il, t: M6805.cond_branch(il, 2, t, il.flag('INT'))],

        # 0x30-0x3f: Read-Modify-Write DIR
        [('NEG', 2, M6805_AddressMode.DIR), ['DIR'], lambda self, il, m: il.store(1, m, il.neg_expr(1, il.load(1, m), 'NZC'))],
        None,
        None,
        [('COM', 2, M6805_AddressMode.DIR), ['DIR'], lambda self, il, m: il.store(1, m, il.not_expr(1, il.load(1, m), 'NZC'))],
        [('LSR', 2, M6805_AddressMode.DIR), ['DIR'], lambda self, il, m: il.store(1, m, il.logical_shift_right(1, il.load(1, m), il.const(1, 1), 'NZC'))],
        None,
        [('ROR', 2, M6805_AddressMode.DIR), ['DIR'], lambda self, il, m: il.store(1, m, il.rotate_right_carry(1, il.load(1, m), il.const(1, 1), il.flag('C'), 'NZC'))],
        [('ASR', 2, M6805_AddressMode.DIR), ['DIR'], lambda self, il, m: il.store(1, m, il.arith_shift_right(1, il.load(1, m), il.const(1, 1), 'NZC'))],
        [('LSL', 2, M6805_AddressMode.DIR), ['DIR'], lambda self, il, m: il.store(1, m, il.shift_left(1, il.load(1, m), il.const(1, 1), 'NZC'))],
        [('ROL', 2, M6805_AddressMode.DIR), ['DIR'], lambda self, il, m: il.store(1, m, il.rotate_left_carry(1, il.load(1, m), il.const(1, 1), il.flag('C'), 'NZC'))],
        [('DEC', 2, M6805_AddressMode.DIR), ['DIR'], lambda self, il, m: il.store(1, m, il.sub(1, il.load(1, m), il.const(1, 1), 'NZ'))],
        None,
        [('INC', 2, M6805_AddressMode.DIR), ['DIR'], lambda self, il, m: il.store(1, m, il.add(1, il.load(1, m), il.const(1, 1), 'NZ'))],
        [('TST', 2, M6805_AddressMode.DIR), ['DIR'], lambda self, il, m: il.sub(1, il.load(1, m), il.const(1, 0), 'NZ')],
        None,
        [('CLR', 2, M6805_AddressMode.DIR), ['DIR'], lambda self, il, m: il.store(1, m, il.const(1, 0), 'NZ')],

        # 0x40-0x4f: Read-Modify-Write INH
        [('NEGA', 1, M6805_AddressMode.INH), [], lambda self, il: il.set_reg(1, 'A', il.neg_expr(1, il.reg(1, 'A'), 'NZC'))],
        None,
        None,
        [('COMA', 1, M6805_AddressMode.INH), [], lambda self, il: il.set_reg(1, 'A', il.not_expr(1, il.reg(1, 'A'), 'NZC'))],
        [('LSRA', 1, M6805_AddressMode.INH), [], lambda self, il: il.set_reg(1, 'A', il.logical_shift_right(1, il.reg(1, 'A'), il.const(1, 1), 'NZC'))],
        None,
        [('RORA', 1, M6805_AddressMode.INH), [], lambda self, il: il.set_reg(1, 'A', il.rotate_right_carry(1, il.reg(1, 'A'), il.const(1, 1), il.flag('C'), 'NZC'))],
        [('ASRA', 1, M6805_AddressMode.INH), [], lambda self, il: il.set_reg(1, 'A', il.arith_shift_right(1, il.reg(1, 'A'), il.const(1, 1), 'NZC'))],
        [('LSLA', 1, M6805_AddressMode.INH), [], lambda self, il: il.set_reg(1, 'A', il.shift_left(1, il.reg(1, 'A'), il.const(1, 1), 'NZC'))],
        [('ROLA', 1, M6805_AddressMode.INH), [], lambda self, il: il.set_reg(1, 'A', il.rotate_left_carry(1, il.reg(1, 'A'), il.const(1, 1), il.flag('C'), 'NZC'))],
        [('DECA', 1, M6805_AddressMode.INH), [], lambda self, il: il.set_reg(1, 'A', il.sub(1, il.reg(1, 'A'), il.const(1, 1)), 'NZ')],
        None,
        [('INCA', 1, M6805_AddressMode.INH), [], lambda self, il: il.set_reg(1, 'A', il.add(1, il.reg(1, 'A'), il.const(1, 1)), 'NZ')],
        [('TSTA', 1, M6805_AddressMode.INH), [], lambda self, il: il.sub(1, il.reg(1, 'A'), il.const(1, 0), 'NZ')],
        None,
        [('CLRA', 1, M6805_AddressMode.INH), [], lambda self, il: il.set_reg(1, 'A', il.const(1, 0), 'NZ')],

        # 0x50-0x5f: Read-Modify-Write INH
        [('NEGX', 1, M6805_AddressMode.INH), [], lambda self, il: il.set_reg(1, 'X', il.neg_expr(1, il.reg(1, 'X'), 'NZC'))],
        None,
        None,
        [('COMX', 1, M6805_AddressMode.INH), [], lambda self, il: il.set_reg(1, 'X', il.not_expr(1, il.reg(1, 'X'), 'NZC'))],
        [('LSRX', 1, M6805_AddressMode.INH), [], lambda self, il: il.set_reg(1, 'X', il.logical_shift_right(1, il.reg(1, 'X'), il.const(1, 1), 'NZC'))],
        None,
        [('RORX', 1, M6805_AddressMode.INH), [], lambda self, il: il.set_reg(1, 'X', il.rotate_right_carry(1, il.reg(1, 'X'), il.const(1, 1), il.flag('C'), 'NZC'))],
        [('ASRX', 1, M6805_AddressMode.INH), [], lambda self, il: il.set_reg(1, 'X', il.arith_shift_right(1, il.reg(1, 'X'), il.const(1, 1), 'NZC'))],
        [('LSLX', 1, M6805_AddressMode.INH), [], lambda self, il: il.set_reg(1, 'X', il.shift_left(1, il.reg(1, 'X'), il.const(1, 1), 'NZC'))],
        [('ROLX', 1, M6805_AddressMode.INH), [], lambda self, il: il.set_reg(1, 'X', il.rotate_left_carry(1, il.reg(1, 'X'), il.const(1, 1), il.flag('C'), 'NZC'))],
        [('DECX', 1, M6805_AddressMode.INH), [], lambda self, il: il.set_reg(1, 'X', il.sub(1, il.reg(1, 'X'), il.const(1, 1)), 'NZ')],
        None,
        [('INCX', 1, M6805_AddressMode.INH), [], lambda self, il: il.set_reg(1, 'X', il.add(1, il.reg(1, 'X'), il.const(1, 1)), 'NZ')],
        [('TSTX', 1, M6805_AddressMode.INH), [], lambda self, il: il.sub(1, il.reg(1, 'X'), il.const(1, 0), 'NZ')],
        None,
        [('CLRX', 1, M6805_AddressMode.INH), [], lambda self, il: il.set_reg(1, 'X', il.const(1, 0), 'NZ')],

        # 0x60-0x6f: Read-Modify-Write IX1
        [('NEG', 2, M6805_AddressMode.IX1), ['IX1','X'], lambda self, il, ix1: il.store(1, ix1, il.neg_expr(1, il.load(1, ix1)))],
        None,
        None,
        [('COM', 2, M6805_AddressMode.IX1), ['IX1','X'], lambda self, il, ix1: il.store(1, ix1, il.not_expr(1, il.load(1, ix1)))],
        [('LSR', 2, M6805_AddressMode.IX1), ['IX1','X'], lambda self, il, ix1: il.store(1, ix1, il.logical_shift_right(1, il.load(1, ix1), il.const(1, 1), 'NZC'))],
        None,
        [('ROR', 2, M6805_AddressMode.IX1), ['IX1','X'], lambda self, il, ix1: il.store(1, ix1, il.rotate_right_carry(1, il.load(1, ix1), il.const(1, 1), il.flag('C'), 'NZC'))],
        [('ASR', 2, M6805_AddressMode.IX1), ['IX1','X'], lambda self, il, ix1: il.store(1, ix1, il.arith_shift_right(1, il.load(1, ix1), il.const(1, 1), 'NZC'))],
        [('LSL', 2, M6805_AddressMode.IX1), ['IX1','X'], lambda self, il, ix1: il.store(1, ix1, il.shift_left(1, il.load(1, ix1), il.const(1, 1), 'NZC'))],
        [('ROL', 2, M6805_AddressMode.IX1), ['IX1','X'], lambda self, il, ix1: il.store(1, ix1, il.rotate_left_carry(1, il.load(1, ix1), il.const(1, 1), il.flag('C'), 'NZC'))],
        [('DEC', 2, M6805_AddressMode.IX1), ['IX1','X'], lambda self, il, ix1: il.store(1, ix1, il.sub(1, il.load(1, ix1), il.const(1, 1)), 'NZ')],
        None,
        [('INC', 2, M6805_AddressMode.IX1), ['IX1','X'], lambda self, il, ix1: il.store(1, ix1, il.add(1, il.load(1, ix1), il.const(1, 1)), 'NZ')],
        [('TST', 2, M6805_AddressMode.IX1), ['IX1','X'], lambda self, il, ix1: il.sub(1, il.load(1, ix1), il.const(1, 0), 'NZ')],
        None,
        [('CLR', 2, M6805_AddressMode.IX1), ['IX1','X'], lambda self, il, ix1: il.store(1, ix1, il.const(1, 0), 'NZ')],

        # 0x70-0x7f: Read-Modify-Write IX
        [('NEG', 1, M6805_AddressMode.IX), ['','X'], lambda self, il, x: il.store(1, x, il.neg_expr(1, il.load(1, x)))],
        None,
        None,
        [('COM', 1, M6805_AddressMode.IX), ['','X'], lambda self, il, x: il.store(1, x, il.not_expr(1, il.load(1, x)))],
        [('LSR', 1, M6805_AddressMode.IX), ['','X'], lambda self, il, x: il.store(1, x, il.logical_shift_right(1, il.load(1, x), il.const(1, 1), 'NZC'))],
        None,
        [('ROR', 1, M6805_AddressMode.IX), ['','X'], lambda self, il, x: il.store(1, x, il.rotate_right_carry(1, il.load(1, x), il.const(1, 1), il.flag('C'), 'NZC'))],
        [('ASR', 1, M6805_AddressMode.IX), ['','X'], lambda self, il, x: il.store(1, x, il.arith_shift_right(1, il.load(1, x), il.const(1, 1), 'NZC'))],
        [('LSL', 1, M6805_AddressMode.IX), ['','X'], lambda self, il, x: il.store(1, x, il.shift_left(1, il.load(1, x), il.const(1, 1), 'NZC'))],
        [('ROL', 1, M6805_AddressMode.IX), ['','X'], lambda self, il, x: il.store(1, x, il.rotate_left_carry(1, il.load(1, x), il.const(1, 1), il.flag('C'), 'NZC'))],
        [('DEC', 1, M6805_AddressMode.IX), ['','X'], lambda self, il, x: il.store(1, x, il.sub(1, il.load(1, x), il.const(1, 1)), 'NZ')],
        None,
        [('INC', 1, M6805_AddressMode.IX), ['','X'], lambda self, il, x: il.store(1, x, il.add(1, il.load(1, x), il.const(1, 1)), 'NZ')],
        [('TST', 1, M6805_AddressMode.IX), ['','X'], lambda self, il, x: il.sub(1, il.load(1, x), il.const(1, 0), 'NZ')],
        None,
        [('CLR', 1, M6805_AddressMode.IX), ['','X'], lambda self, il, x: il.store(1, x, il.const(1, 0), 'NZ')],

        # 0x80-0x8f: Control INH
        [('RTI', 1, M6805_AddressMode.INH), [], lambda self, il: il.nop()],
        [('RTS', 1, M6805_AddressMode.INH), [], lambda self, il: il.ret(il.pop(2))],
        None,
        [('SWI', 1, M6805_AddressMode.INH), [], lambda self, il: il.nop()],
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None,

        # 0x90-0x9f: Control INH
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        [('TAX', 1, M6805_AddressMode.INH), [], lambda self, il: il.set_reg(1, 'X', il.reg(1, 'A'))],
        [('CLC', 1, M6805_AddressMode.INH), [], lambda self, il: il.set_flag('C', il.const(0, 0))],
        [('SEC', 1, M6805_AddressMode.INH), [], lambda self, il: il.set_flag('C', il.const(0, 1))],
        [('CLI', 1, M6805_AddressMode.INH), [], lambda self, il: il.set_flag('I', il.const(0, 0))],
        [('SEI', 1, M6805_AddressMode.INH), [], lambda self, il: il.set_flag('I', il.const(0, 1))],
        [('RSP', 1, M6805_AddressMode.INH), [], lambda self, il: il.set_reg(1, 'SP', il.const(1, 0x7f))],
        [('NOP', 1, M6805_AddressMode.INH), [], lambda self, il: il.nop()],
        None,
        [('TXA', 1, M6805_AddressMode.INH), [], lambda self, il: il.set_reg(1, 'A', il.reg(1, 'X'))],

        # 0xa0-0xaf: Register/Memory IMM
        [('SUB', 2, M6805_AddressMode.IMM), ['IMM'], lambda self, il, imm: il.set_reg(1, 'A', il.sub(1, il.reg(1, 'A'), imm), 'NZC')],
        [('CMP', 2, M6805_AddressMode.IMM), ['IMM'], lambda self, il, imm: il.sub(1, il.reg(1, 'A'), imm, 'NZC')],
        [('SBC', 2, M6805_AddressMode.IMM), ['IMM'], lambda self, il, imm: il.set_reg(1, 'A', il.sub_borrow(1, il.reg(1, 'A'), imm, il.flag('C')), 'NZC')],
        [('CPX', 2, M6805_AddressMode.IMM), ['IMM'], lambda self, il, imm: il.sub(1, il.reg(1, 'X'), imm, 'NZC')],
        [('AND', 2, M6805_AddressMode.IMM), ['IMM'], lambda self, il, imm: il.set_reg(1, 'A', il.and_expr(1, il.reg(1, 'A'), imm), 'NZ')],
        [('BIT', 2, M6805_AddressMode.IMM), ['IMM'], lambda self, il, imm: il.and_expr(1, il.reg(1, 'A'), imm, 'NZ')],
        [('LDA', 2, M6805_AddressMode.IMM), ['IMM'], lambda self, il, imm: il.set_reg(1, 'A', imm)],
        None,
        [('EOR', 2, M6805_AddressMode.IMM), ['IMM'], lambda self, il, imm: il.set_reg(1, 'A', il.xor_expr(1, il.reg(1, 'A'), imm), 'NZ')],
        [('ADC', 2, M6805_AddressMode.IMM), ['IMM'], lambda self, il, imm: il.set_reg(1, 'A', il.add_carry(1, il.reg(1, 'A'), imm, il.flag('C')), 'HNZC')],
        [('ORA', 2, M6805_AddressMode.IMM), ['IMM'], lambda self, il, imm: il.set_reg(1, 'A', il.or_expr(1, il.reg(1, 'A'), imm), 'NZ')],
        [('ADD', 2, M6805_AddressMode.IMM), ['IMM'], lambda self, il, imm: il.set_reg(1, 'A', il.add(1, il.reg(1, 'A'), imm), 'HNZC')],
        None,
        [('BSR', 2, M6805_AddressMode.REL), ['REL'], lambda self, il, t: il.call(t)],
        [('LDX', 2, M6805_AddressMode.IMM), ['IMM'], lambda self, il, imm: il.set_reg(1, 'X', imm)],
        None,

        # 0xb0-0xbf: Register/Memory DIR
        [('SUB', 2, M6805_AddressMode.DIR), ['DIR'], lambda self, il, m: il.set_reg(1, 'A', il.sub(1, il.reg(1, 'A'), il.load(1, m), 'NZC'))],
        [('CMP', 2, M6805_AddressMode.DIR), ['DIR'], lambda self, il, m: il.sub(1, il.reg(1, 'A'), il.load(1, m), 'NZC')],
        [('SBC', 2, M6805_AddressMode.DIR), ['DIR'], lambda self, il, m: il.set_reg(1, 'A', il.sub_borrow(1, il.reg(1, 'A'), il.load(1, m), il.flag('C'), 'NZC'))],
        [('CPX', 2, M6805_AddressMode.DIR), ['DIR'], lambda self, il, m: il.sub(1, il.reg(1, 'X'), il.load(1, m), 'NZC')],
        [('AND', 2, M6805_AddressMode.DIR), ['DIR'], lambda self, il, m: il.set_reg(1, 'A', il.and_expr(1, il.reg(1, 'A'), il.load(1, m), 'NZ'))],
        [('BIT', 2, M6805_AddressMode.DIR), ['DIR'], lambda self, il, m: il.and_expr(1, il.reg(1, 'A'), il.load(1, m), 'NZ')],
        [('LDA', 2, M6805_AddressMode.DIR), ['DIR'], lambda self, il, m: il.set_reg(1, 'A', il.load(1, m))],
        [('STA', 2, M6805_AddressMode.DIR), ['DIR'], lambda self, il, m: il.store(1, m, il.reg(1, 'A'))],
        [('EOR', 2, M6805_AddressMode.DIR), ['DIR'], lambda self, il, m: il.set_reg(1, 'A', il.xor_expr(1, il.reg(1, 'A'), il.load(1, m), 'NZ'))],
        [('ADC', 2, M6805_AddressMode.DIR), ['DIR'], lambda self, il, m: il.set_reg(1, 'A', il.add_carry(1, il.reg(1, 'A'), il.load(1, m), il.flag('C'), 'NZC'))],
        [('ORA', 2, M6805_AddressMode.DIR), ['DIR'], lambda self, il, m: il.set_reg(1, 'A', il.or_expr(1, il.reg(1, 'A'), il.load(1, m), 'NZ'))],
        [('ADD', 2, M6805_AddressMode.DIR), ['DIR'], lambda self, il, m: il.set_reg(1, 'A', il.add(1, il.reg(1, 'A'), il.load(1, m), 'NZC'))],
        [('JMP', 2, M6805_AddressMode.DIR), ['DIR'], lambda self, il, m: M6805.branch(il, m)],
        [('JSR', 2, M6805_AddressMode.DIR), ['DIR'], lambda self, il, m: il.call(m)],
        [('LDX', 2, M6805_AddressMode.DIR), ['DIR'], lambda self, il, m: il.set_reg(1, 'X', il.load(1, m))],
        [('STX', 2, M6805_AddressMode.DIR), ['DIR'], lambda self, il, m: il.store(1, m, il.reg(1, 'X'))],

        # 0xc0-0xcf: Register/Memory EXT
        [('SUB', 3, M6805_AddressMode.EXT), ['EXT'], lambda self, il, ext: il.set_reg(1, 'A', il.sub(1, il.reg(1, 'A'), il.load(1, ext), 'NZC'))],
        [('CMP', 3, M6805_AddressMode.EXT), ['EXT'], lambda self, il, ext: il.sub(1, il.reg(1, 'A'), il.load(1, ext), 'NZC')],
        [('SBC', 3, M6805_AddressMode.EXT), ['EXT'], lambda self, il, ext: il.set_reg(1, 'A', il.sub_borrow(1, il.reg(1, 'A'), il.load(1, ext), il.flag('C'), 'NZC'))],
        [('CPX', 3, M6805_AddressMode.EXT), ['EXT'], lambda self, il, ext: il.sub(1, il.reg(1, 'X'), il.load(1, ext), 'NZC')],
        [('AND', 3, M6805_AddressMode.EXT), ['EXT'], lambda self, il, ext: il.set_reg(1, 'A', il.and_expr(1, il.reg(1, 'A'), il.load(1, ext), 'NZ'))],
        [('BIT', 3, M6805_AddressMode.EXT), ['EXT'], lambda self, il, ext: il.and_expr(1, il.reg(1, 'A'), il.load(1, ext), 'NZ')],
        [('LDA', 3, M6805_AddressMode.EXT), ['EXT'], lambda self, il, ext: il.set_reg(1, 'A', il.load(1, ext))],
        [('STA', 3, M6805_AddressMode.EXT), ['EXT'], lambda self, il, ext: il.store(1, ext, il.reg(1, 'A'))],
        [('EOR', 3, M6805_AddressMode.EXT), ['EXT'], lambda self, il, ext: il.set_reg(1, 'A', il.xor_expr(1, il.reg(1, 'A'), il.load(1, ext), 'NZ'))],
        [('ADC', 3, M6805_AddressMode.EXT), ['EXT'], lambda self, il, ext: il.set_reg(1, 'A', il.add_carry(1, il.reg(1, 'A'), il.load(1, ext), il.flag('C'), 'NZC'))],
        [('ORA', 3, M6805_AddressMode.EXT), ['EXT'], lambda self, il, ext: il.set_reg(1, 'A', il.or_expr(1, il.reg(1, 'A'), il.load(1, ext), 'NZ'))],
        [('ADD', 3, M6805_AddressMode.EXT), ['EXT'], lambda self, il, ext: il.set_reg(1, 'A', il.add(1, il.reg(1, 'A'), il.load(1, ext), 'NZC'))],
        [('JMP', 3, M6805_AddressMode.EXT), ['EXT'], lambda self, il, ext: M6805.branch(il, ext)],
        [('JSR', 3, M6805_AddressMode.EXT), ['EXT'], lambda self, il, ext: il.call(ext)],
        [('LDX', 3, M6805_AddressMode.EXT), ['EXT'], lambda self, il, ext: il.set_reg(1, 'X', il.load(1, ext))],
        [('STX', 3, M6805_AddressMode.EXT), ['EXT'], lambda self, il, ext: il.store(1, ext, il.reg(1, 'X'))],

        # 0xd0-0xdf: Register/Memory IX2
        [('SUB', 3, M6805_AddressMode.IX2), ['IX2', 'X'], lambda self, il, ix2: il.set_reg(1, 'A', il.sub(1, il.reg(1, 'A'), il.load(1, ix2), 'NZC'))],
        [('CMP', 3, M6805_AddressMode.IX2), ['IX2', 'X'], lambda self, il, ix2: il.sub(1, il.reg(1, 'A'), il.load(1, ix2), 'NZC')],
        [('SBC', 3, M6805_AddressMode.IX2), ['IX2', 'X'], lambda self, il, ix2: il.set_reg(1, 'A', il.sub_borrow(1, il.reg(1, 'A'), il.load(1, ix2), il.flag('C'), 'NZC'))],
        [('CPX', 3, M6805_AddressMode.IX2), ['IX2', 'X'], lambda self, il, ix2: il.sub(1, il.reg(1, 'X'), il.load(1, ix2), 'NZC')],
        [('AND', 3, M6805_AddressMode.IX2), ['IX2', 'X'], lambda self, il, ix2: il.set_reg(1, 'A', il.and_expr(1, il.reg(1, 'A'), il.load(1, ix2), 'NZ'))],
        [('BIT', 3, M6805_AddressMode.IX2), ['IX2', 'X'], lambda self, il, ix2: il.and_expr(1, il.reg(1, 'A'), il.load(1, ix2), 'NZ')],
        [('LDA', 3, M6805_AddressMode.IX2), ['IX2', 'X'], lambda self, il, ix2: il.set_reg(1, 'A', il.load(1, ix2))],
        [('STA', 3, M6805_AddressMode.IX2), ['IX2', 'X'], lambda self, il, ix2: il.store(1, ix2, il.reg(1, 'A'))],
        [('EOR', 3, M6805_AddressMode.IX2), ['IX2', 'X'], lambda self, il, ix2: il.set_reg(1, 'A', il.xor_expr(1, il.reg(1, 'A'), il.load(1, ix2), 'NZ'))],
        [('ADC', 3, M6805_AddressMode.IX2), ['IX2', 'X'], lambda self, il, ix2: il.set_reg(1, 'A', il.add_carry(1, il.reg(1, 'A'), il.load(1, ix2), il.flag('C'), 'NZC'))],
        [('ORA', 3, M6805_AddressMode.IX2), ['IX2', 'X'], lambda self, il, ix2: il.set_reg(1, 'A', il.or_expr(1, il.reg(1, 'A'), il.load(1, ix2), 'NZ'))],
        [('ADD', 3, M6805_AddressMode.IX2), ['IX2', 'X'], lambda self, il, ix2: il.set_reg(1, 'A', il.add(1, il.reg(1, 'A'), il.load(1, ix2), 'NZC'))],
        [('JMP', 3, M6805_AddressMode.IX2), ['IX2', 'X'], lambda self, il, ix2: M6805.branch(il, ix2)],
        [('JSR', 3, M6805_AddressMode.IX2), ['IX2' ,'X'], lambda self, il, ix2: il.call(ix2)],
        [('LDX', 3, M6805_AddressMode.IX2), ['IX2' ,'X'], lambda self, il, ix2: il.set_reg(1, 'X', il.load(1, ix2))],
        [('STX', 3, M6805_AddressMode.IX2), ['IX2' ,'X'], lambda self, il, ix2: il.store(1, ix2, il.reg(1, 'X'))],

        # 0xe0-0xef: Register/Memory IX1
        [('SUB', 2, M6805_AddressMode.IX1), ['IX1', 'X'], lambda self, il, ix1: il.set_reg(1, 'A', il.sub(1, il.reg(1, 'A'), il.load(1, ix1), 'NZC'))],
        [('CMP', 2, M6805_AddressMode.IX1), ['IX1', 'X'], lambda self, il, ix1: il.sub(1, il.reg(1, 'A'), il.load(1, ix1), 'NZC')],
        [('SBC', 2, M6805_AddressMode.IX1), ['IX1', 'X'], lambda self, il, ix1: il.set_reg(1, 'A', il.sub_borrow(1, il.reg(1, 'A'), il.load(1, ix1), il.flag('C'), 'NZC'))],
        [('CPX', 2, M6805_AddressMode.IX1), ['IX1', 'X'], lambda self, il, ix1: il.sub(1, il.reg(1, 'X'), il.load(1, ix1), 'NZC')],
        [('AND', 2, M6805_AddressMode.IX1), ['IX1', 'X'], lambda self, il, ix1: il.set_reg(1, 'A', il.and_expr(1, il.reg(1, 'A'), il.load(1, ix1), 'NZ'))],
        [('BIT', 2, M6805_AddressMode.IX1), ['IX1', 'X'], lambda self, il, ix1: il.and_expr(1, il.reg(1, 'A'), il.load(1, ix1), 'NZ')],
        [('LDA', 2, M6805_AddressMode.IX1), ['IX1', 'X'], lambda self, il, ix1: il.set_reg(1, 'A', il.load(1, ix1))],
        [('STA', 2, M6805_AddressMode.IX1), ['IX1', 'X'], lambda self, il, ix1: il.store(1, ix1, il.reg(1, 'A'))],
        [('EOR', 2, M6805_AddressMode.IX1), ['IX1', 'X'], lambda self, il, ix1: il.set_reg(1, 'A', il.xor_expr(1, il.reg(1, 'A'), il.load(1, ix1), 'NZ'))],
        [('ADC', 2, M6805_AddressMode.IX1), ['IX1', 'X'], lambda self, il, ix1: il.set_reg(1, 'A', il.add_carry(1, il.reg(1, 'A'), il.load(1, ix1), il.flag('C'), 'NZC'))],
        [('ORA', 2, M6805_AddressMode.IX1), ['IX1', 'X'], lambda self, il, ix1: il.set_reg(1, 'A', il.or_expr(1, il.reg(1, 'A'), il.load(1, ix1), 'NZ'))],
        [('ADD', 2, M6805_AddressMode.IX1), ['IX1', 'X'], lambda self, il, ix1: il.set_reg(1, 'A', il.add(1, il.reg(1, 'A'), il.load(1, ix1), 'NZC'))],
        [('JMP', 2, M6805_AddressMode.IX1), ['IX1', 'X'], lambda self, il, ix1: M6805.branch(il, ix1)],
        [('JSR', 2, M6805_AddressMode.IX1), ['IX1', 'X'], lambda self, il, ix1: il.call(ix1)],
        [('LDX', 2, M6805_AddressMode.IX1), ['IX1', 'X'], lambda self, il, ix1: il.set_reg(1, 'X', il.load(1, ix1))],
        [('STX', 2, M6805_AddressMode.IX1), ['IX1', 'X'], lambda self, il, ix1: il.store(1, ix1, il.reg(1, 'X'))],

        # 0xf0-0xff: Register/Memory IX
        [('SUB', 1, M6805_AddressMode.IX), ['', 'X'], lambda self, il, x: il.set_reg(1, 'A', il.sub(1, il.reg(1, 'A'), il.load(1, x), 'NZC'))],
        [('CMP', 1, M6805_AddressMode.IX), ['', 'X'], lambda self, il, x: il.sub(1, il.reg(1, 'A'), il.load(1, x))],
        [('SBC', 1, M6805_AddressMode.IX), ['', 'X'], lambda self, il, x: il.set_reg(1, 'A', il.sub_borrow(1, il.reg(1, 'A'), il.load(1, x), il.flag('C'), 'NZC'))],
        [('CPX', 1, M6805_AddressMode.IX), ['', 'X'], lambda self, il, x: il.sub(1, il.reg(1, 'X'), il.load(1, x))],
        [('AND', 1, M6805_AddressMode.IX), ['', 'X'], lambda self, il, x: il.set_reg(1, 'A', il.and_expr(1, il.reg(1, 'A'), il.load(1, x), 'NZ'))],
        [('BIT', 1, M6805_AddressMode.IX), ['', 'X'], lambda self, il, x: il.and_expr(1, il.reg(1, 'A'), il.load(1, x), 'NZ')],
        [('LDA', 1, M6805_AddressMode.IX), ['', 'X'], lambda self, il, x: il.set_reg(1, 'A', il.load(1, x))],
        [('STA', 1, M6805_AddressMode.IX), ['', 'X'], lambda self, il, x: il.store(1, x, il.reg(1, 'A'))],
        [('EOR', 1, M6805_AddressMode.IX), ['', 'X'], lambda self, il, x: il.set_reg(1, 'A', il.xor_expr(1, il.reg(1, 'A'), il.load(1, x), 'NZ'))],
        [('ADC', 1, M6805_AddressMode.IX), ['', 'X'], lambda self, il, x: il.set_reg(1, 'A', il.add_carry(1, il.reg(1, 'A'), il.load(1, x), il.flag('C'), 'NZC'))],
        [('ORA', 1, M6805_AddressMode.IX), ['', 'X'], lambda self, il, x: il.set_reg(1, 'A', il.or_expr(1, il.reg(1, 'A'), il.load(1, x), 'NZ'))],
        [('ADD', 1, M6805_AddressMode.IX), ['', 'X'], lambda self, il, x: il.set_reg(1, 'A', il.add(1, il.reg(1, 'A'), il.load(1, x), 'NZC'))],
        [('JMP', 1, M6805_AddressMode.IX), ['', 'X'], lambda self, il, x: M6805.branch(il, x)],
        [('JSR', 1, M6805_AddressMode.IX), ['', 'X'], lambda self, il, x: il.call(x)],
        [('LDX', 1, M6805_AddressMode.IX), ['', 'X'], lambda self, il, x: il.set_reg(1, 'X', il.load(1, x))],
        [('STX', 1, M6805_AddressMode.IX), ['', 'X'], lambda self, il, x: il.store(1, x, x)],
    ]

    @staticmethod
    def branch(il, target):
        # try to find a label for the branch target
        if isinstance(target, LowLevelILInstruction) and target.operation in [LowLevelILOperation.LLIL_CONST, LowLevelILOperation.LLIL_CONST_PTR]:
            target_label = il.get_label_for_address(il.arch, target.operand[0].value)
            if target_label is not None:
                il.append(il.goto(target_label))
                return

        il.append(il.jump(target))

    @staticmethod
    def cond_branch(il, length, target, cond):
        # try to find a label for the branch target
        if isinstance(target, LowLevelILInstruction) and target.operation in [LowLevelILOperation.LLIL_CONST, LowLevelILOperation.LLIL_CONST_PTR]:
            taken_label = il.get_label_for_address(il.arch, target.operand[0].value)
        else:
            taken_label = None

        # create taken target
        taken_found = True
        if taken_label is None:
            taken_label = LowLevelILLabel()
            taken_found = False

        # create untaken target
        untaken_found = True
        untaken_label = il.get_label_for_address(il.arch, il.current_address + length)
        if untaken_label is None:
            untaken_label = LowLevelILLabel()
            untaken_found = False

        # generate the conditional branch LLIL
        il.append(il.if_expr(cond, taken_label, untaken_label))

        # generate a jump to the branch target if a label couldn't be found
        if not taken_found:
            il.mark_label(taken_label)
            il.append(il.jump(target))

        # generate a label for the untaken branch
        if not untaken_found:
            il.mark_label(untaken_label)

    def get_instruction_info(self, data, addr):
        # instruction lookup
        instruction = self.instructions[struct.unpack('>B', data[0])[0]]
        if instruction is None:
            return None

        (opcode, length, mode) = instruction[0]

        result = InstructionInfo()
        result.length = length

        # add branches
        if opcode in ['RTS', 'RTI']:
            result.add_branch(BranchType.FunctionReturn)
        elif opcode in ['JMP']:
            if mode == M6805_AddressMode.DIR:
                result.add_branch(BranchType.UnconditionalBranch, struct.unpack('>B', data[1])[0])
            elif mode == M6805_AddressMode.EXT:
                result.add_branch(BranchType.UnconditionalBranch, struct.unpack('>H', data[1:3])[0])
            else:
                result.add_branch(BranchType.UnresolvedBranch)
        elif opcode in ['BRA']:
            result.add_branch(BranchType.UnconditionalBranch, addr + length + struct.unpack('>b', data[1])[0])
        elif opcode.startswith('BRSET') or opcode.startswith('BRCLR'):
            result.add_branch(BranchType.TrueBranch, addr + length + struct.unpack('>b', data[2])[0])
            result.add_branch(BranchType.FalseBranch, addr + length)
        elif opcode in ['BRN', 'BHI', 'BLS', 'BCC', 'BCS', 'BNE', 'BEQ', 'BHCC', 'BHCS', 'BPL', 'BMI', 'BMC', 'BMS', 'BIL', 'BIH']:
            result.add_branch(BranchType.TrueBranch, addr + length + struct.unpack('>b', data[1])[0])
            result.add_branch(BranchType.FalseBranch, addr + length)
        elif opcode in ['BSR', 'JSR']:
            if mode == M6805_AddressMode.DIR:
                result.add_branch(BranchType.CallDestination, struct.unpack('>B', data[1])[0])
            elif mode == M6805_AddressMode.EXT:
                result.add_branch(BranchType.CallDestination, struct.unpack('>H', data[1:3])[0])
            else:
                result.add_branch(BranchType.UnresolvedBranch)

        return result

    def get_instruction_text(self, data, addr):
        # instruction lookup
        instruction = self.instructions[struct.unpack('>B', data[0])[0]]
        if instruction is None:
            return None

        (opcode, length, mode) = instruction[0]

        # opcode
        tokens = [InstructionTextToken(InstructionTextTokenType.InstructionToken, '{:6}'.format(opcode))]

        # operands
        offset = 1
        for operand in instruction[1]:
            # add a separator if needed
            if len(tokens) > 1:
                tokens += [InstructionTextToken(InstructionTextTokenType.OperandSeparatorToken, ',')]
            
            if operand == 'IMM':
                immediate = struct.unpack('>B', data[offset])[0]
                tokens += [InstructionTextToken(InstructionTextTokenType.IntegerToken, '#${:X}'.format(immediate), immediate)]
                offset += 1
            elif operand == 'DIR':
                address = struct.unpack('>B', data[offset])[0]
                tokens += [InstructionTextToken(InstructionTextTokenType.PossibleAddressToken, '${:X}'.format(address), address)]
                offset += 1
            elif operand == 'EXT':
                address = struct.unpack('>H', data[offset:offset + 2])[0]
                tokens += [InstructionTextToken(InstructionTextTokenType.PossibleAddressToken, '${:X}'.format(address), address)]
                offset += 2
            elif operand == 'REL':
                address = addr + length + struct.unpack('>b', data[offset])[0]
                tokens += [InstructionTextToken(InstructionTextTokenType.PossibleAddressToken, '${:X}'.format(address), address)]
                offset += 1
            elif operand == 'IX1':
                index = struct.unpack('>B', data[offset])[0]
                tokens += [InstructionTextToken(InstructionTextTokenType.IntegerToken, '${:X}'.format(index), index)]
                offset += 1
            elif operand == 'IX2':
                index = struct.unpack('>H', data[offset:offset + 2])[0]
                tokens += [InstructionTextToken(InstructionTextTokenType.IntegerToken, '${:X}'.format(index), index)]
                offset += 2
            else:
                tokens += [InstructionTextToken(InstructionTextTokenType.TextToken, operand)]
                
        return tokens, length

    def get_instruction_low_level_il(self, data, addr, il):
        # instruction lookup
        instruction = self.instructions[struct.unpack('>B', data[0])[0]]
        if instruction is None:
            return None

        (opcode, length, mode) = instruction[0]

        if mode == M6805_AddressMode.INH:
            il_instr = instruction[2](self, il)
        elif mode == M6805_AddressMode.IMM:
            il_instr = instruction[2](self, il, il.const(1, struct.unpack('>B', data[1])[0]))
        elif mode == M6805_AddressMode.DIR:
            il_instr = instruction[2](self, il, il.const_pointer(1, struct.unpack('>B', data[1])[0]))
        elif mode == M6805_AddressMode.EXT:
            il_instr = instruction[2](self, il, il.const_pointer(2, struct.unpack('>H', data[1:3])[0]))
        elif mode == M6805_AddressMode.IX:
            il_instr = instruction[2](self, il, il.reg(1, 'X'))
        elif mode == M6805_AddressMode.IX1:
            il_instr = instruction[2](self, il, il.add(2, il.reg(1, 'X'), il.const(1, struct.unpack('>B', data[1])[0])))
        elif mode == M6805_AddressMode.IX2:
            il_instr = instruction[2](self, il, il.add(2, il.reg(1, 'X'), il.const(2, struct.unpack('>H', data[1:3])[0])))
        elif mode == M6805_AddressMode.BSC:
            il_instr = instruction[2](self, il, il.const_pointer(1, struct.unpack('>B', data[1])[0]))
        elif mode == M6805_AddressMode.BTB:
            il_instr = instruction[2](self, il, il.const_pointer(1, struct.unpack('>B', data[1])[0]), il.const_pointer(2, addr + length + struct.unpack('>b', data[2])[0]))
        elif mode == M6805_AddressMode.REL:
            il_instr = instruction[2](self, il, il.const_pointer(2, addr + length + struct.unpack('>b', data[1])[0]))

        if isinstance(il_instr, list):
            for i in [i for i in il_instr if i is not None]:
                il.append(i)
        elif il_instr is not None:
            il.append(il_instr)

        return length

class M6805View(BinaryView):
    name = 'M6805'

    def __init__(self, data):
        BinaryView.__init__(self, parent_view=data, file_metadata=data.file)

    @classmethod
    def is_valid_for_data(cls, data):
        return True

    def init(self):
        self.platform = Platform['m6805']

        length = len(self.parent_view)

        try:
            # first 64 bytes are memory mapped registers/ports
            self.add_auto_segment(0, 64, 0, 0, SegmentFlag.SegmentContainsData | SegmentFlag.SegmentReadable | SegmentFlag.SegmentWritable)
            self.add_auto_section('.ports', 0, 64, SectionSemantics.ReadWriteDataSectionSemantics)

            # next 64 bytes is RAM
            self.add_auto_segment(64, 64, 0, 0, SegmentFlag.SegmentContainsData | SegmentFlag.SegmentReadable | SegmentFlag.SegmentWritable)
            self.add_auto_section('.ram', 64, 64, SectionSemantics.ReadWriteDataSectionSemantics)

            # remaining bytes are ROM
            self.add_auto_segment(128, length - 128, 128, length - 128, SegmentFlag.SegmentContainsCode | SegmentFlag.SegmentReadable)
            self.add_auto_section('.rom', 128, length - 128, SectionSemantics.ReadOnlyCodeSectionSemantics)

            vectors = struct.unpack('>4H', self.parent_view.read(length - 8, 8))
            self.add_entry_point(vectors[3])

            # entry points
            self.define_auto_symbol_and_var_or_function(Symbol(SymbolType.FunctionSymbol, vectors[0], '_timer'), None, self.platform)
            self.define_auto_symbol_and_var_or_function(Symbol(SymbolType.FunctionSymbol, vectors[1], '_interrupt'), None, self.platform)
            self.define_auto_symbol_and_var_or_function(Symbol(SymbolType.FunctionSymbol, vectors[2], '_swi'), None, self.platform)
            self.define_auto_symbol_and_var_or_function(Symbol(SymbolType.FunctionSymbol, vectors[3], '_reset'), None, self.platform)

            # ports
            self.define_auto_symbol_and_var_or_function(Symbol(SymbolType.DataSymbol, 0, 'PORTA'), Type.int(1, False), self.platform)
            self.define_auto_symbol_and_var_or_function(Symbol(SymbolType.DataSymbol, 1, 'PORTB'), Type.int(1, False), self.platform)
            self.define_auto_symbol_and_var_or_function(Symbol(SymbolType.DataSymbol, 2, 'PORTC'), Type.int(1, False), self.platform)
            self.define_auto_symbol_and_var_or_function(Symbol(SymbolType.DataSymbol, 3, 'PORTD'), Type.int(1, False), self.platform)
            self.define_auto_symbol_and_var_or_function(Symbol(SymbolType.DataSymbol, 4, 'DDRA'), Type.int(1, False), self.platform)
            self.define_auto_symbol_and_var_or_function(Symbol(SymbolType.DataSymbol, 5, 'DDRB'), Type.int(1, False), self.platform)
            self.define_auto_symbol_and_var_or_function(Symbol(SymbolType.DataSymbol, 6, 'DDRC'), Type.int(1, False), self.platform)
            self.define_auto_symbol_and_var_or_function(Symbol(SymbolType.DataSymbol, 8, 'TDR'), Type.int(1, False), self.platform)
            self.define_auto_symbol_and_var_or_function(Symbol(SymbolType.DataSymbol, 9, 'TCR'), Type.int(1, False), self.platform)
            self.define_auto_symbol_and_var_or_function(Symbol(SymbolType.DataSymbol, 10, 'MISC'), Type.int(1, False), self.platform)
            self.define_auto_symbol_and_var_or_function(Symbol(SymbolType.DataSymbol, 11, 'PCR'), Type.int(1, False), self.platform)
            self.define_auto_symbol_and_var_or_function(Symbol(SymbolType.DataSymbol, 14, 'ACR'), Type.int(1, False), self.platform)
            self.define_auto_symbol_and_var_or_function(Symbol(SymbolType.DataSymbol, 15, 'ARR'), Type.int(1, False), self.platform)

            return True

        except:
            log_error(traceback.format_exc())
            return False

    def perform_get_address_size(self):
        return 2

    def perform_is_executable(self):
        return True

    # FIXME: this isn't working for rol/ror (code ref a68)?
    def get_flag_write_low_level_il(self, op, size, write_type, flag, operands, il):
        if flag == 'C' and op == LowLevelILOperation.LLIL_RLC:
            return il.test_bit(0, operands[0], il.const(1, 7))
        elif flag == 'C' and op == LowLevelILOperation.LLIL_RRC:
            return il.test_bit(0, operands[0], il.const(1, 0))
 
        return self.perform_get_flag_write_low_level_il(op, size, write_type, flag, operands, il)
