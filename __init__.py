
from binaryninja.architecture import Architecture
from binaryninja.platform import Platform
from binaryninja.callingconvention import CallingConvention

from m6805 import M6805, M6805View

class DefaultCallingConvention(CallingConvention):
    int_arg_regs = ['A']
    int_return_reg = 'A'

M6805.register()
M6805View.register()

class M6805Platform(Platform):
    name = 'm6805'

arch = Architecture['m6805']
arch.register_calling_convention(DefaultCallingConvention(arch, 'default'))

platform = M6805Platform(arch)
platform.default_calling_convention = arch.calling_conventions['default']
platform.register('m6805')
