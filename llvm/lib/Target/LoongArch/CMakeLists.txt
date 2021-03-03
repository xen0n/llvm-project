add_llvm_component_group(LoongArch)

set(LLVM_TARGET_DEFINITIONS LoongArch.td)

tablegen(LLVM LoongArchGenRegisterInfo.inc -gen-register-info)
tablegen(LLVM LoongArchGenInstrInfo.inc -gen-instr-info)

add_public_tablegen_target(LoongArchCommonTableGen)

add_llvm_target(LoongArchCodeGen
  LoongArchTargetMachine.cpp

  LINK_COMPONENTS
  Core
  CodeGen
  LoongArchInfo
  Support
  Target

  ADD_TO_COMPONENT
  LoongArch
  )

add_subdirectory(TargetInfo)
