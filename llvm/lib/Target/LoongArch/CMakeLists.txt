add_llvm_component_group(LoongArch)

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
