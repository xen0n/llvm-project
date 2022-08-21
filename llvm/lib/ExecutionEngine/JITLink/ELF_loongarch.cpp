//===--- ELF_loongarch.cpp -JIT linker implementation for ELF/loongarch ---===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// ELF/loongarch jit-link implementation.
//
//===----------------------------------------------------------------------===//

#include "llvm/ExecutionEngine/JITLink/ELF_loongarch.h"
#include "ELFLinkGraphBuilder.h"
#include "JITLinkGeneric.h"
#include "PerGraphGOTAndPLTStubsBuilder.h"
#include "llvm/BinaryFormat/ELF.h"
#include "llvm/ExecutionEngine/JITLink/JITLink.h"
#include "llvm/ExecutionEngine/JITLink/loongarch.h"
#include "llvm/Object/ELF.h"
#include "llvm/Object/ELFObjectFile.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/Endian.h"

#define DEBUG_TYPE "jitlink"
using namespace llvm;
using namespace llvm::jitlink;
using namespace llvm::jitlink::loongarch;

namespace {

class PerGraphGOTAndPLTStubsBuilder_ELF_loongarch
    : public PerGraphGOTAndPLTStubsBuilder<
          PerGraphGOTAndPLTStubsBuilder_ELF_loongarch> {
public:
  static constexpr size_t StubEntrySize = 16;
  static const uint8_t NullGOTEntryContent[8];
  static const uint8_t LA64StubContent[StubEntrySize];
  static const uint8_t LA32StubContent[StubEntrySize];

  using PerGraphGOTAndPLTStubsBuilder<
      PerGraphGOTAndPLTStubsBuilder_ELF_loongarch>::PerGraphGOTAndPLTStubsBuilder;

  bool isLA64() const { return G.getPointerSize() == 8; }

  bool isGOTEdgeToFix(Edge &E) const { return false; /*E.getKind() == R_LARCH_GOT_HI20;*/ }

  Symbol &createGOTEntry(Symbol &Target) {
    Block &GOTBlock =
        G.createContentBlock(getGOTSection(), getGOTEntryBlockContent(),
                             orc::ExecutorAddr(), G.getPointerSize(), 0);
    GOTBlock.addEdge(isLA64() ? R_LARCH_64 : R_LARCH_32, 0, Target, 0);
    return G.addAnonymousSymbol(GOTBlock, 0, G.getPointerSize(), false, false);
  }

  Symbol &createPLTStub(Symbol &Target) {
    LLVM_DEBUG(dbgs() << "XXXXXX LoongArchJITLink createPLTStub:" << Target);
    Block &StubContentBlock = G.createContentBlock(
        getStubsSection(), getStubBlockContent(), orc::ExecutorAddr(), 4, 0);
    // auto &GOTEntrySymbol = getGOTEntry(Target);
    // StubContentBlock.addEdge(R_LARCH_CALL, 0, GOTEntrySymbol, 0);
    return G.addAnonymousSymbol(StubContentBlock, 0, StubEntrySize, true,
                                false);
  }

  void fixGOTEdge(Edge &E, Symbol &GOTEntry) {
    // E.setKind(R_LARCH_PCREL_HI20);
    E.setTarget(GOTEntry);
  }

  void fixPLTEdge(Edge &E, Symbol &PLTStubs) {
    // assert(E.getKind() == R_LARCH_CALL_PLT && "Not a R_RISCV_CALL_PLT edge?");
    // E.setKind(R_LARCH_CALL);
    E.setTarget(PLTStubs);
  }

  bool isExternalBranchEdge(Edge &E) const {
    // return E.getKind() == R_LARCH_CALL_PLT;
    return false;
  }

private:
  Section &getGOTSection() const {
    if (!GOTSection)
      GOTSection = &G.createSection("$__GOT", MemProt::Read);
    return *GOTSection;
  }

  Section &getStubsSection() const {
    if (!StubsSection)
      StubsSection =
          &G.createSection("$__STUBS", MemProt::Read | MemProt::Exec);
    return *StubsSection;
  }

  ArrayRef<char> getGOTEntryBlockContent() {
    return {reinterpret_cast<const char *>(NullGOTEntryContent),
            G.getPointerSize()};
  }

  ArrayRef<char> getStubBlockContent() {
    const uint8_t *StubContent = isLA64() ? LA64StubContent : LA32StubContent;
    return {reinterpret_cast<const char *>(StubContent), StubEntrySize};
  }

  mutable Section *GOTSection = nullptr;
  mutable Section *StubsSection = nullptr;
};

const uint8_t PerGraphGOTAndPLTStubsBuilder_ELF_loongarch::NullGOTEntryContent[8] =
    {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};

const uint8_t
    PerGraphGOTAndPLTStubsBuilder_ELF_loongarch::LA64StubContent[StubEntrySize] = {
        0x0f, 0x00, 0x00, 0x1c,  // pcaddu12i $t3, literal
        0xef, 0x01, 0xc0, 0x28,  // ld.d      $t3, $t3, literal
        0xe0, 0x01, 0x00, 0x4c,  // jr        $t3
        0x00, 0x00, 0x40, 0x03}; // nop

const uint8_t
    PerGraphGOTAndPLTStubsBuilder_ELF_loongarch::LA32StubContent[StubEntrySize] = {
        0x0f, 0x00, 0x00, 0x1c,  // pcaddu12i $t3, literal
        0xef, 0x01, 0x80, 0x28,  // ld.w      $t3, $t3, literal
        0xe0, 0x01, 0x00, 0x4c,  // jr        $t3
        0x00, 0x00, 0x40, 0x03}; // nop
} // end anonymous namespace

namespace llvm {
namespace jitlink {

class ELFJITLinker_loongarch : public JITLinker<ELFJITLinker_loongarch> {
  friend class JITLinker<ELFJITLinker_loongarch>;

public:
  ELFJITLinker_loongarch(std::unique_ptr<JITLinkContext> Ctx,
                     std::unique_ptr<LinkGraph> G, PassConfiguration PassConfig)
      : JITLinker(std::move(Ctx), std::move(G), std::move(PassConfig)) {}

private:
  Error applyFixup(LinkGraph &G, Block &B, const Edge &E) const {
    using namespace loongarch;
    using namespace llvm::support;

    char *BlockWorkingMem = B.getAlreadyMutableContent().data();
    char *FixupPtr = BlockWorkingMem + E.getOffset();
    // orc::ExecutorAddr FixupAddress = B.getAddress() + E.getOffset();
    switch (E.getKind()) {
    case R_LARCH_32: {
      int64_t Value = (E.getTarget().getAddress() + E.getAddend()).getValue();
      *(little32_t *)FixupPtr = static_cast<uint32_t>(Value);
      break;
    }
    case R_LARCH_64: {
      int64_t Value = (E.getTarget().getAddress() + E.getAddend()).getValue();
      *(little64_t *)FixupPtr = static_cast<uint64_t>(Value);
      break;
    }
    }
    return Error::success();
  }
};

template <typename ELFT>
class ELFLinkGraphBuilder_loongarch : public ELFLinkGraphBuilder<ELFT> {
private:
  static Expected<loongarch::EdgeKind_loongarch>
  getRelocationKind(const uint32_t Type) {
    using namespace loongarch;
    switch (Type) {
    case ELF::R_LARCH_32:
      return EdgeKind_loongarch::R_LARCH_32;
    case ELF::R_LARCH_64:
      return EdgeKind_loongarch::R_LARCH_64;
    }

    return make_error<JITLinkError>(
        "Unsupported LoongArch relocation:" + formatv("{0:d}: ", Type) +
        object::getELFRelocationTypeName(ELF::EM_LOONGARCH, Type));
  }

  Error addRelocations() override {
    LLVM_DEBUG(dbgs() << "Processing relocations:\n");

    using Base = ELFLinkGraphBuilder<ELFT>;
    using Self = ELFLinkGraphBuilder_loongarch<ELFT>;
    for (const auto &RelSect : Base::Sections)
      if (Error Err = Base::forEachRelocation(RelSect, this,
                                              &Self::addSingleRelocation))
        return Err;

    return Error::success();
  }

  Error addSingleRelocation(const typename ELFT::Rela &Rel,
                            const typename ELFT::Shdr &FixupSect,
                            Block &BlockToFix) {
    using Base = ELFLinkGraphBuilder<ELFT>;

    uint32_t Type = Rel.getType(false);
    // We do not implement linker relaxation for now.
    if (Type == llvm::ELF::R_LARCH_RELAX)
      return Error::success();

    int64_t Addend = Rel.r_addend;

    Expected<loongarch::EdgeKind_loongarch> Kind = getRelocationKind(Type);
    if (!Kind)
      return Kind.takeError();

    uint32_t SymbolIndex = Rel.getSymbol(false);
    auto ObjSymbol = Base::Obj.getRelocationSymbol(Rel, Base::SymTabSec);
    if (!ObjSymbol)
      return ObjSymbol.takeError();

    Symbol *GraphSymbol = Base::getGraphSymbol(SymbolIndex);
    if (!GraphSymbol)
      return make_error<StringError>(
          formatv("Could not find symbol at given index, did you add it to "
                  "JITSymbolTable? index: {0}, shndx: {1} Size of table: {2}",
                  SymbolIndex, (*ObjSymbol)->st_shndx,
                  Base::GraphSymbols.size()),
          inconvertibleErrorCode());

    auto FixupAddress = orc::ExecutorAddr(FixupSect.sh_addr) + Rel.r_offset;
    Edge::OffsetT Offset = FixupAddress - BlockToFix.getAddress();
    Edge GE(*Kind, Offset, *GraphSymbol, Addend);
    LLVM_DEBUG({
      dbgs() << "    ";
      printEdge(dbgs(), BlockToFix, GE, loongarch::getEdgeKindName(*Kind));
      dbgs() << "\n";
    });

    BlockToFix.addEdge(std::move(GE));
    return Error::success();
  }

public:
  ELFLinkGraphBuilder_loongarch(StringRef FileName,
                            const object::ELFFile<ELFT> &Obj, const Triple T)
      : ELFLinkGraphBuilder<ELFT>(Obj, std::move(T), FileName,
                                  loongarch::getEdgeKindName) {}
};

Expected<std::unique_ptr<LinkGraph>>
createLinkGraphFromELFObject_loongarch(MemoryBufferRef ObjectBuffer) {
  LLVM_DEBUG({
    dbgs() << "Building jitlink graph for new input "
           << ObjectBuffer.getBufferIdentifier() << "...\n";
  });

  auto ELFObj = object::ObjectFile::createELFObjectFile(ObjectBuffer);
  if (!ELFObj)
    return ELFObj.takeError();

  if ((*ELFObj)->getArch() == Triple::loongarch64) {
    auto &ELFObjFile = cast<object::ELFObjectFile<object::ELF64LE>>(**ELFObj);
    return ELFLinkGraphBuilder_loongarch<object::ELF64LE>(
               (*ELFObj)->getFileName(), ELFObjFile.getELFFile(),
               (*ELFObj)->makeTriple())
        .buildGraph();
  }

  assert((*ELFObj)->getArch() == Triple::loongarch32 &&
          "Invalid triple for LoongArch ELF object file");
  auto &ELFObjFile = cast<object::ELFObjectFile<object::ELF32LE>>(**ELFObj);
  return ELFLinkGraphBuilder_loongarch<object::ELF32LE>(
              (*ELFObj)->getFileName(), ELFObjFile.getELFFile(),
              (*ELFObj)->makeTriple())
      .buildGraph();
}

void link_ELF_loongarch(std::unique_ptr<LinkGraph> G,
                        std::unique_ptr<JITLinkContext> Ctx) {
  PassConfiguration Config;
  const Triple &TT = G->getTargetTriple();
  if (Ctx->shouldAddDefaultTargetPasses(TT)) {
    if (auto MarkLive = Ctx->getMarkLivePass(TT))
      Config.PrePrunePasses.push_back(std::move(MarkLive));
    else
      Config.PrePrunePasses.push_back(markAllSymbolsLive);
    Config.PostPrunePasses.push_back(
        PerGraphGOTAndPLTStubsBuilder_ELF_loongarch::asPass);
  }
  if (auto Err = Ctx->modifyPassConfig(*G, Config))
    return Ctx->notifyFailed(std::move(Err));

  ELFJITLinker_loongarch::link(std::move(Ctx), std::move(G), std::move(Config));
}

} // namespace jitlink
} // namespace llvm
