//===-- loongarch.h - Generic JITLink loongarch edge kinds, utilities -*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Generic utilities for graphs representing loongarch objects.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_EXECUTIONENGINE_JITLINK_LOONGARCH_H
#define LLVM_EXECUTIONENGINE_JITLINK_LOONGARCH_H

#include "llvm/ExecutionEngine/JITLink/JITLink.h"

namespace llvm {
namespace jitlink {
namespace loongarch {

/// Represents loongarch fixups
enum EdgeKind_loongarch : Edge::Kind {

  // TODO: Capture and replace to generic fixups
  /// A plain 32-bit pointer value relocation
  ///
  /// Fixup expression:
  ///   Fixup <= Target + Addend : uint32
  ///
  R_LARCH_32 = Edge::FirstRelocation,

  /// A plain 64-bit pointer value relocation
  ///
  /// Fixup expression:
  ///   Fixup <- Target + Addend : uint32
  ///
  R_LARCH_64,
};

/// Returns a string name for the given loongarch edge. For debugging purposes
/// only
const char *getEdgeKindName(Edge::Kind K);
} // namespace loongarch
} // namespace jitlink
} // namespace llvm

#endif
